# Threshold / cutoff detection
# Objective 2: find the values within each feature where grade performance shifts.
#
# Two independent approaches, so the cutoffs can be cross-checked:
#   1. a decision tree, which picks its own split points
#   2. binned averages, which show where the mean grade steps down
#
# reaction_time_ms is deliberately left out of the tree. It is a consequence of
# gaming, not a cause of grades, so letting it split would steal credit from
# gaming_hours and make the cutoffs harder to interpret.

library(tidyverse)
library(rpart)
library(rpart.plot)

df <- read_csv("data/cleaned_data.csv", show_col_types = FALSE)

dir.create("output", showWarnings = FALSE)

# ---- Letter grade bins ----
# Standard scale, so a threshold can be stated as "drops from A- to B+".
grade_breaks <- c(-Inf, 60, 63, 67, 70, 73, 77, 80, 83, 87, 90, 93, Inf)
grade_labels <- c("F", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")

to_letter <- function(x) {
  cut(x, breaks = grade_breaks, labels = grade_labels, right = FALSE)
}

df <- df %>% mutate(letter_grade = to_letter(grades))

cat("\n=== Letter grade distribution ===\n")
print(df %>% count(letter_grade) %>% mutate(pct = round(100 * n / sum(n), 1)))

write_csv(df, "data/cleaned_data_with_grades.csv")

# ---- Approach 1: decision tree ----
predictors <- c("gaming_hours", "study_hours", "sleep_hours", "attendance",
                "addiction_score", "device_usage", "social_activity", "age",
                "stress_level", "gaming_genre")

tree_data <- df %>%
  select(grades, all_of(predictors)) %>%
  mutate(across(where(is.character), as.factor))

set.seed(432)
tree <- rpart(grades ~ ., data = tree_data,
              method = "anova",
              control = rpart.control(cp = 0.005, maxdepth = 4, minbucket = 100))

png("output/decision_tree.png", width = 1600, height = 1000, res = 150)
rpart.plot(tree, type = 4, extra = 101, box.palette = "RdYlGn",
           main = "Where grades split: regression tree on student features")
dev.off()

# Pull the primary split of each internal node out of the fitted tree.
# rpart stores splits per node in the order: primary, competitors, surrogates.
frame <- tree$frame
splits <- as.data.frame(tree$splits)
internal <- which(frame$var != "<leaf>")

split_rows <- integer(0)
idx <- 1
for (i in internal) {
  split_rows <- c(split_rows, idx)
  idx <- idx + 1 + frame$ncompete[i] + frame$nsurrogate[i]
}

node_ids <- as.numeric(rownames(frame))[internal]

tree_splits <- tibble(
  depth        = floor(log2(node_ids)) + 1,
  # rpart makes rownames unique by appending .1, .2 ... strip that back off
  variable     = sub("\\.[0-9]+$", "", rownames(splits)[split_rows]),
  split_value  = round(splits$index[split_rows], 2),
  n_at_node    = frame$n[internal],
  mean_grade   = round(frame$yval[internal], 1)
) %>% arrange(depth, desc(n_at_node))

cat("\n=== Tree split points (these are the candidate cutoffs) ===\n")
print(as.data.frame(tree_splits))
write_csv(tree_splits, "output/tree_splits.csv")

# ---- Approach 2: binned averages ----
# Mean grade per bin with a 95% CI, so a visible step down can be checked
# against sampling noise before it gets called a threshold.
bin_specs <- list(
  gaming_hours    = seq(0, 8, by = 1),
  study_hours     = seq(1, 10, by = 1),
  sleep_hours     = seq(4, 9, by = 1),
  attendance      = seq(60, 100, by = 5),
  addiction_score = seq(-5, 25, by = 5)
)

binned_summary <- function(var, breaks) {
  df %>%
    mutate(bin = cut(.data[[var]], breaks = breaks, include.lowest = TRUE)) %>%
    filter(!is.na(bin)) %>%
    group_by(bin) %>%
    summarise(
      n          = n(),
      mean_grade = mean(grades),
      se         = sd(grades) / sqrt(n()),
      .groups    = "drop"
    ) %>%
    mutate(
      ci_low  = mean_grade - 1.96 * se,
      ci_high = mean_grade + 1.96 * se,
      letter  = to_letter(mean_grade),
      variable = var,
      # keep the within-variable ordering as a number. Stacking these tables
      # would otherwise merge the factor levels across variables and scramble
      # which bin follows which.
      bin_index = as.integer(bin),
      bin = as.character(bin)
    )
}

all_bins <- imap_dfr(bin_specs, ~ binned_summary(.y, .x))

for (v in names(bin_specs)) {
  d <- all_bins %>%
    filter(variable == v) %>%
    mutate(bin = fct_reorder(bin, bin_index))

  p <- ggplot(d, aes(x = bin, y = mean_grade)) +
    geom_col(fill = "steelblue", alpha = 0.85) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    geom_text(aes(label = letter), vjust = -0.8, size = 3.5) +
    labs(
      title = paste("Mean grade by", v),
      subtitle = "Error bars are 95% CIs; label is the letter grade of the bin mean",
      x = v, y = "Mean grade"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0("output/binned_", v, ".png"), plot = p, width = 8, height = 5, dpi = 150)
}

write_csv(all_bins, "output/binned_means.csv")

# ---- Largest step down in each feature ----
# The steepest drop between neighbouring bins is the plain-language cutoff.
threshold_summary <- all_bins %>%
  group_by(variable) %>%
  arrange(bin_index, .by_group = TRUE) %>%
  mutate(
    drop        = lag(mean_grade) - mean_grade,
    from_bin    = lag(bin),
    from_letter = lag(letter)
  ) %>%
  filter(!is.na(drop)) %>%
  slice_max(drop, n = 1) %>%
  ungroup() %>%
  transmute(
    variable,
    boundary   = paste(from_bin, "->", bin),
    grade_drop = round(drop, 1),
    shift      = paste(from_letter, "->", letter),
    n_after    = n,
    # a non-positive value means grades never actually step down for this
    # feature, so there is no threshold to report
    threshold  = if_else(drop > 0, "yes", "none found")
  ) %>%
  arrange(desc(grade_drop))

cat("\n=== Steepest grade drop per feature ===\n")
print(as.data.frame(threshold_summary))
write_csv(threshold_summary, "output/threshold_summary.csv")

# ---- Letter grade mix across gaming hours ----
# Shows the whole distribution shifting, not just the mean moving.
p <- df %>%
  mutate(bin = cut(gaming_hours, breaks = bin_specs$gaming_hours, include.lowest = TRUE)) %>%
  filter(!is.na(bin)) %>%
  count(bin, letter_grade) %>%
  group_by(bin) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = bin, y = prop, fill = letter_grade)) +
  geom_col() +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Letter grade mix by daily gaming hours",
       x = "Gaming hours per day", y = "Share of students", fill = "Grade") +
  theme_minimal()

ggsave("output/letter_grade_by_gaming.png", plot = p, width = 9, height = 5, dpi = 150)

cat("\nDone. Wrote plots and CSVs to output/\n")
