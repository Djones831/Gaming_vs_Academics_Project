library(tidyverse)
library(corrplot)

df <- read_csv("data/cleaned_data.csv")

# ---- Correlation matrix ----
numeric_vars <- df %>% select(where(is.numeric)) %>% select(-student_id)
corr_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.7)

# ---- Scatterplots against grades ----
plot_vars <- c("gaming_hours", "study_hours", "sleep_hours", "attendance", "addiction_score")

for (v in plot_vars) {
  p <- ggplot(df, aes_string(x = v, y = "grades")) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", color = "blue") +
    labs(title = paste("Grades vs", v))
  print(p)
  ggsave(paste0("output/scatter_", v, ".png"), plot = p)
}

# ---- Boxplots by categorical variables ----
ggplot(df, aes(x = stress_level, y = grades)) + 
  geom_boxplot() + 
  labs(title = "Grades by Stress Level")

ggplot(df, aes(x = gaming_genre, y = grades)) + 
  geom_boxplot() + 
  labs(title = "Grades by Gaming Genre")