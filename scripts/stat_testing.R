# Goal: Find which of our factors have a statistically significant relationship with grades.
#
# The factors our group wants to compare with grades are: gaming hours, study hours,
# sleep hours, attendance, and addiction score.
#
# We are using 95% confidence to determine statistical significance (alpha = 0.05).
# The null hypothesis is that a factor has no relationship with grades.
# The alternative is that it does.
#
# NOTE ON METHOD:
# The first version of this script used single factor ANOVA with factor(gaming_hours),
# factor(attendance), and so on. That turned out to be the wrong tool here, and it gave
# us two wrong answers. ANOVA is for comparing a handful of groups, but our factors are
# continuous measurements, so factor() creates one group per distinct value. Attendance
# has 3472 distinct values across 8000 students, which is about 2 students per group, so
# the test has almost no power left and reported attendance as not significant when it
# actually is. See the cell size table below.
#
# The ANOVA is kept at the bottom so the report can show the comparison, but the
# correlation tests and the regression are the results we are actually using.
#
# This reads cleaned_data_with_grades.csv, so run threshold_analysis.R first.

library(broom)

df = read.csv("data/cleaned_data_with_grades.csv")

vars = c("gaming_hours", "study_hours", "sleep_hours", "attendance", "addiction_score")

head(df)

# ---- Why ANOVA does not fit this data ----
# If a factor has close to one group per student there is nothing left to average
# within a group, which is what the F test needs.
cell_sizes = data.frame(
  variable    = vars,
  n_levels    = sapply(vars, function(v) length(unique(df[[v]]))),
  obs_per_cell = round(sapply(vars, function(v) nrow(df) / length(unique(df[[v]]))), 2),
  row.names   = NULL
)

cat("\n=== Distinct values per factor (why the ANOVA breaks down) ===\n")
print(cell_sizes)

# ---- Test 1: correlation of each factor with grades ----
# This is the direct replacement for the one way ANOVA. It asks the same question,
# "does this factor relate to grades", but it uses the fact that the values are ordered
# instead of throwing that information away.
cor_results = do.call(rbind, lapply(vars, function(v) {
  ct = cor.test(df[[v]], df$grades)
  m  = lm(df$grades ~ df[[v]])
  data.frame(
    variable    = v,
    r           = round(unname(ct$estimate), 3),
    p_value     = ct$p.value,
    slope       = round(unname(coef(m)[2]), 3),
    r_squared   = round(summary(m)$r.squared, 4),
    conclusion  = ifelse(ct$p.value < 0.05, "Reject", "Fail to reject")
  )
}))

cat("\n=== Correlation of each factor with grades ===\n")
print(cor_results)
write.csv(cor_results, "output/correlation_results.csv", row.names = FALSE)

# Every factor including attendance comes back significant on its own.
# The slope column is the useful part for the report: it says how many grade points
# one unit of each factor is worth, which a p value on its own never tells us.

# ---- Test 2: all five factors in one regression ----
# The correlations above look at one factor at a time, so a factor can look important
# just by riding along with another one. Putting them in the same model shows what each
# factor is worth once the others are already accounted for.
full_model = lm(grades ~ gaming_hours + study_hours + sleep_hours + attendance + addiction_score,
                data = df)

reg_results = tidy(full_model)
reg_results$conclusion = ifelse(reg_results$p.value < 0.05, "Reject", "Fail to reject")

cat("\n=== Multiple regression, all five factors together ===\n")
print(as.data.frame(reg_results))
cat(sprintf("\nAdjusted R-squared = %.4f\n", summary(full_model)$adj.r.squared))
write.csv(reg_results, "output/regression_results.csv", row.names = FALSE)

# addiction_score is the one factor that stops being significant here. That is because
# it is almost the same measurement as gaming hours:
cat(sprintf("\ncor(addiction_score, gaming_hours) = %.3f\n",
            cor(df$addiction_score, df$gaming_hours)))

# ---- Conclusion ----
# Study hours, gaming hours, sleep hours, and attendance are all statistically
# significant predictors of grades, both on their own and when tested together.
#
# Addiction score is significant on its own but not once the other factors are included.
# It correlates 0.909 with gaming hours, so it is not adding a separate effect, it is
# measuring gaming a second time. We report gaming hours as the real driver.
#
# Attendance is significant, which is the opposite of what the ANOVA version concluded.
#
# Limitation worth stating in the report: the adjusted R-squared of about 0.91 is
# unusually high for survey style data, and the 0.909 correlation between two supposedly
# separate variables is very clean. This Kaggle dataset is likely simulated rather than
# collected, so these results should be read as an exercise on the data as given, not as
# a real world finding about students.

# ---- Original ANOVA, kept for the method comparison in the report ----
# Heads up: this block is the slow part of the script, a couple of minutes, because
# aov() is fitting thousands of levels per model. Everything above it runs instantly.
# If you only need the results and not the comparison table, stop the script here.
anova_list = list(
  gaming     = aov(df$grades ~ factor(df$gaming_hours)),
  study      = aov(df$grades ~ factor(df$study_hours)),
  sleep      = aov(df$grades ~ factor(df$sleep_hours)),
  attendance = aov(df$grades ~ factor(df$attendance)),
  addiction  = aov(df$grades ~ factor(df$addiction_score))
)

results = do.call(rbind, lapply(names(anova_list), function(n) {
  tab = as.data.frame(summary(anova_list[[n]])[[1]])
  tab$term = rownames(tab)
  tab$model = n
  tab
}))

write.csv(results, "output/anova_results.csv", row.names = FALSE)

# Side by side, this is the table to put in the report when explaining the method change.
comparison = data.frame(
  variable    = vars,
  obs_per_cell = cell_sizes$obs_per_cell,
  anova_p      = sapply(anova_list, function(a) summary(a)[[1]][["Pr(>F)"]][1]),
  correct_p    = cor_results$p_value,
  row.names    = NULL
)
comparison$anova_says   = ifelse(comparison$anova_p < 0.05, "Reject", "Fail to reject")
comparison$correct_says = ifelse(comparison$correct_p < 0.05, "Reject", "Fail to reject")
comparison$agrees       = ifelse(comparison$anova_says == comparison$correct_says, "", "DISAGREES")

cat("\n=== ANOVA vs correlation test ===\n")
print(comparison)
write.csv(comparison, "output/method_comparison.csv", row.names = FALSE)
