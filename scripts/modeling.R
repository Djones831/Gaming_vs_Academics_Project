# Build a regression model that predicts grades from student lifestyle factors.
#
# stat_testing.R established which factors matter. This script asks the two questions
# a p value cannot answer:
#   1. what shape is each relationship, straight line or curved
#   2. how well does the model predict students it has never seen
#
# Models are fit on a training split and scored on a held out test split, so the
# accuracy reported here is not the model grading its own homework.

library(tidyverse)

df <- read_csv("data/cleaned_data.csv", show_col_types = FALSE)

dir.create("output", showWarnings = FALSE)

# ---- Train / test split ----
# 70/30, same seed as threshold_analysis.R so the two scripts stay comparable.
set.seed(432)
train_idx <- sample(nrow(df), size = floor(0.7 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

cat(sprintf("Training on %d students, testing on %d\n", nrow(train), nrow(test)))

# Grades were capped at 100 in cleaning.R, so a prediction above 100 cannot be right
# no matter what the formula says. Clamp before scoring.
rmse <- function(actual, predicted) sqrt(mean((actual - pmin(predicted, 100))^2))

r2 <- function(actual, predicted) {
  1 - sum((actual - pmin(predicted, 100))^2) / sum((actual - mean(actual))^2)
}

# ---- Candidate models, simplest first ----
# Each adds something the previous one lacked, so the comparison shows whether the
# extra complexity actually earns its place.
models <- list(
  # study hours alone, the strongest single predictor, as a floor to beat
  study_only = grades ~ study_hours,

  # all five factors, straight line each. This is the model from stat_testing.R
  main_effects = grades ~ gaming_hours + study_hours + sleep_hours + attendance +
                          addiction_score,

  # gaming and addiction both bend rather than run straight, so add squared terms
  with_curves = grades ~ gaming_hours + I(gaming_hours^2) + study_hours + sleep_hours +
                         attendance + addiction_score + I(addiction_score^2),

  # the decision tree split gaming at 4.61 in one branch and 3.95 in the other, which
  # is what an interaction term represents: gaming costs more when study hours are low
  with_interaction = grades ~ gaming_hours * study_hours + I(gaming_hours^2) +
                              sleep_hours + attendance + addiction_score +
                              I(addiction_score^2),

  # addiction_score correlates 0.909 with gaming_hours, so check what dropping it costs
  no_addiction = grades ~ gaming_hours * study_hours + I(gaming_hours^2) +
                          sleep_hours + attendance
)

fits <- lapply(models, function(f) lm(f, data = train))

comparison <- imap_dfr(fits, function(m, name) {
  tibble(
    model      = name,
    n_terms    = length(coef(m)) - 1,
    train_rmse = round(rmse(train$grades, predict(m, train)), 3),
    test_rmse  = round(rmse(test$grades,  predict(m, test)),  3),
    test_r2    = round(r2(test$grades,    predict(m, test)),  4)
  )
})

cat("\n=== Model comparison, scored on the held out test set ===\n")
print(as.data.frame(comparison))
write_csv(comparison, "output/model_comparison.csv")

# train_rmse and test_rmse staying close is the sign we are not overfitting. If the
# test error were much worse than the training error the model would be fitting noise.

# ---- Choose the final model ----
# Not simply the lowest test_rmse. Two of these land within 0.001 of each other, which
# is noise from this particular split rather than a real difference, and picking on raw
# minimum would hand us extra terms that do nothing. So take the model with the fewest
# terms that gets within 1% of the best error.
best_rmse <- min(comparison$test_rmse)

best_name <- comparison %>%
  filter(test_rmse <= best_rmse * 1.01) %>%
  slice_min(n_terms, n = 1) %>%
  slice_min(test_rmse, n = 1) %>%
  pull(model)

cat(sprintf("\nBest test error: %.3f (%s)\n", best_rmse,
            comparison$model[which.min(comparison$test_rmse)]))
cat(sprintf("Chosen for parsimony, within 1%% of best: %s\n", best_name))

# Refit the chosen structure on all 8000 students for the coefficients we report.
final_model <- lm(models[[best_name]], data = df)

coefs <- broom::tidy(final_model) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== Final model coefficients (fit on all 8000 students) ===\n")
print(as.data.frame(coefs))
cat(sprintf("\nAdjusted R-squared = %.4f\n", summary(final_model)$adj.r.squared))
cat(sprintf("Residual standard error = %.2f grade points\n", summary(final_model)$sigma))

write_csv(coefs, "output/model_coefficients.csv")

# ---- Diagnostics ----
# Students sitting exactly on the 100 point cap are flagged separately. Their actual
# grade was truncated, so their residual is forced negative by the cap rather than by
# the model being wrong. Mixing them into the trend line would misrepresent the fit.
diag_df <- tibble(
  fitted   = fitted(final_model),
  residual = resid(final_model),
  censored = df$grades >= 100
)

p1 <- ggplot(diag_df, aes(x = fitted, y = residual)) +
  geom_point(aes(colour = censored), alpha = 0.25, size = 0.9) +
  geom_hline(yintercept = 0, colour = "red") +
  # loess on the uncensored students only, so the cap does not drag the line down
  geom_smooth(data = filter(diag_df, !censored), method = "loess",
              colour = "blue", se = FALSE) +
  scale_colour_manual(values = c("FALSE" = "grey30", "TRUE" = "darkorange"),
                      labels = c("FALSE" = "normal", "TRUE" = "at 100 point cap"),
                      name = NULL) +
  labs(title = "Residuals vs fitted values",
       subtitle = paste("Blue line is flat across the normal range, so the model fits.",
                        "\nThe orange diagonal is the grade cap, not model error."),
       x = "Predicted grade", y = "Residual") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("output/residuals_vs_fitted.png", p1, width = 8, height = 5.5, dpi = 150)

# Q-Q plot, uncensored students only for the same reason.
p2 <- ggplot(filter(diag_df, !censored), aes(sample = residual)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(colour = "red") +
  labs(title = "Normal Q-Q plot of residuals",
       subtitle = "Excludes students at the 100 point cap. Points on the line means the normality assumption holds",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()

ggsave("output/qq_residuals.png", p2, width = 8, height = 5, dpi = 150)

# Predicted vs actual on the test set, the plain visual of how good the model is.
test_pred <- tibble(
  actual    = test$grades,
  predicted = pmin(predict(final_model, test), 100)
)

p3 <- ggplot(test_pred, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  labs(title = "Predicted vs actual grades, held out test set",
       subtitle = "Red line is a perfect prediction",
       x = "Actual grade", y = "Predicted grade") +
  theme_minimal()

ggsave("output/predicted_vs_actual.png", p3, width = 7, height = 6, dpi = 150)

# ---- Limitations to state in the report ----
ceiling_n <- sum(df$grades >= 100)
cat(sprintf("\nStudents at the 100 point ceiling: %d (%.1f%%)\n",
            ceiling_n, 100 * ceiling_n / nrow(df)))

# 1. cleaning.R capped grades at 100, so about 7% of students sit exactly on the
#    ceiling. The model cannot tell apart a student who would have scored 101 from one
#    who would have scored 118. A censored regression (tobit) handles this properly but
#    is beyond our scope, so we flag it instead.
# 2. An adjusted R-squared this high is not typical of real survey data. This Kaggle
#    dataset appears to be simulated, so the numbers demonstrate the method working
#    rather than a real world claim about what gaming costs a student.
# 3. This is observational. Students who game more also study less, so the coefficients
#    describe association, not proof that cutting gaming would raise a grade.

cat("\nDone. Wrote model tables and diagnostic plots to output/\n")
