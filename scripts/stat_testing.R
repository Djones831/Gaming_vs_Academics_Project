# Goal: Preform ANOVA testing to find statistical significance in grades vs the other factors
# time


library(broom)

df = read.csv("cleaned_data_with_grades.csv")

# The factors our group wants to compare with grades are: gaming hours, study hours,
# sleep hours, attendance, and addiction score

# With the way this data is formatted, we will have to use single factor ANOVA
# This is to test if there is any significance that the factors we are looking at have towards the grades
# With this in mind, we are using 95% confidence to determine statistical significance 
# (which means we are using an alpha value of 5)

# The null hypothesis is that there is no statistical significance and that the it does not matter
# The alternative would be that there is statistical significance and that it does


head(df)

gaming_anova = aov(df$grades ~ factor(df$gaming_hours))
summary(gaming_anova)
# Reject

study_anova = aov(df$grades ~ factor(df$study_hours))
summary(study_anova)
# Reject

sleep_anova = aov(df$grades ~ factor(df$sleep_hours))
summary(sleep_anova)
# Reject

attendance_anova = aov(df$grades ~ factor(df$attendance))
summary(attendance_anova)
# Fail to reject

addiction_anova = aov(df$grades ~ factor(df$addiction_score))
summary(addiction_anova)
# Reject

# Conclusion:
# It seems that all the variables are statistically significance to changes in grades
# except for attendance. This means that any impact they had on this values actually matters.

# Converting the summaries into a list to put into a csv
anova_list = list(
  gaming = gaming_anova,
  study = study_anova,
  sleep = sleep_anova,
  attendance = attendance_anova,
  addiction = addiction_anova
)

results = do.call(rbind, lapply(names(anova_list), function(n) {
  tab = as.data.frame(summary(anova_list[[n]])[[1]])
  tab$term = rownames(tab)
  tab$model = n
  tab
}))

write.csv(results, "anova_results.csv", row.names = FALSE)




