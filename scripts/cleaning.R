library(tidyverse)

# ---- Load data ----
df <- read_csv("data/Gaming_Academic_Performance.csv")

# Structure and types
glimpse(df)

# Missing values check
colSums(is.na(df))

# ---- Flag grades issue ----
# Some grades exceed 100, which shouldn't be possible on a normal scale
summary(df$grades)
df %>% filter(grades > 100) %>% nrow()

# ---- Clean grades ----
# 134 records (1.7%) had grades over 100, up to 118.63
# These appear to be genuine high performers (more study hours, less gaming,
# better attendance, lower addiction scores vs rest of sample), not data errors
# Capped at 100 to preserve these informative cases rather than dropping them
df_clean <- df %>%
  mutate(grades = pmin(grades, 100))

# ---- Convert categorical columns to factors ----
df_clean <- df_clean %>%
  mutate(
    gender = as.factor(gender),
    gaming_genre = as.factor(gaming_genre),
    stress_level = factor(stress_level, levels = c("Low", "Medium", "High"), ordered = TRUE)
  )

# ---- Post-cleaning check ----
summary(df_clean$grades)

# Histogram of grades with key values labeled (only bars over 400 students get a number)
ggplot(df_clean, aes(x = grades)) +
  geom_histogram(binwidth = 5, color = "black", fill = "steelblue") +
  stat_bin(binwidth = 5, geom = "text", 
           aes(label = ifelse(after_stat(count) > 400, after_stat(count), "")),
           vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of Grades", x = "Grade", y = "Frequency")

# ---- Save cleaned data ----
write_csv(df_clean, "data/cleaned_data.csv")