


# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(janitor)
library(ggcorrplot)
library(stringr)



# Download directly from UCI repository
url_mat <- "https://raw.githubusercontent.com/arunk13/MSDA-Assignments/master/IS607Fall2015/Assignment3/student-mat.csv"
df1_mat <- read.csv(url_mat, sep = ";")

# View structure
glimpse(df1_mat)
dim(df1_mat)

#  Cleaning Conventions


# Clean column names
df1_mat <- df1_mat %>% janitor::clean_names()

# Check for missing values
colSums(is.na(df1_mat))

# Replace missing numeric values with column mean
df1_mat<- df1_mat %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Replace missing categorical values with "Unknown"
df1_mat <- df1_mat %>%
  mutate(across(where(is.character),
                ~ ifelse(is.na(.), "Unknown", .)))

# Remove duplicates
df1_mat <- df1_mat %>% distinct()

# Trim whitespace
df1_mat <- df1_mat %>%
  mutate(across(where(is.character), str_trim))

#
# Transformation Conventions


# Create new derived columns
df1_mat <- df1_mat %>%
  mutate(
    avg_grade = (g1 + g2 + g3) / 3,
    pass_fail = ifelse(avg_grade >= 10, "Pass", "Fail"),
    study_effort = case_when(
      studytime <= 2 ~ "Low",
      studytime == 3 ~ "Moderate",
      TRUE ~ "High"
    )
  )

# Convert categorical variables to factors
df1_mat <- df1_mat %>%
  mutate(across(where(is.character), as.factor))

glimpse(df1_mat)


#  Exploratory Data Analysis (EDA)


# Summary stats
summary(df1_mat)

# Count of students by gender
ggplot(df1_mat, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Count of Students by Gender", x = "Gender", y = "Count")

# Distribution of average grades
ggplot(df1_mat, aes(x = avg_grade)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Average Grades", x = "Average Grade", y = "Frequency")

# Study time vs grade
ggplot(df1_mat, aes(x = factor(studytime), y = avg_grade, fill = factor(studytime))) +
  geom_boxplot() +
  labs(title = "Study Time vs Average Grade", x = "Study Time", y = "Average Grade")

# Family relationship vs academic performance
ggplot(df1_mat, aes(x = factor(famrel), y = avg_grade, fill = factor(famrel))) +
  geom_boxplot() +
  labs(title = "Family Relationship vs Academic Performance",
       x = "Family Relationship (1=very bad, 5=excellent)", y = "Average Grade")

#  Correlation Analysis


# Select numeric columns
num_df1 <- df1_mat %>% select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(num_df1, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)





