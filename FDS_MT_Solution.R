# Qno.2

# i)

# Extract Salary column as vector and Calculate average salary
salary_vec <- employee_df$Salary
avg_salary <- mean(salary_vec)

#ii)

# Store ages in a vector and Minimum and maximum ages
age_vec <- employee_df$Age
min_age <- min(age_vec)
max_age <- max(age_vec)

# iii)

# Create list for a single employee and Display each element
employee1 <- list(
  Name = "X",
  Department = "HR",
  Age = 34,
  Salary = 50000
)
employee1$Name
employee1$Department
employee1$Age
employee1$Salary

# iv)

# Mean of Salary, Standard deviation of Salary and Correlation between Age and Salary
mean_salary <- mean(employee_df$Salary)
sd_salary <- sd(employee_df$Salary)
cor_age_salary <- cor(employee_df$Age, employee_df$Salary)

# Qno.3

# i) scatter plot to illustrate the relationship between hp and mpg

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Horsepower vs. MPG",
       x = "Horsepower (hp)",
       y = "Miles Per Gallon (mpg)")

# ii) boxplot to show the distribution of mpg for each cylinder category

ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Boxplot of MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Miles Per Gallon (mpg)")

# iii ) histogram to display the distribution of the wt variable  

ggplot(mtcars, aes(x = wt)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Car Weights",
       x = "Weight (1000 lbs)",
       y = "Frequency")








