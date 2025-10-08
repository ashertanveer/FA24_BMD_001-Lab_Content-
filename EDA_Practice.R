

library(palmerpenguins)
library(ggplot2)
library(dplyr)

#  Load dataset
datasets::penguins

#  Check summary
summary(penguins)

# Check missing values
colSums(is.na(penguins))

#  Standardize column names
colnames(penguins) <- c("species", "island", "bill_length_mm", 
                        "bill_depth_mm", "flipper_length_mm", 
                        "body_mass_g", "sex", "year")

#  Convert data types
penguins$species <- as.factor(penguins$species)
penguins$island <- as.factor(penguins$island)
penguins$sex <- as.factor(penguins$sex)
penguins$year <- as.integer(penguins$year)

# — analyzing one variable at a time(univariate analysis)

# 1. Summary statistics for numeric columns
summary(select(penguins, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# 2. Frequency table for categorical variables
table(penguins$species)
table(penguins$island)
table(penguins$sex, useNA = "ifany")  # includes NAs

# 3. Visualizations
# Histogram of body mass
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Penguin Body Mass", x = "Body Mass (g)", y = "Count")

# Bar chart of species
ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Count of Penguins by Species", x = "Species", y = "Count")


# — analyzing relationships between two variables(bivariate analysis)

# 1. Correlation between numeric variables
cor(penguins$bill_length_mm, penguins$flipper_length_mm, use = "complete.obs")

# 2. Scatter plot of bill length vs flipper length
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point() +
  labs(title = "Bill Length vs Flipper Length by Species",
       x = "Bill Length (mm)", y = "Flipper Length (mm)")

# 3. Boxplot: body mass by sex
ggplot(penguins, aes(x = sex, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(title = "Body Mass by Sex", x = "Sex", y = "Body Mass (g)")

# 4. Average flipper length by island
penguins %>%
  group_by(island) %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE))

# — analyzing 3 or more variables together(multivariate analysis)

# 1. Scatter plot of 3 variables (color by species)
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm, color = species, size = body_mass_g)) +
  geom_point(alpha = 0.7) +
  labs(title = "Multivariate: Bill vs Flipper Length, Colored by Species & Sized by Body Mass")

# 2. Boxplot comparing body mass across both sex and species
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species and Sex", x = "Species", y = "Body Mass (g)")

# 3. Correlation matrix for numeric variables
numeric_cols <- select(penguins, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
cor_matrix <- cor(numeric_cols, use = "complete.obs")
print(cor_matrix)



