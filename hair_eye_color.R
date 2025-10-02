# Load libraries
library(ggplot2)
library(dplyr)
# Convert HairEyeColor dataset to a data frame
df <- as.data.frame(HairEyeColor)

# 1. Blank plot with data and aesthetics (Hair vs Eye, size = Freq)
p <- ggplot(df, aes(x = Hair, y = Eye, size = Freq, color = Sex))

# 2. Adding a geom (scatterplot)
p + geom_point() 
# 3. Adding a statistical transformation (linear regression line)
# (Here we use numeric index for Eye to fit regression line)
df2 <- df %>% mutate(EyeNum = as.numeric(Eye))
ggplot(df2, aes(x = EyeNum, y = Freq, )) +
  geom_smooth(method = "lm", se = FALSE) 
  

# 4. Customizing with a color scale (categorical)
ggplot(df, aes(x = Hair, y = Freq, fill = Eye)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Bar Chart with Custom Color Scale")

# 5. Coordinate system: Flipped bar chart
ggplot(df, aes(x = Hair, y = Freq, fill = Eye)) +
  geom_bar(stat = "identity", position="dodge") +
  coord_flip() +
  ggtitle("Flipped Bar Chart of Hair vs Frequency")


# 6. Faceting by Sex
ggplot(df, aes(x = Eye, y = Freq, fill = Hair)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sex) +
  ggtitle("Faceted by Sex")

# 7. Applying a theme and labels
ggplot(df, aes(x = Hair, y = Freq, fill = Eye)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sex) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Hair and Eye Color Frequencies by Sex",
       x = "Hair Color", 
       y = "Count",
       fill = "Eye Color") +
  theme_minimal()
