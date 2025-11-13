
# Load required libraries
library(dplyr)
library(ggplot2)
library(ggcorrplot)

# Import dataset
data <- read.csv("openaq_location_8876_measurments.csv")

# Display structure and summary
str(data)
summary(data)

# Show number of rows and columns
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

# Show missing values per column
colSums(is.na(data))


# Remove duplicate rows
data <- distinct(data)

# Rename columns for clarity (example — adjust as needed)
names(data) <- tolower(names(data))
names(data) <- gsub("\\.", "_", names(data))  # replace dots with underscores

# Handle missing values — remove rows with too many NAs
data <- data %>%
  filter(complete.cases(.))

# Convert data types
# Convert date column to Date format (if available)
if("date_utc" %in% names(data)) {
  data$date_utc <- as.POSIXct(data$date_utc, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
}

# Convert pollutants to numeric (if not already)
pollutants <- c("pm25", "pm10", "no2", "co", "so2")
for (p in pollutants) {
  if (p %in% names(data)) {
    data[[p]] <- as.numeric(data[[p]])
  }
}

# Handle outliers using IQR method
for (p in pollutants) {
  if (p %in% names(data)) {
    Q1 <- quantile(data[[p]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[p]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    data <- data %>% filter(data[[p]] >= (Q1 - 1.5 * IQR) & data[[p]] <= (Q3 + 1.5 * IQR))
  }
}


# a) Descriptive Statistics
for (p in pollutants) {
  if (p %in% names(data)) {
    cat("\n--- Statistics for", p, "---\n")
    cat("Mean:", mean(data[[p]], na.rm=TRUE), "\n")
    cat("Median:", median(data[[p]], na.rm=TRUE), "\n")
    cat("Variance:", var(data[[p]], na.rm=TRUE), "\n")
    cat("Standard Deviation:", sd(data[[p]], na.rm=TRUE), "\n")
  }
}

# Correlation matrix for numeric variables
num_data <- data %>% select(where(is.numeric))
corr_matrix <- cor(num_data, use="complete.obs")
print(corr_matrix)

# Correlation heatmap
ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Heatmap of Pollutants")


# b) Visual Analysis

# Histogram for pollutant distributions
for (p in pollutants) {
  if (p %in% names(data)) {
    ggplot(data, aes(x = data[[p]])) +
      geom_histogram(fill="steelblue", color="black", bins=30) +
      ggtitle(paste("Distribution of", p)) +
      theme_minimal()
  }
}

# Boxplot for pollutants across locations
if("location" %in% names(data)) {
  ggplot(data, aes(x = location, y = pm25, fill = location)) +
    geom_boxplot() +
    theme_minimal() +
    ggtitle("PM2.5 Variation Across Locations") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Time-series line plot for pollutant trends
if("date_utc" %in% names(data) && "pm25" %in% names(data)) {
  ggplot(data, aes(x = date_utc, y = pm25)) +
    geom_line(color = "tomato") +
    theme_minimal() +
    ggtitle("PM2.5 Concentration Over Time")
}


