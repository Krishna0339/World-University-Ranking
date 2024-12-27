# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("H:/assisgnment/hertfordshire assignment/Krishna stoke/Dataset/archive (1)/cwurData.csv")  
# Inspect the structure of the dataset
str(data)

# Clean data (e.g., removing any NA values in the relevant columns)
data_clean <- data %>%
  drop_na(quality_of_education, world_rank)

# Data exploration: Summary statistics
summary(data_clean)

# Visualize the relationship between Quality of Education and World Rank
ggplot(data_clean, aes(x = quality_of_education, y = world_rank)) +
  geom_point(color = 'blue') +  # Points in blue color
  labs(title = "Scatter Plot: Quality of Education vs. World Rank", 
       x = "Quality of Education", 
       y = "World Rank") +
  theme_minimal()

# Perform linear regression: World Rank ~ Quality of Education
model <- lm(world_rank ~ quality_of_education, data = data_clean)

# Summary of the regression model
summary(model)

# Diagnostic plots for the regression model
par(mfrow = c(2, 2))  # Set up the plot area for 4 plots
plot(model)

# Hypothesis testing: Null hypothesis H0: Quality of Education has no effect on World Rank
# Based on the p-value from the model summary, we will reject or fail to reject the null hypothesis.
p_value <- summary(model)$coefficients[2, 4]
cat("P-value from regression:", p_value, "\n")

# Interpretation of null hypothesis rejection based on p-value
if (p_value < 0.05) {
  print("Null hypothesis is rejected: Quality of education significantly affects world rank.")
} else {
  print("Null hypothesis is not rejected: Quality of education does not significantly affect world rank.")
}

# Extracting R-squared value and interpreting its meaning
r_squared <- summary(model)$r.squared
cat("R-squared value:", r_squared, "\n")
cat("Interpretation: R-squared suggests that", round(r_squared * 100, 2), "% of the variation in world rank can be explained by quality of education.\n")

# Visualize the regression line
ggplot(data_clean, aes(x = quality_of_education, y = world_rank)) +
  geom_point(color = 'blue') + 
  geom_smooth(method = 'lm', color = 'red') + 
  labs(title = "Regression Line: Quality of Education vs. World Rank", 
       x = "Quality of Education", 
       y = "World Rank") +
  theme_minimal()

# Save the plots
ggsave("scatter_plot.png")
ggsave("regression_plot.png")

# Save the model summary as a text file for later use
capture.output(summary(model), file = "model_summary.txt")

