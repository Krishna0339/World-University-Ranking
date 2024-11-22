install.packages("rio")
library(rio)
my_data <- import("C:\Users\Lenovo\Downloads\dataset sports ticket.xlsx")
'hexView'
1
local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
if(nchar(pkg)) library(pkg, character.only=TRUE)})
local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
if(nchar(pkg)) library(pkg, character.only=TRUE)})
q()
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stats)
# Load dataset (replace the file path with your actual file path)
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
df <- read.csv(file_path)
# View dataset structure
head(df)
# Clean the data by removing rows with missing values in 'Salary.Estimate' or 'Type.of.ownership'
df <- df %>% 
  drop_na(Salary.Estimate, Type.of.ownership)
# Convert 'Salary.Estimate' to numeric, remove non-numeric characters if present
df$Salary.Estimate <- as.numeric(gsub("[^0-9.-]", "", df$Salary.Estimate))
# Check the cleaned data
summary(df)
# Boxplot to visualize Salary Estimate by Type of Ownership
boxplot_plot <- ggplot(df, aes(x = as.factor(Type.of.ownership), y = Salary.Estimate)) +
  geom_boxplot() +
  labs(title = "Salary Estimate by Type of Ownership", 
       x = "Type of Ownership", 
       y = "Salary Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the boxplot to a file
ggsave("boxplot_salary_by_ownership.png", plot = boxplot_plot, width = 8, height = 6)
print(boxplot_plot)
# Histogram to check the distribution of Salary Estimate
histogram_plot <- ggplot(df, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", 
       x = "Salary Estimate", 
       y = "Frequency") +
  theme_minimal()
# Save the histogram to a file
ggsave("histogram_salary_estimate.png", plot = histogram_plot, width = 8, height = 6)
print(histogram_plot)
> histogram_plot <- ggplot(df, aes(x = Salary.Estimate)) +
+   geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
+   labs(title = "Salary Estimate Distribution", 
+        x = "Salary Estimate", 
+        y = "Frequency") +
+   theme_minimal()
> 
> # Save the histogram to a file
> ggsave("histogram_salary_estimate.png", plot = histogram_plot, width = 8, height = 6)
Warning message:
Removed 2252 rows containing non-finite outside the scale range (`stat_bin()`). 
> print(histogram_plot)
Warning message:
Removed 2252 rows containing non-finite outside the scale range (`stat_bin()`). 
# Perform the Shapiro-Wilk test for normality on 'Salary.Estimate'
shapiro_test <- shapiro.test(df$Salary.Estimate)
# Show Shapiro-Wilk test result in a plot
normality_plot <- ggplot() +
  annotate("text", x = 1, y = 1, 
           label = paste("Shapiro-Wilk Test p-value: ", round(shapiro_test$p.value, 5)), 
           size = 6) +
  theme_void() +
  ggtitle("Shapiro-Wilk Normality Test Result")
# Save the normality plot to a file
ggsave("shapiro_wilk_test.png", plot = normality_plot, width = 8, height = 6)
print(normality_plot)
# Perform ANOVA to check if there are significant differences in Salary Estimate across different Types of Ownership
anova_result <- aov(Salary.Estimate ~ Type.of.ownership, data = df)
# Display the summary of ANOVA results
summary(anova_result)
# Conclusion based on ANOVA p-value
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  print("There is a statistically significant difference in Salary Estimates across different Types of Ownership.")
} else {
  print("There is no statistically significant difference in Salary Estimates across different Types of Ownership.")
}
# Load the necessary library
library(tidyr)
# Clean the data by removing rows with missing values in 'Salary.Estimate' or 'Type.of.ownership'
df <- df %>% 
  drop_na(Salary.Estimate, Type.of.ownership)
# Convert 'Salary.Estimate' to numeric, handling non-numeric characters
df$Salary.Estimate <- as.numeric(gsub("[^0-9.-]", "", df$Salary.Estimate))
# Check for any remaining NAs
sum(is.na(df$Salary.Estimate))
# Create a boxplot for Salary Estimate by Type of Ownership
boxplot_plot <- ggplot(df, aes(x = as.factor(Type.of.ownership), y = Salary.Estimate)) +
  geom_boxplot() +
  labs(title = "Salary Estimate by Type of Ownership", 
       x = "Type of Ownership", 
       y = "Salary Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display boxplot
print(boxplot_plot)
# Create a histogram for the distribution of Salary Estimate
histogram_plot <- ggplot(df, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", 
       x = "Salary Estimate", 
       y = "Frequency") +
  theme_minimal()
# Display histogram
print(histogram_plot)
# Perform the Shapiro-Wilk test to check normality of Salary Estimate
shapiro_test <- shapiro.test(df$Salary.Estimate)
# Visualize the Shapiro-Wilk test result
normality_plot <- ggplot() +
  annotate("text", x = 1, y = 1, 
           label = paste("Shapiro-Wilk Test p-value: ", round(shapiro_test$p.value, 5)), 
           size = 6) +
  theme_void() +
  ggtitle("Shapiro-Wilk Normality Test Result")
# Display the normality plot
print(normality_plot)
# Remove rows with NA values in Salary.Estimate
df_clean <- df %>% drop_na(Salary.Estimate)
# Ensure there are enough data points for the Shapiro-Wilk test (more than 3)
if (nrow(df_clean) > 3) {
  # Perform the Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(df_clean$Salary.Estimate)
  # Visualize the Shapiro-Wilk test result
  normality_plot <- ggplot() +
    annotate("text", x = 1, y = 1, 
             label = paste("Shapiro-Wilk Test p-value: ", round(shapiro_test$p.value, 5)), 
             size = 6) +
    theme_void() +
    ggtitle("Shapiro-Wilk Normality Test Result")
  # Display the normality plot
  print(normality_plot)
} else {
  print("Not enough data for Shapiro-Wilk normality test.")
}
# Load required libraries
library(dplyr)
library(ggplot2)
# Clean the data by removing rows with NA values in 'Salary.Estimate'
df_clean <- df %>% drop_na(Salary.Estimate)
# Ensure there are enough data points for the Shapiro-Wilk test (more than 3 valid values)
if (length(df_clean$Salary.Estimate) > 3) {
  # Perform the Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(df_clean$Salary.Estimate)
  # Visualize the Shapiro-Wilk test result
  normality_plot <- ggplot() +
    annotate("text", x = 1, y = 1, 
             label = paste("Shapiro-Wilk Test p-value: ", round(shapiro_test$p.value, 5)), 
             size = 6) +
    theme_void() +
    ggtitle("Shapiro-Wilk Normality Test Result")
  # Display the normality plot
  print(normality_plot)
} else {
  print("Not enough valid data for Shapiro-Wilk normality test.")
}
# Load required libraries
library(dplyr)
# Specify the file path to your dataset
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
# Load the dataset into a data frame
df <- read.csv(file_path)
# Check the first few rows to confirm the data has loaded correctly
head(df)
# Load required libraries
library(ggplot2)
# Ensure Salary.Estimate is numeric and clean any NA values
df_clean <- df %>% drop_na(Salary.Estimate)
df_clean$Salary.Estimate <- as.numeric(df_clean$Salary.Estimate)
# Create a histogram to visualize Salary.Estimate distribution
histogram_plot <- ggplot(df_clean, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", x = "Salary Estimate", y = "Frequency") +
  theme_minimal()
# Display the histogram
print(histogram_plot)
# Optionally, save the plot as a PNG
ggsave("salary_histogram.png", plot = histogram_plot, width = 8, height = 6)
# Load required libraries
library(ggplot2)
# Load the dataset
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
df <- read.csv(file_path)
# Clean data (remove NA values)
df_clean <- df %>% drop_na(Salary.Estimate)
# Create histogram for Salary.Estimate
ggplot(df_clean, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", x = "Salary Estimate", y = "Frequency") +
  theme_minimal()
# Load required libraries
library(ggplot2)
library(dplyr)
# Load the dataset
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
df <- read.csv(file_path)
# Clean data (remove rows with NA values in Salary.Estimate)
df_clean <- df %>% drop_na(Salary.Estimate)
# Create and display histogram
ggplot(df_clean, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", x = "Salary Estimate", y = "Frequency") +
  theme_minimal()
# Load required libraries
library(ggplot2)
library(dplyr)
# Load the dataset
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
df <- read.csv(file_path)
# Clean the data by removing rows with NA values in 'Salary.Estimate'
df_clean <- df %>% drop_na(Salary.Estimate)
# Create a histogram to visualize the distribution of Salary.Estimate
ggplot(df_clean, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", x = "Salary Estimate", y = "Frequency") +
  theme_minimal()
# Load required libraries
library(ggplot2)
library(dplyr)
# Load the dataset (update the file path accordingly)
file_path <- "H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv"
df <- read.csv(file_path)
# Clean the data by removing rows with NA values in 'Salary.Estimate'
df_clean <- df %>% drop_na(Salary.Estimate)
# Ensure Salary.Estimate is numeric (if not already)
df_clean$Salary.Estimate <- as.numeric(df_clean$Salary.Estimate)
# Create a histogram to visualize the distribution of Salary.Estimate
histogram_plot <- ggplot(df_clean, aes(x = Salary.Estimate)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Salary Estimate Distribution", x = "Salary Estimate", y = "Frequency") +
  theme_minimal()
# Print histogram
print(histogram_plot)
# Create a boxplot to visualize Salary Estimate by Type of Ownership
boxplot_plot <- ggplot(df_clean, aes(x = as.factor(Type.of.ownership), y = Salary.Estimate)) +
  geom_boxplot() +
  labs(title = "Salary Estimate by Type of Ownership", x = "Type of Ownership", y = "Salary Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print boxplot
print(boxplot_plot)
# Perform Shapiro-Wilk test to check the normality of Salary.Estimate
if (length(df_clean$Salary.Estimate) > 3) {
  shapiro_test <- shapiro.test(df_clean$Salary.Estimate)
  # Visualize the Shapiro-Wilk test result
  normality_plot <- ggplot() +
# Load required libraries
library(ggplot2)
library(dplyr)
# Load the dataset
data <- read.csv("H:/assisgnment/hertfordshire assignment/Bhanu_hertfordshire/Team Research/Dataset/archive (1)/DataAnalyst.csv")
# Check the structure of the dataset to confirm column names
str(data)
# Clean the data if necessary
# You may need to clean up the 'Salary Estimate' column by removing any unwanted characters (e.g., $ and commas) and converting it to numeric
data$Salary.Estimate <- gsub("[\\$,]", "", data$Salary.Estimate) # Remove $ and commas
data$Salary.Estimate <- as.numeric(data$Salary.Estimate)  # Convert to numeric
# Check if 'Type of Ownership' is a factor or character type, if not convert it
data$Type.of.Ownership <- as.factor(data$Type.of.Ownership)
# Create the histogram based on Type of Ownership
ggplot(data, aes(x = Salary.Estimate, fill = Type.of.Ownership)) + 
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) + 
  labs(title = "Salary Estimate Distribution by Type of Ownership",
       x = "Salary Estimate",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set3")
