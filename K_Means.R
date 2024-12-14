# Importing necessary packages


library(stats)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)



# Load the data from the Excel file
data <- read.csv("C:\\programming\\R language\\DS project\\cleaned6_grc.csv")

# Check column names and the number of missing values in each column
print("Column names in the data:")
print(colnames(data))

print("Missing values before cleaning:")
print(sapply(data, function(x) sum(is.na(x))))

# Remove rows with any missing values (NA) in 'age' or 'total'
data_clean <- data %>%
  select(age, total) %>%
  filter(!is.na(age) & !is.na(total))

# Remove duplicate rows from the cleaned data to prevent any many-to-many relationships
data_clean <- data_clean %>% distinct()

# Verify the cleaned data (show the first few rows)
print("Data after cleaning:")
print(head(data_clean))

# Check for any missing values in the cleaned data
print("Missing values after cleaning:")
print(sapply(data_clean, function(x) sum(is.na(x))))

# Set the number of clusters and a random seed for reproducibility
set.seed(200)
n_clusters <- 3

# Apply K-means clustering to 'age' and 'total' columns
kmeans_result <- kmeans(data_clean, centers = n_clusters)

# Add the cluster assignment to the cleaned data
data_clean$Cluster <- kmeans_result$cluster

# Merge the cluster assignments back to the original data
data_with_cluster <- left_join(data, data_clean, by = c("age", "total"))

# Check if there are any rows in the original data that were not matched with the cleaned data
unmatched_rows <- data %>% anti_join(data_clean, by = c("age", "total"))
print("Rows in data not matched with data_clean:")
print(unmatched_rows)

# Display the final data with the cluster assignments
print("Final Data with Clusters:")
print(head(data_with_cluster))

# Check for missing values in the final data after the merge
print("Missing values in the final data:")
print(sapply(data_with_cluster, function(x) sum(is.na(x))))

# Plot the data using ggplot to visualize customer segmentation by cluster
ggplot(data_with_cluster, aes(x = age, y = total, color = factor(Cluster))) +
  geom_point(size = 3) +  # Plotting data points
  labs(
    title = "Customer Segmentation",  # Title of the plot
    x = "Age",  # Label for the x-axis
    y = "Total Spending",  # Label for the y-axis
    color = "Cluster"  # Legend label for cluster colors
  ) +
  theme_minimal()  # Using a minimal theme for the plot

#create a table with customer names, age, total spending, and their assigned cluster number.
customer_table <- data_with_cluster %>%
  select(customer, age, total, Cluster)

# Print the table displaying customer information
print("Customer Table with Name, Age, Total Spending, and Cluster:")
print(customer_table)
