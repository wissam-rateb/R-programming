# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Specify the file path
file_path <- "data34.xlsx"

# Read the data into a DataFrame
df <- read_excel(file_path)

# Display the first few rows of the data
head(df)

# Descriptive Statistics
summary(df)

# Group by 'Märke' and calculate average price
average_price_by_brand <- df %>%
  group_by(Märke) %>%
  summarise(Average_Price = mean(Pris, na.rm = TRUE))

print(average_price_by_brand)

# Count the number of cars by fuel type
fuel_type_count <- df %>%
  group_by(Bränsle) %>%
  summarise(Count = n())

print(fuel_type_count)

# Group by 'Län' and calculate the total number of cars and average price
if ("Län" %in% colnames(df)) {
  analysis_by_country <- df %>%
    group_by(Län) %>%
    summarise(
      Total_Cars = n(),
      Average_Price = mean(Pris, na.rm = TRUE)
    )
  
  print(analysis_by_country)
  
  # Visualization: Bar plot of average price by country
  ggplot(analysis_by_country, aes(x = Län, y = Average_Price)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Average Price by Country", x = "Country", y = "Average Price")
} else {
  print("No 'Län' column found in the dataset.")
}

# Linear Regression Analysis: Predicting Price based on Mileage and Year
# Check if relevant columns exist in the dataset
if (all(c("Pris", "År", "Miltal") %in% colnames(df))) {
  # Remove rows with missing values in relevant columns
  df_reg <- df %>%
    filter(!is.na(Pris) & !is.na(År) & !is.na(Miltal))
  
  # Fit the linear regression model
  model <- lm(Pris ~ År + Miltal, data = df_reg)
  
  # Display regression results
  summary(model)
  
  # Visualization: Regression plot of Price vs. Mileage
  ggplot(df_reg, aes(x = Miltal, y = Pris)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    theme_minimal() +
    labs(title = "Price vs. Mileage Regression", x = "Mileage", y = "Price")
  
} else {
  print("Required columns ('Pris', 'År', 'Miltal') not found in the dataset.")
}
