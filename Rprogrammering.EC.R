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

# Example Analysis: Descriptive Statistics
summary(df)

# Example Analysis: Group by 'Märke' and calculate average price
average_price_by_brand <- df %>%
  group_by(Märke) %>%
  summarise(Average_Price = mean(Pris, na.rm = TRUE))

print(average_price_by_brand)

# Example Analysis: Count the number of cars by fuel type
fuel_type_count <- df %>%
  group_by(Bränsle) %>%
  summarise(Count = n())

print(fuel_type_count)

# Example Analysis: Group by 'Land' and calculate the total number of cars and average price
if ("Län" %in% colnames(df)) {
  analysis_by_country <- df %>%
    group_by(Län) %>%
    summarise(
      Total_Cars = n(),
      Average_Price = mean(Pris, na.rm = TRUE)
    )
  
  print(analysis_by_country)
  
  # Example Visualization: Bar plot of average price by country
  ggplot(analysis_by_country, aes(x = Län, y = Average_Price)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Average Price by Country", x = "Country", y = "Average Price")
} else {
  print("No 'Län' column found in the dataset.")
}