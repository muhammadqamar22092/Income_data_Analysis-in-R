setwd("C:/Users/hp/Documents/R_Lab_Assignmnet")
# Load the dataset
income_data <- read.csv("Income_Data.csv")
str(income_data)

# Calculate the number of rows and columns
num_rows <- nrow(income_data)
num_columns <- ncol(income_data)

# Print the result
print(paste("Number of Rows:", num_rows))
print(paste("Number of Columns:", num_columns))

# Converting the specified variables to factor variables
income_data$state <- as.factor(income_data$state)
income_data$county <- as.factor(income_data$county)
income_data$county_FIPS <- as.factor(income_data$county_FIPS)

# Printing the summary of the dataset
summary_data <- summary(income_data)

# Display the summary
print(summary_data)

summary(income_data$state)
summary(income_data$per_capita_personal_income_2020)
library(dplyr)
# Filter numeric columns and calculate statistics
numeric_data <- income_data[sapply(income_data, is.numeric)]
summary_stats <- sapply(numeric_data, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
# Print summary statistics
print(summary_stats)

# Calculating the population
# population = (number of people with a bachelor's degree / percentage of people with a bachelor's degree) * 100
income_data <- income_data %>%
  mutate(Population = (bachelor_degree_numbers_2020 / bachelor_degree_percentage_2020) * 100)

# Printing a summary of the "Population" column
population_summary <- summary(income_data$Population)
print(population_summary)


library(ggplot2)
# Calculate the average percentage of individuals with a bachelor's degree by state
state_degrees <- income_data %>%
  group_by(state) %>%
  summarize(
    Total_Degree_Holders = sum(bachelor_degree_numbers_2020, na.rm = TRUE),
    Total_Population = sum(bachelor_degree_numbers_2020 / bachelor_degree_percentage_2020 * 100, na.rm = TRUE)
  ) %>%
  mutate(Average_Percentage = (Total_Degree_Holders / Total_Population) * 100) %>%
  arrange(desc(Average_Percentage))
print(state_degrees$Average_Percentage)
# Plotting the bar chart
ggplot(state_degrees, aes(x = reorder(state, -Average_Percentage), y = Average_Percentage)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Percentage of Bachelor's Degree Holders by State in 2020", x = "State", y = "Average Percentage")

# Plotting the scatter plot with a scatter plot line
ggplot(income_data, aes(x = bachelor_degree_percentage_2020, y = per_capita_personal_income_2020)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Add scatter plot line with formula
  labs(title = "Scatter Plot of Per Capita Personal Income 2020 vs Bachelor's Degree Percentage 2020",
       x = "Bachelor's Degree Percentage 2020",
       y = "Per Capita Personal Income 2020") +
  theme_minimal()



# Calculate the total population for each state
top_10_states <- income_data %>%
  group_by(state) %>%
  summarize(Total_Population = sum(bachelor_degree_numbers_2020 / (bachelor_degree_percentage_2020 / 100), na.rm = TRUE)) %>%
  arrange(desc(Total_Population)) %>%
  head(10)
print(top_10_states)

# Filter data for only the top 10 states
top_counties_by_income <- income_data %>%
  filter(state %in% top_10_states$state) %>%
  group_by(state) %>%
  filter(per_capita_personal_income_2020 == max(per_capita_personal_income_2020)) %>%
  select(state, county, per_capita_personal_income_2020)
print(top_counties_by_income)

