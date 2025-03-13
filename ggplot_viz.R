attach(labour_rates)

library(summarytools)

#View summary of data
print(dfSummary(labour_rates))

#newdata as top countries 
top_countries <- labour_rates

#______________________________Visualization__________________________________________________________________


# identify countries with high employemnt rates

summed_data <- top_countries %>%
  group_by(country) %>%
  summarise(Total_Rate = sum(rates, na.rm = TRUE))

topp_countries <- summed_data %>%
  arrange(desc(Total_Rate)) %>%
  slice_head(n = 10)

# Plotting the data
ggplot(topp_countries, aes(x = reorder(country, Total_Rate), y = Total_Rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with Highest Employment Rates",
    x = "Country",
    y = "Employment Rate"
  ) +
  theme_minimal()

#_________________________________________________________________________________________________
# Visualization: Employment rate comparison
ggplot(labour_rates, aes(x = country , y = rates, fill = `Place of birth`)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Employment Rate by Country and Birth Status", 
       x = "Country", y = "Employment Rate (%)")


# Boxplot to see distribution
ggplot(labour_rates, aes(x = `Place of birth`, y = rates, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Distribution of Employment Rate by Birth Status", 
       x = "Birth Status", y = "Employment Rate (%)")




#employment gap 

# Calculate Employment Gaps (Foreign-born - Native-born)
employment_gap <- labour_rates %>%
  select(`Time period`, Sex, `Place of birth`, country, rates) %>%
  pivot_wider(
    names_from = `Place of birth`,
    values_from = rates
  ) %>%
  mutate(
    Employment_Gap = `Foreign-born` - `Native-born`
  ) %>%
  drop_na(Employment_Gap) # Remove rows where we couldn't calculate the gap

# View the result
print(employment_gap)


# Plotting the Employment Gap
ggplot(employment_gap, aes(x = reorder(country, Employment_Gap), y = Employment_Gap, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Employment Gap Between Foreign-born and Native-born Workers",
    x = "Country",
    y = "Employment Gap (Foreign-born - Native-born)",
    fill = "Sex"
  )


# Define thresholds for highlighting
high_threshold <- 5   # You can adjust this threshold
low_threshold <- -5    # You can adjust this threshold

# Categorize gaps
employment_gap$Gap_Category <- ifelse(
  employment_gap$Employment_Gap > high_threshold, "Positive Gap",
  ifelse(employment_gap$Employment_Gap < low_threshold, "Negative Gap", "Small Gap")
)

# Plot with Highlighting
ggplot(employment_gap, aes(x = reorder(country, Employment_Gap), y = Employment_Gap, fill = Gap_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Positive Gap" = "darkgreen", "Negative Gap" = "darkblue", "Small Gap" = "gray")) +
  labs(
    title = "Employment Gap Between Foreign-born and Native-born Workers (2022 - 2023)",
    x = "Country",
    y = "Employment Gap (Foreign-born - Native-born)",
    fill = "Gap Category"
  )


# Group data by Country, Year, and Sex to compare years
employment_gap_yearly <- labour_rates %>%
  filter(Sex != "Total") %>%
  group_by(country, `Time period`, Sex) %>%
  summarise(
    Foreign_rate = sum(rates[Place_of_birth == "Foreign-born"], na.rm = TRUE),
    Native_rate = sum(rates[Place_of_birth == "Native-born"], na.rm = TRUE)
  ) %>%
  mutate(Employment_Gap = Foreign_rate - Native_rate) %>%
  ungroup()

# Plot comparing gaps across years
ggplot(employment_gap_yearly, aes(x = reorder(country, Employment_Gap), y = Employment_Gap, fill = as.factor(`Time period`))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Comparison of Employment Gaps Across 2022 and 2023",
    x = "Country",
    y = "Employment Gap (Foreign-born - Native-born)",
    fill = "Year"
  )