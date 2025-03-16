attach(labour_rates)

library(summarytools)

#View summary of data
print(dfSummary(labour_rates))

#new data as top countries 
top_countries <- labour_rates

#_______________Visualization__________________________________________________________________


# 1. identify countries with high employment rates

summed_data <- top_countries %>%
  group_by(country,sex,time_period,place_of_birth) %>%
  summarise(Total_Rate = sum(rates, na.rm = TRUE))

#creating a function for labour_rate_plot

Firstplot <-  function(sex, year,birth,top_n) {
  #creating usable data 
  data_for_plot1 <- summed_data%>%
                                  filter(sex == sex, time_period == year,place_of_birth == birth ) %>% 
                                  arrange(desc(Total_Rate)) %>% head(as.numeric(top_n))  %>% 
                                  arrange(desc(top_n))
  #creating plot
 ggplot(data_for_plot1, aes(x = reorder(country,Total_Rate,), y = Total_Rate)) +
    geom_bar(stat = "identity", fill = "#1d2f6f") +
   geom_text(aes(label = paste0(Total_Rate, "%")),    # Adds labels to bars
             hjust = -0.2,                         # Adjust label position (to the right of the bars)
             size = 4,                             # Font size of the labels
             color = "black") +                    # Label color
    coord_flip() +
    labs(title = paste("Labour Rates for",birth, sex, "in", year),
         x = "Country",
         y = "Rates") +
    theme(panel.background = element_blank(),
          axis.title.x = element_text(size = 12),  # Increased font size for x-axis title
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 14),   # Increased font size for x-axis labels
          axis.text.y = element_text(size = 14))  # Increased font size for y-axis title
   
}

#creating a plot for new tabpanel
combinedplot <-  function(sex, year,top_n) {
  #creating usable data 
  data_for_plot5 <- summed_data%>%
    filter(sex == sex, time_period == year) %>% 
    arrange(desc(Total_Rate)) %>% head(as.numeric(top_n))  %>% 
    arrange(desc(top_n)) %>%  # Select the top 'n' countries based on Total_Rate
    mutate(Top_colour = ifelse(Total_Rate == max(Total_Rate), "pink", "grey"))
  # Select top 'n' countries based on Total_Rate
  
  #creating plot
  ggplot(data_for_plot5, aes(x = reorder(country,Total_Rate,), y = Total_Rate, fill = Top_colour)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Labour Rates for",birth, sex, "in", year),
         x = "Country",
         y = "Rates") +
    theme(panel.background = element_blank())
}



# 2. Identify employment gap between

# Calculate Employment Gaps (Foreign-born - Native-born)
employment_gap <- labour_rates %>%
  select(time_period, sex, place_of_birth, country, rates) %>%
  pivot_wider(
    names_from = place_of_birth,
    values_from = rates
  ) %>%
  mutate(
    Employment_Gap = `Foreign-born` - `Native-born`
  ) %>%
  drop_na(Employment_Gap) # Remove rows where we couldn't calculate the gap

# View the result
print(employment_gap)


# Plotting the Employment Gap

secondplot <-  function(gap,yearr,top_nn) {
  #creating usable data 
  data_for_plot2 <- employment_gap %>%
                                    filter(sex == gap,time_period == yearr) %>% 
                                    arrange(desc(Employment_Gap)) %>% 
                                    head(as.numeric(top_nn))  %>% 
                                     arrange(desc(top_nn)) # Select top 'n' countries based on Total_Rate
  
  #creating plot
  ggplot(data_for_plot2, aes(x = reorder(country, Employment_Gap), y = Employment_Gap, 
                             fill = ifelse(Employment_Gap > 0, "Positive gap", "Negative gap"))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(Employment_Gap, 1)),    # Adds labels to bars
              hjust = -0.2,                         # Adjust label position (to the right of the bars)
              size = 4,                             # Font size of the labels
              color = "black") +                    # Label color
    coord_flip()  +
    scale_fill_manual(values = c("Positive gap" = "#003f91", "Negative gap" = "#5da9e9")) +
    labs(
      title = paste( "Employment Gap Between", gap, "Workers"),
      x = "Country",
      y = "Employment Gap (Foreign-born - Native-born)",
      caption = "A positive gap indicates the Foreign-born has a higher rate, while a negative gap shows the opposite"
      )+
    theme(panel.background = element_blank(),
          axis.title.x = element_text(size = 12),  # Increased font size for x-axis title
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 14),   # Increased font size for x-axis labels
          axis.text.y = element_text(size = 14),
          legend.position = "bottom",                       # Position legend at the bottom
          legend.title = element_blank())                    # Remove legend title
}


