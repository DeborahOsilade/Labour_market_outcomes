
#importing dataset 

library(readxl)
library(ggplot2)
library(tidyverse)
library(janitor)

labour_rates <- read_excel("labour rates.xlsx")


#headers as first row
colnames(labour_rates) <-  paste0("V", seq_along(labour_rates))

#delete the first 3 rows                                 
labour_rates <- labour_rates %>% slice(-c(1:3))                                

#transposed data
labour_rates <-  t(labour_rates)

#create a table
labour_rates <-  as.tibble(labour_rates)

#removed blank rows
labour_rates <-  labour_rates[-2, , drop = FALSE] 
labour_rates <-  labour_rates[-14, , drop = FALSE] 
labour_rates <-  labour_rates[,-4 , drop = FALSE] 
labour_rates <-  labour_rates[-c(13, 14), ]
labour_rates <-  labour_rates[-13, , drop = FALSE] 
labour_rates <-  labour_rates[,-c(41:45) , drop = FALSE] 
 
attach(labour_rates) # attached columns from the data
colnames(labour_rates) #viewed column names 

#used first row as a header
colnames(labour_rates) <- labour_rates[1, ] 

#remove first row
labour_rates <- labour_rates %>% 
                               row_to_names(row_number = 1)
#filter where sex is not Total
labour_rates <-  labour_rates %>% 
                               filter(Sex != "Total") 
#delete all empty columns 
labour_rates <- labour_rates %>% 
                                 remove_empty(which = "cols" )

#turned  data into a data frame
labour_rates <- as.data.frame(labour_rates)

attach(labour_rates)

#pivoted other columns into country and rates
labour_rates <- labour_rates %>%
  pivot_longer(
    cols = -c(`Time period`, Sex, `Place of birth`),  # Use back ticks for spaces
    names_to = "country",  # New column for previous column names
    values_to = "rates"  # New column for values
  )

#Removed missing rows under rates column
labour_rates <-  labour_rates %>% 
  filter(!is.na(rates))

#checking data distribution
summary(labour_rates)

#changed rates column to numeric
labour_rates$rates <-  as.numeric(labour_rates$rates)

#round rates column 
labour_rates$rates <-round(labour_rates$rates,1)

labour_rates <- janitor::clean_names(labour_rates)

