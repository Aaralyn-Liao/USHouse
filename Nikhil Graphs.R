library(tidyverse)
library(ggplot2)
library(zipcodeR)
library(choroplethrZip)
library(stringr)
library(lubridate)


#read in new data with zip code data
full_data <- read.table("zip_code_market_tracker.tsv000", header = TRUE, sep = "\t")

df_zc <- full_data



df_zc <- df_zc %>% select("period_begin", "period_end", "table_id", "state", "state_code", 
                          "property_type", "median_sale_price", "median_list_price", 
                          "median_ppsf", "median_list_ppsf", "homes_sold", 
                          "new_listings", "inventory", "months_of_supply", 
                          "avg_sale_to_list", "sold_above_list", "off_market_in_two_weeks", 
                          "parent_metro_region", "region", "parent_metro_region") %>%
  mutate(period_begin=lubridate::ymd(period_begin))%>%
  mutate(period_end=lubridate::ymd(period_end))%>%
  filter(property_type=="All Residential",
         period_begin>=as.Date("2018-01-01"),
         period_end>=as.Date("2018-04-04"))%>%
  arrange(desc(period_begin))

#create a column with just zipcode
df_zc$zipcode <- str_sub(df_zc$region, start= -5)


#library from here https://www.rdocumentation.org/packages/choroplethrZip/versions/1.5.0

#read in data of zip codes
data("zip_code_db")

#definition of urban, rural and suburban:
#https://medium.com/pew-research-center-decoded/evaluating-what-makes-a-u-s-community-urban-suburban-or-rural-159f9d082842

#create new column that is household density per sq mile
zip_code_db$households_per_sq_mi <- zip_code_db$housing_units/zip_code_db$land_area_in_sqmi

#create new column that uses ^ to classify as urban and rural
zip_code_db <- zip_code_db %>% 
  mutate(Urban = 
           case_when(households_per_sq_mi > 1314 ~ "urban",
                     households_per_sq_mi < 106 ~ "rural",
                     households_per_sq_mi < 1314 & households_per_sq_mi > 106 ~ "suburban",
                     TRUE ~ "NA" 
                     ))

#In the original df now join by the two values of zipcode to classify redfin data as Urban, Rural and Suburban
small_df <- zip_code_db %>% select(zipcode, Urban)
df_zc$urban <- left_join(df_zc, small_df)

data <- sample_n(df_zc, 1000)
data$period_begin <- as.Date(data$period_begin)
data$urban <- data$urban$Urban
data$urban <- as.factor(data$urban)


#Inventory vs Time for Rural vs Urban Areas (#8)
ggplot(data, aes(x = period_begin, y = median_sale_price, group = urban, color = urban)) +
  geom_line() + 
  ggtitle("Inventory vs Time for Rural vs Urban Areas") + xlab("Date") + ylab("Median Sale Price")

#Plot of yearly states with the most yearly price change (#7)
tmp <- data

tmp$year <- year(tmp$period_begin)

tmp <- tmp %>% group_by(state, year) %>%
  summarise(sale_price = mean(median_sale_price)) %>%
  arrange(state, year) %>%
  summarise(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sale_price - lag(sale_price), # Difference in route between years
         yearly_appreciation = (Diff_growth / Diff_year)/sale_price * 100) %>%
  summarise(mean_yoy_price_change = mean(yearly_appreciation, na.rm = TRUE)) %>%
  arrange(mean_yoy_price_change)

tmp <- na.omit(tmp)

tmp <- rbind (head(tmp, 5),tail(tmp, 5))

ggplot(tmp, aes(x = mean_yoy_price_change, y = state)) +
  geom_bar(stat = "identity") + xlab("Mean Yearly Price Change") + 
  ggtitle("States with the Highest Annual % Housing Price Change")

ggplot(tmp, aes(x = state, y = mean_yoy_price_change)) +
  geom_bar(stat = "identity") + ylab("Mean Yearly Price Change") + 
  ggtitle("States with the Highest Annual % Housing Price Change")

  
# Time series of time vs percentage sold above asking price for metro areas (9)
metros <- subset(data, parent_metro_region == "Los Angeles, CA" | 
              parent_metro_region == "New York, NY" |
              parent_metro_region == "Houston, TX" |
              parent_metro_region == "San Fransisco, CA" |
              parent_metro_region == "Chicago, IL" |
              parent_metro_region == "Atlanta, GA")

ggplot(metros, aes(x = period_begin, y = sold_above_list, 
                 group = parent_metro_region, color = parent_metro_region)) +
  geom_line()

