library(tidyverse)
library(ggplot2)
library(zipcodeR)
library(choroplethrZip)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(usmap)
library(ggstatsplot)
library(GGally)

#read in new data with zip code data
full_data <- read.table("zip_code_market_tracker.tsv000", header = TRUE, sep = "\t")
full_data$period_begin <- as.Date(full_data$period_begin)
full_data$period_end <- as.Date(full_data$period_end)
full_data$year <- year(full_data$period_begin)
full_data$month <- month(full_data$period_begin)
df_zc <- full_data



df_zc <- full_data %>% select("period_begin", "period_end", "table_id", "state", "state_code", 
                              "property_type", "median_sale_price", "median_list_price", 
                              "median_ppsf", "median_list_ppsf", "homes_sold", 
                              "new_listings", "inventory", "months_of_supply", 
                              "avg_sale_to_list", "sold_above_list", "off_market_in_two_weeks", 
                              "parent_metro_region", "region", "parent_metro_region", "year", "month", "price_drops") %>%
  filter(property_type == "All Residential") %>%
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
df_zc$urban <- df_zc$urban$Urban


write.csv(df_zc, file = "USHouse_zip.csv")

data <- df_zc
#data$period_begin <- as.Date(data$period_begin)
#data$urban <- data$urban$Urban 
#data$urban <- as.factor(data$urban)
data <- filter(data, data$urban != "NA")

#Inventory vs Time for Rural vs Urban Areas (#8)
ggplot(data, aes(x = period_begin, y = median_sale_price, group = urban, color = urban)) +
  geom_line() + 
  ggtitle("Inventory vs Time for Rural vs Urban Areas") + xlab("Date") + ylab("Median Sale Price") + 
  facet_wrap(~urban, nrow = 3)

#Plot of yearly states with the most yearly price change (#7)
tmp <- df_zc

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

tmp <- rbind (head(tmp, 3),tail(tmp, 3))


#improvements: 
#add color, add counts on top/side of the bar
#order the data
#separate appreciation/depreciation into one plot

tmp$state <- factor(tmp$state,
                    levels = c("New Mexico", "Alaska", "Virginia", "Utah", "Nevada", "Idaho"))

#This one
ggplot(tmp, aes(x = mean_yoy_price_change, 
                y = state, fill = state)) +
  geom_bar(stat = "identity") + xlab("Mean Yearly Price Change") + 
  ggtitle("States with the Highest Annual % Housing Price Change") +
  geom_text(aes(label = round(mean_yoy_price_change, digits = 1)), vjust = 0) 

ggplot(tmp, aes(x = mean_yoy_price_change, 
                y = state, color = state)) +
  geom_bar(stat = "identity") + xlab("Mean Yearly Price Change") + 
  ggtitle("States with the Highest Annual % Housing Price Change") +
  geom_text(aes(label = round(mean_yoy_price_change, digits = 1)), vjust = 0) 


ggplot(tmp, aes(x = state, y = mean_yoy_price_change)) +
  geom_bar(stat = "identity") + ylab("Mean Yearly Price Change") + 
  ggtitle("States with the Highest Annual % Housing Price Change")


# Time series of time vs percentage sold above asking price for metro areas (9)
# Facet Wrap
metros <- subset(data, parent_metro_region == "Los Angeles, CA" | 
                   parent_metro_region == "New York, NY" |
                   parent_metro_region == "Houston, TX" |
                   parent_metro_region == "San Fransisco, CA" |
                   parent_metro_region == "Chicago, IL" |
                   parent_metro_region == "Atlanta, GA")

ggplot(metros, aes(x = period_begin, y = sold_above_list, 
                   group = parent_metro_region, color = parent_metro_region)) +
  geom_line() + 
  ggtitle("% Sold Above List Price Over Time") 




#2020 Census State Population Data
#https://www.census.gov/data/tables/2020/dec/2020-apportionment-data.html
#link to data:
#https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment-2020-table01.xlsx

#2010-2019 Census State Population Data:
#https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295
#link to data:
#https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx


#differences were calculated in excel

state_pop <- read.csv("Yearly State Population.csv", header = TRUE)
flows <- state_pop[13:22]
flows$state <- state_pop$state

flows_till_19 <- flows[1:10]
flows$avg_yearly_migration_till_19 <- rowMeans(flows_till_19)

Covid_migration_flows <- state_pop %>% 
  summarise(state, 
            Change = X2020.00 - X2019.00)


plot_usmap(data = Covid_migration_flows, values = "Change", color = "red") + 
  scale_fill_continuous(
    low = "blue", high = "red", name = "Difference", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  ggtitle("Change in Yearly State Migration in 2020 vs Average of 2010-2019")



metros2 <- subset(df_zc, parent_metro_region == "Los Angeles, CA" | 
                    parent_metro_region == "New York, NY" |
                    parent_metro_region == "Houston, TX" |
                    parent_metro_region == "San Fransisco, CA" |
                    parent_metro_region == "Chicago, IL" |
                    parent_metro_region == "Atlanta, GA")


#Scatter plot median list ppsf vs homes_sold by metro region

ggplot(metros2, aes(x = median_list_ppsf, y = inventory, color = parent_metro_region, na.rm = TRUE)) +
  geom_point()


ggplot(metros2, aes(x = median_list_ppsf, y = homes_sold, color = parent_metro_region, na.rm = TRUE)) +
  geom_point()


#boxplots of metro regions median_list_ppsf 
ggplot(metros2, aes(x = parent_metro_region, y = median_list_ppsf)) +
  geom_boxplot()



bp_data <- subset(full_data, period_begin > "2018-12-31" & period_begin < "2020-12-31")
bp_data$year <- year(bp_data$period_begin)

ggplot(bp_data, aes(x = median_list_ppsf)) +
  geom_boxplot() + facet_wrap(~year, nrow = 2)

#box plot of median list ppsf by city 2019 vs 2020 
metros3 <- filter(df_zc, year > 2018, year < 2021)
metros3 <- subset(metros3, parent_metro_region == "Los Angeles, CA" | 
                    parent_metro_region == "New York, NY" |
                    parent_metro_region == "Houston, TX" |
                    parent_metro_region == "San Fransisco, CA" |
                    parent_metro_region == "Chicago, IL" |
                    parent_metro_region == "Atlanta, GA")

#Median PPSF 2019 vs 2020 by city
ggplot(metros3, aes(x = median_list_ppsf, y = parent_metro_region)) +
  geom_boxplot() + 
  facet_wrap(~year) +
  ggtitle("Median PPSF 2019 vs 2020")


#create new df with just April 2019/April 2020
metros4 <- filter(metros3, month == "5")
ggplot(metros4, aes(x = inventory, y = parent_metro_region)) +
  geom_boxplot() + 
  facet_wrap(~year) +
  ggtitle("Inventory May 2019 vs May 2020")


ggplot(metros2, aes(x = median_list_ppsf, y = off_market_in_two_weeks, color = parent_metro_region)) +
  geom_point(alpha = 0.8)


#off market in 2 weeks 2019 vs 2020
ggplot(metros3, aes(x = parent_metro_region, y = off_market_in_two_weeks)) +
  geom_boxplot() +
  facet_wrap(~year)



#pairs graph

pairs_df <- df_zc %>%
  select(median_list_ppsf, months_of_supply, 
         avg_sale_to_list, off_market_in_two_weeks, new_listings, 
         inventory)

sample_pairs <- sample_n(pairs_df, 20000)
#sample_pairs$urban <- sample_pairs$urban$Urban 
#sample_pairs <- filter(sample_pairs, sample_pairs$urban != "NA")
#sample_pairs$urban <- as.factor(sample_pairs$urban)

pairs(sample_pairs[1:4], 
      col = c("red", "cornflowerblue", "purple")[sample_pairs$urban],
      pch = c(8, 18, 1)[sample_pairs$urban]) 

ggpairs(sample_pairs[1:4], aes(color = sample_pairs$urban))


Covid_migration_flows$state <- state.abb[match(Covid_migration_flows$state,state.name)]


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

mtcars %>%
  group_by(cyl) %>%
  mutate(outlier = ifelse(is_outlier(drat), drat, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(cyl), y = drat)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)


Covid_migration_flows <- Covid_migration_flows %>%
  mutate(outlier = ifelse(is_outlier(Change), 1, as.numeric(0)))

ggplot(Covid_migration_flows, aes(x = Change, y = state, color = outlier)) +
  geom_point() +
  ggtitle("Change in State Population April 2020 vs April 2019") +
  ylab("State")


#pending sales

#box plot of median list ppsf by city 2019 vs 2020 
metros3 <- filter(full_data, year > 2018, year < 2021)
metros3 <- subset(metros3, parent_metro_region == "Los Angeles, CA" | 
                    parent_metro_region == "New York, NY" |
                    parent_metro_region == "Houston, TX" |
                    parent_metro_region == "San Fransisco, CA" |
                    parent_metro_region == "Chicago, IL" |
                    parent_metro_region == "Atlanta, GA")


ggplot(metros3, aes(x = period_begin, y = pending_sales, color = parent_metro_region)) + 
  geom_point() + 
  facet_wrap(~year, nrow = 2, scales = "free_x")


ggplot(metros3, aes(x = period_begin, y = pending_sales, color = urban)) + 
  geom_line() + 
  facet_wrap(~year, nrow = 2, scales = "free_x")
