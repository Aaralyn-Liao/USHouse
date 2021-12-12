library(tidyverse)

df <- read.table("/Users/david/Downloads/state_market_tracker-2.tsv000", sep = "\t", header = TRUE)

house <- df %>% select("period_begin", "period_end", "table_id", "state", "state_code", "property_type", "median_sale_price", "median_list_price", "median_ppsf", "median_list_ppsf", "homes_sold", "new_listings", "inventory", "months_of_supply", "avg_sale_to_list", "sold_above_list", "off_market_in_two_weeks", "parent_metro_region")%>%
  mutate(period_begin=lubridate::ymd(period_begin))%>%
  mutate(period_end=lubridate::ymd(period_end))%>%
  filter(property_type=="All Residential",
         period_begin>=as.Date("2018-01-01"),
         period_end>=as.Date("2018-01-31"))%>%
  arrange(desc(period_begin))

# plot1
library(zoo)

tmp12 = house %>% group_by(state_code) %>%
  summarize(mean_supply = mean(months_of_supply)) %>%
  arrange(desc(mean_supply)) %>% 
  select(state_code)

names12 = unlist(lapply(tmp12$state_code, as.character))

df12 = house %>% group_by(period_begin,state_code) %>%
  summarize(mean_supply = mean(months_of_supply)) %>%
  mutate(period_begin = format(as.Date(period_begin), "%Y-%m"))%>%
  mutate(period_begin = as.yearmon(period_begin)) %>%
  mutate(state_code = factor(state_code, levels = names12)) %>%
  rename(time = period_begin)


library(ggplot2)
library(plotly)
p12 = ggplot(df12, aes(x=time, y=mean_supply)) +
  geom_line(aes(color=state_code)) +
  xlab("") + ylab("Months of Supply") +
  ggtitle("Time series plots of Months of Supply and \noff-market in two weeks for each state", )+
  theme(axis.title=element_text(size=10))

p12 = ggplotly(p12)

tmp13 = house %>% group_by(state_code) %>%
  summarize(mean_offmarket = mean(off_market_in_two_weeks)) %>%
  arrange(desc(mean_offmarket)) %>% 
  select(state_code)

names13 = unlist(lapply(tmp13$state_code, as.character))

df13 = house %>% group_by(period_begin,state_code) %>%
  summarize(mean_offmarket = mean(off_market_in_two_weeks)) %>%
  mutate(period_begin = format(as.Date(period_begin), "%Y-%m"))%>%
  mutate(period_begin = as.yearmon(period_begin)) %>%
  mutate(state_code = factor(state_code, levels = names13)) %>%
  rename(time = period_begin)

p13 = ggplot(df13, aes(x=time, y=mean_offmarket)) +
  geom_line(aes(color=state_code)) +
  xlab("Time") + ylab("Off-market in two weeks")

p13 = ggplotly(p13)

fig14 = subplot(p12, p13, nrows=2, titleX = T, titleY = T) %>%
  layout(showlegend = F)

fig14

# plot2

house14 = house %>% 
  filter(as.numeric(format(period_begin, "%Y")) == 2019 | as.numeric(format(period_begin, "%Y")) == 2020 | as.numeric(format(period_begin, "%Y")) == 2021) %>%
  mutate(period_begin = substr(period_begin,1,4)) %>%
  group_by(period_begin, state_code) %>%
  summarize(mean_sale_ppsf = mean(median_ppsf)) %>%
  pivot_wider(names_from = period_begin, values_from = mean_sale_ppsf, names_glue = "{.value}_{period_begin}") %>%
  mutate(diff20 = mean_sale_ppsf_2020 - mean_sale_ppsf_2019, diff21 = mean_sale_ppsf_2021-mean_sale_ppsf_2020) %>%
  rename(state = state_code)


library(usmap)
library(plotly)

p14 = plot_usmap(data = house14, values = "diff20", labels = T)+
  scale_fill_gradientn(
    name = "Average ppsf",
    colors = c("red", "yellow", "forestgreen"),
    breaks = c(min(house14$diff20), 0, max(house14$diff20)),
    label = scales::comma) + 
  labs(title = "US heatmap of average sale PPSF")+
  theme(legend.position = "right")

#p14 = ggplotly(p14)
p14


#plot3
tmp1 = house %>%
  filter(as.numeric(format(period_begin, "%Y")) == 2020) %>%
  filter(period_begin < "2020-07-01") %>%
  arrange(desc(period_begin), state_code) %>%
  select(period_begin, state, off_market_in_two_weeks)

tmp2 = house %>%
  filter(as.numeric(format(period_begin, "%Y")) == 2019) %>%
  filter(period_begin < "2019-07-01") %>%
  arrange(desc(period_begin), state_code) %>%
  select(period_begin, state, off_market_in_two_weeks)

tmp1 = tmp1 %>%
  rename(off_market_in_two_weeks_2020 = off_market_in_two_weeks) %>%
  mutate(off_market_in_two_weeks_2019 = tmp2$off_market_in_two_weeks) %>%
  mutate(diff = off_market_in_two_weeks_2020 -off_market_in_two_weeks_2019)

ggplot(tmp1 , aes(diff, reorder(state, diff, median)))+
  geom_boxplot()+
  theme_grey(base_size = 25) +
  geom_vline(xintercept = 0, color="red",size=1.2)+
  labs(x="Difference of proportion of off-market in two weeks between 2020 & 2019", y="States", 
       title = "Boxplot of Difference of proportion of off-market in two weeks \nbetween 2020 & 2019 for each state")





