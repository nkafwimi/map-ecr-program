########################################
###    Data Wrangling Session ##########
###    March 17, 2025         ##########
###    Author: Neema Kafwimi  ##########
########################################

# load libraries 
library(tidyverse)        # for data wrangling 


#load data
routine_data <- read.csv("data raw/edited_routine_data.csv")

# EDA
head(routine_data)
tail(routine_data)
View(routine_data)
str(routine_data)          # data structure
names(routine_data)
summary(routine_data)
class(routine_data)

# selecting columns
df1 <- dplyr::select(routine_data, adm1, month, conf_u5)    

# filtering rows
filter(routine_data, month == "Jan" & conf_u5 > 200)
df2 <- filter(routine_data, adm1 == "East" & test_rdt_u5 > 100) # filtering based on adm1 and test_rdt_u5
df3 <- filter(routine_data, adm1 == "East" | test_rdt_u5 > 100) # filtering based on adm1 or test_rdt_u5

df4 <- routine_data %>% select(adm1, test_u5) %>% filter(test_u5 > 100)

df5 <- routine_data %>% select(adm1, month, conf_u5) %>% filter(conf_u5 > 100 & month == "Apr")

# group by function
routine_data %>% group_by(month) %>% summarize(mean_conf_u5 = mean(conf_u5), 
                                               total_conf_u5 = sum(conf_u5)) 

# group by and filter function
total_confU5_Apr <- routine_data %>% select(adm1, month, conf_u5) %>%
  filter(month == "Apr") %>%
  summarize(total_conf_U5 = sum(conf_u5))

df6 <- routine_data %>% select(adm1, month, conf_u5) %>%
  filter(month == "Apr") %>%
  group_by(adm1) %>%                                         # returns one value when grouped by month                                
  summarize(total_conf_U5 = sum(conf_u5))

df7 <- routine_data %>% mutate(prop_conf_u5 = test_u5 / conf_u5)
drop_na(df7)
  


