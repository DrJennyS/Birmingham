library(ggplot2)
library(tidyverse)
library(readr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3") 

weights_1981 <- read_csv("weights_1981.csv") 
weights_1971 <- read_csv("weights_1971.csv")

setwd("~/R/Birmingham&Walsall/Week3/1981") 
dir() #check files in working directory

data_1981 <- read_csv("81sas09ews_4.csv") %>% mutate(code = str_extract(zoneid, pattern = "\\d{6}$"))

weights_1981 <- weights_1981 %>% mutate(code = str_extract(ED81CD, pattern = "\\d{6}$"))

left_join(weights_1981, data_1981)

filter(data_1981, zoneid == "01AAAA01") # This means I need to change my look up file

#### STOP YOU NOW DEFINITELY NEED TO PUT EVERYTHING ON github ####