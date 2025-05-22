library(tidyverse)
library(readr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week4") 
dir()

Townsend_aggregated <- read_csv("Townsend_aggregated.csv")
Manufacturing_aggregated <- read_csv("Manufacturing_aggregated.csv")

#####################
#### Missingness ####
#####################

Townsend_aggregated <- Townsend_aggregated %>% mutate(zero_LSOA = total_residents == 0, zero_households = total_households == 0, small_LSOA = total_residents > 0 & total_residents < 500, large_LSOA = total_residents >5000) 

Townsend_aggregated %>%
  group_by(year) %>% summarize(count_zero = sum(zero_LSOA), prop_zero = mean(zero_LSOA), count_no_households = sum(zero_households), count_small = sum(small_LSOA), count_large = sum(large_LSOA))

### Geographical location of problematic regions (revised once weight dfs were corrected)

library(sf)
library(corrplot)
library(tmap)
library(spdep)
library(spgwr)
library(RColorBrewer)

# This file contains the boundary information
Birmingham_2021 <- read_sf("~/R/Birmingham&Walsall/Week3/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% 
  filter(str_detect(LSOA21NM, "Birmingham")) 

var <- "zero_LSOA"

#This table lists the LSOA21 areas with zero residents
temp_data <- Townsend_aggregated %>% group_by(lsoa21) %>% summarize(occurances = sum(!!sym(var))) %>% arrange(desc(occurances)) 

EDA_Graph <- function(var){
#This is a .shp file of all of the regions in the Townsend data for each year
 temp_data <- Townsend_aggregated %>% group_by(lsoa21) %>% summarize(occurances = sum(!!sym(var)))
 Check_missing <- Birmingham_2021 %>% right_join(temp_data, by = c("LSOA21CD" = "lsoa21")) 
## Visualisation - zeros are present in LSOAs not in Birmingham for 2021 - 2001. 
map <- tm_shape(Check_missing) + tm_fill("occurances", fill.scale = tm_scale_categorical("brewer.purples")) + tm_borders(fill_alpha=0.4)
return(map)
}

EDA_Graph("zero_LSOA")
EDA_Graph("small_LSOA")
EDA_Graph("zero_households")
EDA_Graph("large_LSOA")

## Distribution of population by year

ggplot(Townsend_aggregated, aes(total_residents)) + geom_histogram() + facet_wrap(~year, nrow = 2)
ggplot(Townsend_aggregated, aes(total_households)) + geom_histogram() + facet_wrap(~year, nrow = 2)

