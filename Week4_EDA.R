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

#######################################
#### Distribution of each variable ####
#######################################

ggplot(Townsend_aggregated, aes(x = total_Active, y = unemployed)) + geom_point() + facet_wrap(~year, nrow = 3)

Townsend_aggregated %>% filter(year != "1971") %>%
  #ggplot(aes(x = total_households, y = Overcrowded)) +
  #ggplot(aes(x = total_households, y = Owned)) +
  ggplot(aes(x = total_households, y = no_car)) + 
  geom_point() + facet_wrap(~year, nrow = 3)
   
ggplot(Townsend_aggregated, aes(x =unemployed)) + geom_histogram() + facet_wrap(~year, nrow = 3)

Townsend_aggregated %>% filter(year != "1971") %>%
  #ggplot(aes(x = Overcrowded)) +
  #ggplot(aes(x = Owned)) +
  ggplot(aes(x = no_car)) + 
  geom_histogram() + facet_wrap(~year, nrow = 3)

Graph_variable <- function(data, y, var){
geographical_townsend <- Birmingham_2021 %>% right_join(data %>% filter(year == y), by = c("LSOA21CD" = "lsoa21"))
map <- tm_shape(geographical_townsend) + 
  tm_fill(col = var, 
          style = "cont", 
          palette = "brewer.blues") + 
  tm_borders(alpha = 0.4)
return(map) }

Graph_variable(Townsend_aggregated, "2021", "unemployed")
Graph_variable(Townsend_aggregated, "2021", "Overcrowded")
Graph_variable(Townsend_aggregated, "2021", "Owned")
Graph_variable(Townsend_aggregated, "2021", "no_car")

Graph_variable(Townsend_aggregated, "2011", "unemployed")
Graph_variable(Townsend_aggregated, "2011", "Overcrowded")
Graph_variable(Townsend_aggregated, "2011", "Owned")
Graph_variable(Townsend_aggregated, "2011", "no_car")

Graph_variable(Townsend_aggregated, "2001", "unemployed")
Graph_variable(Townsend_aggregated, "1991", "unemployed")
Graph_variable(Townsend_aggregated, "1981", "unemployed")
Graph_variable(Townsend_aggregated, "1971", "unemployed")

##################
#### Outliers ####
##################

#############################
#### EDA - Correlations #####
#############################

plot_correlation <- function(data, y){
Cor_Matrix <- data %>% filter(year == y) %>% select(-year) %>%  cor()
plot <- corrplot(Cor_Matrix, addCoef.col = 'black', tl.pos = 'd')
#return(plot)
}

plot_correlation(Townsend_aggregated %>% select(year, total_residents, unemployed, Overcrowded:no_car), "1971")

plot_correlation(Manufacturing_aggregated %>% select(year, total_residents, InActive_sick:Well_Edu), "2021")

# Between year correlation of each factor

plot_time_correlation <- function(data, var){
var_data <- data %>% select(year, lsoa21, all_of(var)) %>% pivot_wider(names_from = year, values_from = var)
corrplot(cor(var_data %>% select(-lsoa21)), type = 'lower', tl.pos = 'd', tl.col = 'black', cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), addCoef.col = 'white')
}

plot_time_correlation(Townsend_aggregated, "unemployed")
plot_time_correlation(Townsend_aggregated, "Overcrowded")
plot_time_correlation(Townsend_aggregated, "Owned")
plot_time_correlation(Townsend_aggregated, "no_car")

#######################################
#### Proportions & Direction ##########
#######################################

TEST <- Townsend_aggregated %>% mutate(prop_unemployed = unemployed/total_Active, prop_Overcrowded = Overcrowded/total_households, 
                               prop_not_owned = (total_households - Owned)/total_households, prop_no_car = no_car/total_households) %>%
  select(year, prop_unemployed:prop_no_car)

plot_correlation(TEST, "2021")
plot_correlation(TEST, "2011")

### PCA - Townsend variables

pc <- prcomp(TEST %>% filter(year == "2021") %>% select(-year), center = TRUE, scale. = TRUE)
print(pc)

# Overall proportions

Baseline_proportions <- Townsend_aggregated %>% select(year, total_Active:no_car) %>% group_by(year) %>% 
  summarize(across(total_Active:no_car, \(x) sum(x))) %>% 
  mutate(base_unemployed = unemployed/total_Active, base_Overcrowded = Overcrowded/total_households, 
         base_not_owned = (total_households - Owned)/total_households, base_no_car = no_car/total_households) %>%
  select(year, base_unemployed: base_no_car)

# Graph overall trends in variables
Baseline_proportions %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>% 
  ggplot(aes(x = year, y = proportion, group = variable, color = variable)) + geom_line()

# Calculate relative proportions

Relative_proportions <- TEST %>% left_join(Baseline_proportions) %>% mutate(relative_unemployment = prop_unemployed - base_unemployed, 
                                                    relative_overcrowded = prop_Overcrowded - base_Overcrowded, 
                                                    relatve_not_owned = prop_not_owned - base_not_owned, 
                                                    relative_no_car = prop_no_car - base_no_car) %>%
  select(year, relative_unemployment:relative_no_car) %>% pivot_longer(-year, names_to = "variable", values_to = "proportion")

ggplot(Relative_proportions, aes(x = variable, y = proportion, group = variable)) + geom_jitter() 
ggplot(Relative_proportions, aes(x = variable, y = proportion, group = variable)) + geom_boxplot() 



#############################################
#### PCA Manufacturing  Index variables #####
#############################################






#################################################################################################################################



############################
#### Create proportions ####
############################


###############################
#### Standardise variables ####
###############################

####################################
#### Create index for each year ####
####################################


