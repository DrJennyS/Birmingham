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

## See below!! ##

#############################
#### EDA - Correlations #####
#############################

library(corrplot)

plot_correlation <- function(data, y){
Cor_Matrix <- data %>% filter(year == y) %>% select(-year) %>%  cor()
plot <- corrplot(Cor_Matrix, addCoef.col = 'black', tl.pos = 'd')
#return(plot)
}

plot_correlation(Townsend_aggregated %>% select(year, total_residents, unemployed, Overcrowded:no_car), "1971")

plot_correlation(Manufacturing_aggregated %>% select(year, total_residents, InActive_sick, Skilled:Well_Edu) %>% na.omit(), "2001")

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

Townsend_proportions <- Townsend_aggregated %>% mutate(prop_unemployed =  ifelse(total_Active == 0, NA, unemployed/total_Active), 
                                                       prop_Overcrowded = ifelse(total_households == 0, NA, Overcrowded/total_households), 
                                                       prop_not_owned = ifelse(total_households == 0, NA, (total_households - Owned)/total_households), 
                                                       prop_no_car = ifelse(total_households == 0, NA, no_car/total_households)) %>%
  filter(total_households != 0 & total_Active != 0) %>%
  select(year, lsoa21, prop_unemployed:prop_no_car) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(across(where(is.numeric), ~ .x * 100)) %>% #Percentages
  mutate(prop_unemployed = log(prop_unemployed + 1), prop_Overcrowded = log(prop_Overcrowded + 1)) #Transformation of skewed variables
  
sum(is.na(Townsend_proportions))

# Amend this to remove non-numerical columns !!!
plot_correlation(Townsend_proportions %>% select(-lsoa21), "2021")
plot_correlation(Townsend_proportions %>% select(-lsoa21), "2011")

# Overall proportions

Baseline_proportions <- Townsend_aggregated %>% select(year, total_Active:no_car) %>% group_by(year) %>% 
  summarize(across(total_Active:no_car, \(x) sum(x))) %>% 
  mutate(base_unemployed = unemployed/total_Active, base_Overcrowded = Overcrowded/total_households, 
         base_not_owned = (total_households - Owned)/total_households, base_no_car = no_car/total_households) %>%
  select(year, base_unemployed: base_no_car)

# Graph overall trends in variables
Baseline_proportions %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>% 
  ggplot(aes(x = year, y = proportion, group = variable, color = variable)) + geom_line()

##################################
### PCA - Townsend variables #####
##################################

pc <- prcomp(Townsend_proportions %>% filter(year == "2021") %>% select(-year), center = TRUE, scale. = TRUE)
print(pc)

###############################
#### Standardise variables ####
###############################

Townsend_z_scores <- Townsend_proportions %>% 
  ## z-scores over all time points
  mutate(across(where(is.numeric), \(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),  .names = "z_{str_remove(.col, 'prop_')}")) %>% 
  group_by(year) %>% 
  ## z-scores by time point
  mutate(across(starts_with("prop_"), \(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),  .names = "z_{str_remove(.col, 'prop_')}_year")) %>%
  ungroup()

# Min/Max (by year)

Townsend_z_scores %>% select(year, lsoa21, z_unemployed_year:z_no_car_year) %>% group_by(year) %>% 
  summarize(across(starts_with("z_"), \(x) min(x), .names = "min_{.col}"))

Townsend_z_scores %>% select(year, lsoa21, z_unemployed_year:z_no_car_year) %>% group_by(year) %>% 
  summarize(across(starts_with("z_"), \(x) max(x), .names = "max_{.col}"))

# Count Outliers
Townsend_z_scores %>% select(year, lsoa21, z_unemployed_year:z_no_car_year) %>% group_by(year) %>% 
  summarize(across(starts_with("z_"), \(x) sum(abs(x) > 3), .names = "count_outliers_{.col}"))

#Label Outliers in z_scores data and look at other columns in original data
Townsend_z_scores %>% select(year, lsoa21, z_unemployed_year) %>% mutate(Outlier = abs(z_unemployed_year) > 3) %>%
  filter(Outlier == TRUE) %>% left_join(Townsend_aggregated %>% mutate(year = as.factor(year))) %>% select(year:z_unemployed_year, total_residents:unemployed, zero_LSOA:large_LSOA)

Townsend_z_scores %>% select(year, lsoa21, z_no_car_year) %>% mutate(Outlier = abs(z_no_car_year) > 3) %>%
  filter(Outlier == TRUE) %>% left_join(Townsend_aggregated %>% mutate(year = as.factor(year))) 

## This analysis suggests that the LSOAs with zero households should be replaced with NA as they are creating additional outliers that could unduly influence the analysis. 
## Changed line 139 accordingly and repeated following analysis

####################################
#### Create index for each year ####
####################################

Townsend_index <- Townsend_z_scores %>% rowwise() %>% mutate(Townsend_index = sum(c_across(z_unemployed:z_no_car))) %>%
  mutate(Townsend_index_year = sum(c_across(z_unemployed_year:z_no_car_year))) %>%
  select(year, lsoa21, Townsend_index, Townsend_index_year)

### Visualise the overall trend in the Townsend index

Summary <- Townsend_index %>% select(year, Townsend_index) %>% 
  group_by(year) %>% summarise(mean_Townsend_index = mean(Townsend_index, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))

ggplot(Summary,aes(x = year, y = mean_Townsend_index)) + geom_line()

### Create and label quintitles

## for Townsend Index we want to identify the 20% lowest (LSOA 2021, year) pairs overall and label these Quintitle 1 etc. 

return_quintile <- function(index) {
  # Category Labels
  labels <- c(1,2,3,4,5)
  # Calculate breaks based on quintiles
  breaks <- quantile(index, probs = seq(0, 1, 0.2), na.rm = TRUE)
  # Handle case where there are duplicate values at boundaries
  if (length(unique(breaks)) < 6) {
    # Alternative approach using rank
    ranks <- rank(index, ties.method = "average", na.last = "keep")
    n <- sum(!is.na(index))
    norm_ranks <- (ranks - 0.5) / n
    result <- cut(norm_ranks, breaks = seq(0, 1, 0.2), 
                  labels = labels, include.lowest = TRUE)
  } else {
    # Standard approach
    result <- cut(index, breaks = breaks, 
                  labels = labels, include.lowest = TRUE)
  }
  return(result)
}

# Overall Townsend index and quintiles
Townsend_index_all <- Townsend_index %>% select(-Townsend_index_year) %>% bind_cols(Quintile = return_quintile(Townsend_index$Townsend_index))

table(Townsend_index_all$Quintile, Townsend_index_all$year)

## We would like to look at trends in each input variable in relation to the quintile of the (LSOA 2021, year) pair

## Scattergraphs

Joined <- Townsend_index_all %>% left_join(Townsend_proportions) 

ggplot(Joined, aes(x = Townsend_index, y = prop_no_car)) + geom_point() + facet_wrap(~year, ncol = 2)

## Trends lines

Joined <- Townsend_index_all %>% mutate(year = as.numeric(as.character(year))) %>% left_join(Townsend_aggregated)

Average_proportions <- Joined %>% group_by(Quintile, year) %>% 
  summarize(across(total_residents:no_car, \(x) sum(x))) %>% 
  mutate(av_unemployed = unemployed/total_Active, av_Overcrowded = Overcrowded/total_households, 
         av_not_owned = (total_households - Owned)/total_households, av_no_car = no_car/total_households) %>%
  select(year, av_unemployed: av_no_car)

# Graph overall trends in variables
graph_trend <- function(var){
Average_proportions %>% pivot_longer(-c(year,Quintile), names_to = "variable", values_to = "proportion") %>% 
  filter(variable == var) %>%
  ggplot(aes(x = year, y = proportion, group = Quintile, color = Quintile)) + geom_line()
}

graph_trend("av_unemployed")
graph_trend("av_Overcrowded")
graph_trend("av_not_owned")
graph_trend("av_no_car")
#As expected these changes are particularly interesting!!!

# Townsend index by census and quintiles for each census point
Townsend_index_year <- Townsend_index %>% select(-Townsend_index) %>% pivot_wider(names_from = year, values_from = Townsend_index_year) %>%
  mutate(across(where(is.numeric), ~return_quintile(.x), .names = "Quintile_{.col}"))

#Cross-tabulate - These are the relative movements so if an area was in the bottom 20% in 1971 compared with bottom 20% in 2021
table(Townsend_index_year$Quintile_1971, Townsend_index_year$Quintile_2021)

# Biggest movers
Townsend_index_year %>% mutate(Overall_diff = as.numeric(Quintile_2021) - as.numeric(Quintile_1971)) %>%
  ggplot(aes(x=Overall_diff)) + geom_bar()

#### Final geographical plots!

## Relative Townsend Index (i.e. using all basis)
geographical_townsend <- Birmingham_2021 %>% right_join(Townsend_index_all %>% select(-Townsend_index) %>%
                                                          pivot_wider(names_from = "year", values_from = "Quintile"), by = c("LSOA21CD" = "lsoa21"))
Graph_index <- function(y){
  map <- tm_shape(geographical_townsend) + 
    tm_fill(col = y, 
            style = "cat", 
            palette = "brewer.PRGn") + 
    tm_borders(alpha = 0.4)
  return(map) }

Graph_index("2021")
Graph_index("2011")
Graph_index("2001")
Graph_index("1991")
Graph_index("1981")
Graph_index("1971")

geographical_townsend <- Birmingham_2021 %>% right_join(Townsend_index_year %>% mutate(Overall_diff = as.numeric(Quintile_2021) - as.numeric(Quintile_1971)), by = c("LSOA21CD" = "lsoa21"))

Graph_index("Overall_diff")

### Sanity Checks - correlation with population?!

Cor_Matrix <- Townsend_index_all %>% left_join(Townsend_proportions) %>% 
  left_join(Townsend_aggregated %>% select(year, lsoa21, total_residents, total_Active, total_households) %>% mutate(year = as.factor(year))) %>%
  select(-c(year, lsoa21, Quintile)) %>% cor()
  
corrplot(Cor_Matrix, addCoef.col = 'black', tl.pos = 'd')

####
write_csv()
