library(tidyverse)
library(readr)
library(moments)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week4") 
dir()

Manufacturing_aggregated <- read_csv("Manufacturing_aggregated.csv")

#####################
#### Missingness ####
#####################

#Note that the missingness in this file corresponds to the same zero_LSOAs in the Townsend data so does not require further comment.

library(sf)
library(corrplot)
library(tmap)
library(spdep)
library(spgwr)
library(RColorBrewer)

# This file contains the boundary information
Birmingham_2021 <- read_sf("~/R/Birmingham&Walsall/Week3/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% 
  filter(str_detect(LSOA21NM, "Birmingham")) 

## Distribution of 10% sample in relevant years population by year
Manufacturing_aggregated %>% filter(year <= 1991) %>% ggplot(aes(residents_10)) + geom_histogram() + facet_wrap(~year)

## Compare 10% sample to Actual 10% residents
Manufacturing_aggregated %>% filter(year <= 1991) %>% mutate(ratio = residents_10/total_residents) %>% 
  ggplot(aes(ratio)) + geom_boxplot()

## Small sample outliers 
IQR_residents_10 <- Manufacturing_aggregated %>% filter(year <= 1991) %>% select(year:residents_10) %>%
  #ratio calculated - handles zero LSOAs as balanced whereas sets populated LSOAs with no small sample data as NA - These need imputation!!
  mutate(ratio = ifelse(residents_10 == 0, ifelse(total_residents == 0, 0.1, NA), residents_10/total_residents)) %>% 
  group_by(year) %>%
  mutate(low_quartile = quantile(ratio, probs = 0.25, na.rm = TRUE), upper_quartile = quantile(ratio, probs = 0.75, na.rm = TRUE)) %>%
  mutate(IQR = upper_quartile - low_quartile, low_outlier = ratio < low_quartile - 1.5*IQR, upper_outlier = ratio > upper_quartile + 1.5*IQR)

summary(IQR_residents_10$low_outlier) # 19 lower outliers (significant <10% of residents canvased to small sample questions)
summary(IQR_residents_10$upper_outlier) # 51 upper outliers (significant >10% of residents canvased to small sample questions)

# Label these outliers in the original data for later!
Manufacturing_aggregated <- Manufacturing_aggregated %>% left_join(IQR_residents_10 %>% select(year, lsoa21, low_outlier, upper_outlier))

## Graph these outliers on a map
Graph_variable_categorical <- function(data, y, var){
  geographical_Manufacturing <- Birmingham_2021 %>% right_join(data %>% filter(year == y), by = c("LSOA21CD" = "lsoa21"))
  map <- tm_shape(geographical_Manufacturing) + 
    tm_fill(col = var, 
            style = "cat", 
            palette = "brewer.greens") + 
    tm_borders(alpha = 0.4)
  return(map) }

Graph_variable_categorical(Manufacturing_aggregated, 1981, "low_outlier")

# Are these systematically large/small? 
Manufacturing_aggregated %>% group_by(year) %>% summarize(count_low_outliers = sum(low_outlier, na.rm = TRUE), count_upper_outlier = sum(upper_outlier, na.rm = TRUE))

##########################################
#### Distribution of Health variables ####
##########################################

### Ill-health
ggplot(Manufacturing_aggregated, aes(x = total_residents, y = log(InActive_sick/total_residents + 1))) + geom_point() + facet_wrap(~year, nrow = 3)

## Trend in ill-health

Trend_health <- Manufacturing_aggregated %>% select(year, InActive_sick, Disabled, poor_health,total_residents, total_persons) %>% group_by(year) %>% 
  summarize(across(everything(), \(x) sum(x))) %>% 
  mutate(InActive_sick = InActive_sick/total_residents, 
         Disabled = Disabled/total_residents, 
         poor_health = ifelse(total_persons ==0, 0, poor_health/total_persons)) %>%
  select(-c(total_persons, total_residents))

ggplot(Trend_health %>% pivot_longer(-year, names_to = "variable", values_to = "count"), aes(x = year, y = count, group = variable, color = variable)) + geom_line()

## Correlation between health variables

proportions_health <- Manufacturing_aggregated %>% select(year, lsoa21, InActive_sick, Disabled, poor_health,total_residents, total_persons) %>%
  mutate(InActive_sick = InActive_sick/total_residents, 
         Disabled = Disabled/total_persons, 
         poor_health = ifelse(total_persons ==0, 0, poor_health/total_persons)
         ) %>%
  select(-c(total_persons, total_residents))

ggplot(proportions_health, aes(x = InActive_sick)) + geom_histogram() + facet_wrap(~year, nrow = 3)
ggplot(proportions_health, aes(x = InActive_sick, group = year, y = year)) + geom_boxplot()

library(corrplot)
plot_correlation <- function(data, y){
  Cor_Matrix <- data %>% filter(year == y) %>% filter(!if_any(where(is.numeric), ~is.na(.x))) %>% select(-c(year,lsoa21)) %>%  cor()
  plot <- corrplot(Cor_Matrix, addCoef.col = 'black', tl.pos = 'd')
}

plot_correlation(proportions_health, "2011")

plot_time_correlation <- function(data, var){
  var_data <- data %>% select(year, lsoa21, all_of(var)) %>% pivot_wider(names_from = year, values_from = var)
  corrplot(cor(var_data %>% filter(!if_any(where(is.numeric), ~is.na(.x))) %>% select(-lsoa21) ), type = 'upper', tl.pos = 'd', tl.col = 'black', cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), addCoef.col = 'white')
}

plot_time_correlation(proportions_health, "InActive_sick")


### Geographical plots 

## General geographical plot

Graph_variable <- function(data, y, var){
  geographical_Manufacturing <- Birmingham_2021 %>% right_join(data %>% filter(year == y), by = c("LSOA21CD" = "lsoa21"))
  map <- tm_shape(geographical_Manufacturing) + 
    tm_fill(col = var, 
            style = "cont", 
            palette = "brewer.blues") + 
    tm_borders(alpha = 0.4)
  return(map) }

# These are the two years with some extreme outliers (but they do not seem to geographically overlap)
Graph_variable(proportions_health, 1971, "InActive_sick")
Graph_variable(proportions_health, 1991, "InActive_sick")

## Geographical plot of outliers)

IQR_proportions_health <- Manufacturing_aggregated %>% select(year, lsoa21, InActive_sick,total_residents) %>%
  #ratio calculated - handles zero LSOAs as balanced
  mutate(ratio_sick = ifelse(total_residents == 0, NA, InActive_sick/total_residents)) %>% # Changed this to NA (26th May)
  group_by(year) %>%
  mutate(low_quartile = quantile(ratio_sick, probs = 0.25, na.rm = TRUE), upper_quartile = quantile(ratio_sick, probs = 0.75, na.rm = TRUE)) %>%
  mutate(IQR = upper_quartile - low_quartile, is_outlier = ratio_sick > upper_quartile + 1.5*IQR) 

#IQR_proportions_health %>% filter(is.na(ratio_sick))

# Count outliers - note less than 1% of data from 1981-2021 and less than 2% of data for 1971. 
IQR_proportions_health %>% group_by(year) %>% summarize(count_outliers = sum(is_outlier, na.rm = TRUE))

geographical_Manufacturing <- Birmingham_2021 %>% right_join(IQR_proportions_health %>% group_by(lsoa21) %>% 
                                                               summarize(count_outlier = sum(is_outlier)), by = c("LSOA21CD" = "lsoa21"))
tm_shape(geographical_Manufacturing) + 
    tm_fill(col = "count_outlier", 
            style = "cat") + 
    tm_borders(alpha = 0.4)

# There does not seem to be an immediate pattern in these!

### Winstorization of outliers - 
# The most straightforward approach is to shift these outliers to a more reasonable position... 
# I'm going for the upper_quartile + 2*IQR (This only shifts the pesky 1991 and 1971 values)

proportions_health_adj <- IQR_proportions_health %>% 
  mutate(New_sick = ifelse(ratio_sick > upper_quartile + 2*IQR, upper_quartile + 2*IQR, ratio_sick), 
         Health_Adjusted = New_sick != ratio_sick) %>% select(year, lsoa21, prop_sick = New_sick, Health_Adjusted)

# proportions_health_adj %>% filter(is.na(prop_sick)) - There are 4 NA rows here

ggplot(proportions_health_adj, aes(x = prop_sick)) + geom_histogram() + facet_wrap(~year, nrow = 3)
ggplot(proportions_health_adj, aes(x = prop_sick, group = year, y = year)) + geom_boxplot()

### Relationship with unemployment??? 
Townsend_aggregated <- read_csv("Townsend_aggregated.csv")

extra_proportions <- Townsend_aggregated %>% select(year, lsoa21, total_Active:unemployed) %>% 
  mutate(unemployed = unemployed/total_Active) %>% select(-total_Active) %>% left_join(proportions_health)

# Scatter plot relationship between unemployment and health
ggplot(extra_proportions, aes(x = InActive_sick, y = unemployed)) + geom_point() + facet_wrap(~year, nrow = 3)

## We can see that this is an uneven adjustment with unemployment and there is one super weird point in 1991 - This is Rubery (near Rednal)
## Overall the scatterplots give a good indication that InActive_sick is correlated with unemployment in later censuses and could be an effective proxy for hidden unemployment. 

# Correlation
plot_correlation(extra_proportions, "2001")

# Averages/Trend lines
unemployed_proportions <- Townsend_aggregated %>% select(year, total_Active:unemployed) %>% group_by(year) %>% 
  summarize(across(total_Active:unemployed, \(x) sum(x, na.rm = TRUE))) %>% 
  mutate(unemployed = unemployed/total_Active) %>% select(year, unemployed)

unemployed_proportions %>% left_join(Trend_health) %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>%
  ggplot(aes(x = year, y = proportion, group = variable, colour = variable)) + geom_line()

####################################
##### Types of Manual workers ######
####################################

### Create proportions on a reasonable basis

proportions_manual <- Manufacturing_aggregated %>% select(year, lsoa21, residents_10, total_residents, Foreman:Manufacturing) %>%
  mutate(Total_manual = Foreman + Skilled + Semi_skilled + Unskilled, 
         prop_manual = ifelse(Total_employed == 0, NA, ifelse(Total_manual/Total_employed > 1, 1, Total_manual/Total_employed)), 
         prop_manufacturing = ifelse(Total_employed == 0 , NA, Manufacturing/Total_employed), 
         across(Foreman:Unskilled, ~ifelse(Total_manual == 0, NA, .x/Total_manual), .names = "prop_{.col}")) #Note these are relative proportions that might need adjustment below. 

# sum(proportions_manual$Total_employed == 0) #- There are 6 NA rows corresponding to these 
# sum(proportions_manual$total_residents == 0) #- There are 4 NA rows corresponding to these (subset of the above)
# There is one additional row with Total_manual == 0 but this generates prop_manual = 0 not NA and NA's in the relative proportion rows

# Histograms
graph_manual <- function(prop){
  if(prop %in% c("prop_manual", "prop_manufacturing")){
    prop <- sym(prop)
    proportions_manual %>% ggplot(aes(x = {{prop}})) + geom_histogram() + facet_wrap(~year, nrow = 3)
  } else {
    prop <- sym(prop)
    proportions_manual %>% mutate(workforce_prop = {{prop}}*prop_manual) %>% # Create proportion of workforce 
    ggplot(aes(x = workforce_prop)) + geom_histogram() + facet_wrap(~year, nrow = 3)
    }
}

graph_manual("prop_manual")
graph_manual("prop_manufacturing")
graph_manual("prop_Foreman")
graph_manual("prop_Skilled")
graph_manual("prop_Semi_skilled")
graph_manual("prop_Unskilled")

# Boxplots 

boxplot_manual <- function(prop){
  if(prop %in% c("prop_manual", "prop_manufacturing")){
    prop <- sym(prop)
    proportions_manual %>% ggplot(aes(x = {{prop}}, y = year, group = year)) + geom_boxplot() 
  } else {
    prop <- sym(prop)
    proportions_manual %>% mutate(workforce_prop = {{prop}}*prop_manual) %>% # Create proportion of workforce 
      ggplot(aes(x = workforce_prop, y = year, group = year)) + geom_boxplot() 
  }
}

boxplot_manual("prop_manual")
boxplot_manual("prop_manufacturing")
boxplot_manual("prop_Foreman")
boxplot_manual("prop_Skilled")
boxplot_manual("prop_Semi_skilled")
boxplot_manual("prop_Unskilled")

# There are a considerable number of outliers here! But I have decided to take no action about these

###########################################

# Relationship between proportion manufacturing and proportion manual

proportions_manual %>% ggplot(aes(x = prop_manufacturing, y = prop_manual)) + geom_point() + facet_wrap(~year, nrow = 3,  scales = "free")

# Relationship between proportion manufacturing and type of manual worker - split by year!

proportions_manual %>% select(year, lsoa21, prop_manual:prop_Unskilled) %>% mutate(across(prop_Foreman:prop_Unskilled, ~.x*prop_manual)) %>%
  select(-c(prop_manual, prop_Foreman)) %>% pivot_longer(-c(year:prop_manufacturing), names_to = "skill_level", values_to = "proportion") %>%
  ggplot(aes(x = prop_manufacturing, y = proportion)) + geom_point() + facet_grid(rows = vars(year), cols = vars(skill_level), scales = "free")

## These are the same NA rows as previously!!

###########################################

### Trends in each type of manual worker

# Stacked bar graph 

Manufacturing_aggregated %>% select(year, Foreman:Total_employed) %>% group_by(year) %>%
  summarize(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  mutate(Total_manual = Foreman + Skilled + Semi_skilled + Unskilled, 
         across(Foreman:Unskilled, ~ .x/Total_manual, .names = "prop_{.col}")) %>%
  select(year, prop_Foreman:prop_Unskilled) %>% pivot_longer(-year, names_to = "Skill_level", values_to = "proportion") %>%
  ggplot(aes(x = as.factor(year), y = proportion, fill = Skill_level)) + geom_col()

# Line graph (proportions)

Manufacturing_aggregated %>% select(year, Foreman:Manufacturing) %>% group_by(year) %>%
  summarize(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  mutate(Total_manual = Foreman + Skilled + Semi_skilled + Unskilled, 
         across(Foreman:Unskilled, ~ .x/Total_employed, .names = "prop_{.col}"), 
         prop_manual = Total_manual/Total_employed, 
         prop_manufacturing = Manufacturing/Total_employed) %>%
  select(year, prop_Foreman:prop_manufacturing) %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>%
  ggplot(aes(x = year, y = proportion, group = variable, color = variable)) + geom_line()

# This shows the trend in Semi-skilled and Skilled workers being very similar over time and following the same pattern as total manual workers. 

# Average Absolute values 
Manufacturing_aggregated %>% 
  # For 1971-1991 we need to calculated expected values across the whole population of a LSOA21 using the 10% sample size
  mutate(ratio = ifelse(residents_10 == 0, ifelse(total_residents == 0, 0.1, NA), residents_10/total_residents), 
         across(Foreman:Manufacturing, ~ifelse(year > 1991, .x, .x/ratio))) %>%
  select(year, Foreman:Manufacturing) %>% group_by(year) %>%
  summarize(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
  mutate(Total_manual = Foreman + Skilled + Semi_skilled + Unskilled) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "mean_count") %>%
  ggplot(aes(x = year, y = mean_count, group = variable, color = variable)) + geom_line()

### Geographical plots 

Graph_variable(proportions_manual, 1971, "prop_manual")
Graph_variable(proportions_manual, 1981, "prop_manual")
Graph_variable(proportions_manual, 1991, "prop_manual")
Graph_variable(proportions_manual, 2001, "prop_manual")
Graph_variable(proportions_manual, 2011, "prop_manual")
Graph_variable(proportions_manual, 2021, "prop_manual")

Graph_variable(proportions_manual, 1971, "prop_manufacturing")
Graph_variable(proportions_manual, 1981, "prop_manufacturing")
Graph_variable(proportions_manual, 1991, "prop_manufacturing")
Graph_variable(proportions_manual, 2001, "prop_manufacturing")
Graph_variable(proportions_manual, 2011, "prop_manufacturing")
Graph_variable(proportions_manual, 2021, "prop_manufacturing")

Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 1971, "prop_Unskilled")
Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 1981, "prop_Unskilled")
Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 1991, "prop_Unskilled")
Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 2001, "prop_Unskilled")
Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 2011, "prop_Unskilled")
Graph_variable(proportions_manual %>% mutate(prop_Unskilled = prop_Unskilled*prop_manual), 2021, "prop_Unskilled")

#####################################
##### Proportion well-educated ######
#####################################

# Some of the outliers for 1971 are insane as they represent more than 100% of the working age residents being well_educated!! 
# Looking at the table description is this because of the inclusion of retired people? 
# At this stage, I am considering capping these variables at 1!!
List_LSOA = c("E01009174", "E01009157", "E01009155")

proportions_Edu <- Manufacturing_aggregated %>% select(year, lsoa21, residents_10, total_residents, Well_Edu) %>%
  mutate(prop_Edu = ifelse(year >1991, Well_Edu/total_residents, ifelse(residents_10 == 0, NA, ifelse(Well_Edu/residents_10 >1, 1, Well_Edu/residents_10)))) %>% 
  ### Correct erroneous NAs caused by the imputation of 2001 data
  mutate(prop_Edu = ifelse(year == 1991 & lsoa21 %in% List_LSOA, Well_Edu/total_residents, prop_Edu)) %>%
  select(year, lsoa21, prop_Edu) 

proportions_Edu %>% filter(is.na(prop_Edu))

# Histogram 
    proportions_Edu %>% ggplot(aes(x = prop_Edu)) + geom_histogram() + facet_wrap(~year, nrow = 3)

# Boxplots 
    proportions_Edu %>% ggplot(aes(x = prop_Edu, y = year, group = year)) + geom_boxplot() 

#Average trend
Manufacturing_aggregated %>% select(year, total_residents, residents_10, Well_Edu) %>% group_by(year) %>%
      summarize(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
      mutate(prop_Edu = ifelse(year >1991, Well_Edu/total_residents, Well_Edu/residents_10)) %>%
      ggplot(aes(x = year, y = prop_Edu)) + geom_line()

#Geographical representation 

Graph_variable(proportions_Edu, 1971, "prop_Edu")
Graph_variable(proportions_Edu, 1981, "prop_Edu")
Graph_variable(proportions_Edu, 1991, "prop_Edu")
Graph_variable(proportions_Edu, 2001, "prop_Edu")
Graph_variable(proportions_Edu, 2011, "prop_Edu")
Graph_variable(proportions_Edu, 2021, "prop_Edu")

#######################################
#### Proportions & Direction ##########
#######################################

## Collect up proportions here: 

#Redefined proportions to avoid generating incorrect NAs
proportions_manual <- Manufacturing_aggregated %>% select(year, lsoa21, residents_10, total_residents, Foreman:Manufacturing) %>%
  mutate(Total_manual = Foreman + Skilled + Semi_skilled + Unskilled, 
         prop_manual = ifelse(Total_employed == 0, NA, ifelse(Total_manual/Total_employed > 1, 1, Total_manual/Total_employed)), 
         prop_manufacturing = ifelse(Total_employed == 0 , NA, Manufacturing/Total_employed), 
         across(Foreman:Unskilled, ~ifelse(Total_employed == 0, NA, .x/Total_employed), .names = "prop_{.col}")) #These are now absolute proportions


proportions_all <- proportions_manual %>% select(year, lsoa21, prop_manufacturing, prop_manual, prop_Skilled:prop_Unskilled) %>%
  mutate(prop_Skilled = prop_Skilled + prop_Semi_skilled) %>%
  select(-c(prop_manual, prop_Semi_skilled)) %>%
  # Change direction of Well_Educated
  left_join(proportions_Edu %>% mutate(prop_low_Edu = 1 - prop_Edu) %>% select(-prop_Edu)) %>%
  left_join(proportions_health_adj %>% select(-Health_Adjusted)) 

# Here are the NAs for clarity
sum(is.na(proportions_all))
proportions_all %>% filter(if_any(starts_with("prop_"), ~is.na(.x)))

###########################################################
##### Correlation variables within proposed new index #####
###########################################################

plot_correlation(proportions_all, "2021")
plot_correlation(proportions_all, "2011")
plot_correlation(proportions_all, "2001")

plot_correlation(proportions_all, "1991")
plot_correlation(proportions_all, "1981")
plot_correlation(proportions_all, "1971")

plot_time_correlation(proportions_all, "prop_sick")
plot_time_correlation(proportions_all, "prop_low_Edu")
plot_time_correlation(proportions_all, "prop_Unskilled")
plot_time_correlation(proportions_all, "prop_Skilled")
plot_time_correlation(proportions_all, "prop_manufacturing")


# Graph of trend in overall proportions (crude as does not contain adjustments made previously....)

proportions_all_Birmingham <- Manufacturing_aggregated %>% group_by(year) %>%
  select(total_residents, residents_10, Total_employed, InActive_sick, Skilled, Semi_skilled, Unskilled, Manufacturing, Well_Edu) %>%
  summarize(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  mutate(prop_sick = InActive_sick/total_residents,  
         across(Skilled:Manufacturing, ~.x/Total_employed, .names = "prop_{.col}"), 
         prop_Edu = ifelse(year > 1991, Well_Edu/total_residents, ifelse(Well_Edu/residents_10 > 1, 1, Well_Edu/residents_10)),
         prop_low_Edu = 1 - prop_Edu,
         prop_Skilled = prop_Skilled + prop_Semi_skilled) %>%
  select(year, prop_sick:prop_low_Edu, -prop_Semi_skilled, -prop_Edu) 

proportions_all_Birmingham %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>%
  ggplot(aes(x = year, y = proportion, group = variable, color = variable)) + geom_line()

#######################################
### PCA - Manufacturing variables #####
#######################################

pc <- prcomp(na.omit(proportions_all %>% filter(year == "1981") %>% select(-c(year, lsoa21))), center = TRUE, scale. = TRUE)
print(pc)

library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

###############################
#### Standardise variables ####
###############################

# In this case, I'm going to make do min/max rescaling - However this is very sensitive to outliers which we know we have quite a few of.... 

Manufacturing_minmax <- proportions_all %>%
  ## Over all time points
  mutate(across(starts_with("prop_"), \(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),  .names = "minmax_{str_remove(.col, 'prop_')}")) %>% 
  group_by(year) %>% 
  ## within each census 
  mutate(across(starts_with("prop_"), \(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),  .names = "minmax_{str_remove(.col, 'prop_')}_year")) %>%
  ungroup()

# Here are the NAs for clarity
sum(is.na(Manufacturing_minmax))
Manufacturing_minmax %>% filter(if_any(everything(), ~is.na(.x)))

####################################
#### Create index for each year ####
####################################

Manufacturing_index <- Manufacturing_minmax %>% rowwise() %>% 
  filter(!if_any(everything(), ~is.na(.x))) %>%
  mutate(Manufacturing_index = sum(c_across(minmax_manufacturing:minmax_low_Edu))) %>%
  mutate(Manufacturing_index_year = mean(c_across(minmax_manufacturing_year:minmax_low_Edu_year))) %>%
  select(year, lsoa21, Manufacturing_index, Manufacturing_index_year)

### Visualise the overall trend in the Manufacturing index

Summary <- Manufacturing_index %>% select(year, Manufacturing_index) %>% 
  group_by(year) %>% summarise(mean_Manufacturing_index = mean(Manufacturing_index, na.rm = TRUE)) 

ggplot(Summary,aes(x = year, y = mean_Manufacturing_index)) + geom_line()

### Create and label quintitles

## for Manufacturing Index we want to identify the 20% HIGHEST (LSOA 2021, year) pairs overall and label these Quintitle 1 etc. 

return_quintile <- function(index) {
  # Category Labels
  labels <- c(5,4,3,2,1)
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

### Overall Manufacturing index and quintiles

Manufacturing_index_all <- Manufacturing_index %>% bind_cols(Quintile = return_quintile(Manufacturing_index$Manufacturing_index))

table(Manufacturing_index_all$Quintile, Manufacturing_index_all$year)

## We would like to look at trends in each input variable in relation to the quintile of the (LSOA 2021, year) pair

## Scattergraphs

Joined <- Manufacturing_index_all %>% left_join(proportions_all) 

ggplot(Joined, aes(x = Manufacturing_index, y = prop_sick)) + geom_point() + facet_wrap(~year, ncol = 2)
ggplot(Joined, aes(x = Manufacturing_index, y = prop_manufacturing)) + geom_point() + facet_wrap(~year, ncol = 2)
ggplot(Joined, aes(x = Manufacturing_index, y = prop_Skilled)) + geom_point() + facet_wrap(~year, ncol = 2)
ggplot(Joined, aes(x = Manufacturing_index, y = prop_Unskilled)) + geom_point() + facet_wrap(~year, ncol = 2)
ggplot(Joined, aes(x = Manufacturing_index, y = prop_low_Edu)) + geom_point() + facet_wrap(~year, ncol = 2)

## Trends lines

# This adds the mean Manufacturing index line to the previous trend graph

proportions_all_Birmingham %>% left_join(Summary) %>% pivot_longer(-year, names_to = "variable", values_to = "proportion") %>%
  ggplot(aes(x = year, y = proportion, group = variable, color = variable)) + geom_line()

# Trends in each variable split by quintile

Av_proportions_all_by_quintile <- Manufacturing_index_all %>% select(-Manufacturing_index_year) %>%
  left_join(Manufacturing_aggregated) %>% group_by(year, Quintile) %>%
  select(Manufacturing_index, total_residents, residents_10, Total_employed, InActive_sick, Skilled, Semi_skilled, Unskilled, Manufacturing, Well_Edu) %>%
  summarize(mean_index = mean(Manufacturing_index, na.rm = TRUE), across(-Manufacturing_index, ~sum(.x, na.rm = TRUE))) %>%
  mutate(prop_sick = InActive_sick/total_residents,  
         across(Skilled:Manufacturing, ~.x/Total_employed, .names = "prop_{.col}"), 
         prop_Edu = ifelse(year > 1991, Well_Edu/total_residents, ifelse(Well_Edu/residents_10 > 1, 1, Well_Edu/residents_10)),
         prop_low_Edu = 1 - prop_Edu,
         prop_Skilled = prop_Skilled + prop_Semi_skilled) %>%
  select(year, Quintile, mean_index, prop_sick:prop_low_Edu, -prop_Semi_skilled, -prop_Edu) 

# Graph overall trends in variables by quintile
graph_trend <- function(var){
  Av_proportions_all_by_quintile  %>% pivot_longer(-c(year,Quintile), names_to = "variable", values_to = "proportion") %>% 
  filter(variable == var) %>%
  ggplot(aes(x = year, y = proportion, group = Quintile, color = Quintile)) + geom_line()
}

#graph_trend("mean_index")
graph_trend("prop_Skilled")
graph_trend("prop_sick")
graph_trend("prop_Unskilled")
graph_trend("prop_low_Edu")
#As expected these changes are particularly interesting!!!

##### Indices which are specific to each year ######

# Manufacturing index by census and quintiles for each census point
Manufacturing_index_year <- Manufacturing_index %>% select(-Manufacturing_index) %>% pivot_wider(names_from = year, values_from = Manufacturing_index_year) %>%
  mutate(across(where(is.numeric), ~return_quintile(.x), .names = "Quintile_{.col}"))

#Cross-tabulate - These are the relative movements so if an area was in the bottom 20% in 1971 compared with bottom 20% in 2021
table(Manufacturing_index_year$Quintile_1971, Manufacturing_index_year$Quintile_2021)

# Biggest movers
Manufacturing_index_year %>% mutate(Overall_diff = as.numeric(Quintile_2021) - as.numeric(Quintile_1971)) %>%
  ggplot(aes(x=Overall_diff)) + geom_bar()

#### Final geographical plots! 

## Relative Manufacturing Index (i.e. using all basis)
geographical_Manufacturing <- Birmingham_2021 %>% right_join(Manufacturing_index_all %>% select(-Manufacturing_index) %>%
                                                          pivot_wider(names_from = "year", values_from = "Quintile"), by = c("LSOA21CD" = "lsoa21"))
Graph_index <- function(y){
  map <- tm_shape(geographical_Manufacturing) + 
    tm_fill(col = y, 
            style = "cat", 
            palette = "brewer.prgn") + 
    tm_borders(alpha = 0.4)
  return(map) }

Graph_index("2021")
Graph_index("2011")
Graph_index("2001")
Graph_index("1991")
Graph_index("1981")
Graph_index("1971")

geographical_Manufacturing <- Birmingham_2021 %>% right_join(Manufacturing_index_year %>% mutate(Overall_diff = as.numeric(Quintile_2021) - as.numeric(Quintile_1971)), by = c("LSOA21CD" = "lsoa21"))

Graph_index("Overall_diff")

### Sanity Checks - correlation with population?!

Cor_Matrix <- Manufacturing_index_all %>% left_join(proportions_all) %>% 
  left_join(Manufacturing_aggregated %>% select(year, lsoa21, total_residents) ) %>%
  select(-c(year, lsoa21, Quintile)) %>% filter(!if_any(everything(), ~is.na(.x))) %>% cor()

#sum(is.na(Cor_Matrix))
corrplot(Cor_Matrix, addCoef.col = 'black', tl.pos = 'l', type = 'lower')
# This shows not excessively correlated with population density!

#### Write CSV file of Index and variables used to create it

Manufacturing_prepared <- proportions_all %>% left_join(Manufacturing_index_all %>% rename(Overall_Quintile = Quintile)) %>%
  left_join(Manufacturing_index_year %>% select(lsoa21, Quintile_2021:Quintile_1971) %>% 
              pivot_longer(-lsoa21, names_to = "description", values_to = "Census_Quintile") %>%
              mutate(year = as.numeric(str_extract(description, pattern = "\\d{4}$"))) %>%
              select(-description))

write_csv(Manufacturing_prepared, "~/R/Birmingham&Walsall/Week5/Manufacturing_prepared.csv")
