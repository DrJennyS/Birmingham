library(sf)
library(ggplot2)
library(tidyverse)
library(readr)
library(corrplot)
library(tmap)
library(RColorBrewer)
library(spdep)
library(spgwr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3") 
dir() #check files in working directory

Birmingham_postcodes <- read_sf("My_pc_lookup.shp")

########## 2021 Boundary file for visualisations ##########################

Birmingham_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham"))

List_2021_LSOA <- st_drop_geometry(Birmingham_2021) %>% select(LSOA21CD)
######################################################################################################################
################## EDA on postcodes ##################################################################################
######################################################################################################################

# Count the number of postcodes per 2021 LSOA
active_pc_2021 <- st_drop_geometry(Birmingham_postcodes) %>% filter(doterm > 202103|is.na(doterm), dointr <= 202103)

summary_by_LSOA21 <- active_pc_2021 %>% group_by(lsoa21) %>% summarise(postcode_count = n()) %>% arrange(desc(postcode_count))

summary(summary_by_LSOA21)

# Histogram of number of postcodes in each 2021 LSOA
ggplot(summary_by_LSOA21, aes(x=postcode_count)) + geom_histogram()

#There are a few very large postcode areas - the areas with more than 75 postcodes are listed below

summary_by_LSOA21 %>% filter(postcode_count >= 75)

#Postcodes at largest LSOA

outliers <- Birmingham_postcodes %>% filter(lsoa21 == "E01033620") %>% 
  mutate(pc_area = str_extract(pcd, pattern = "^B\\d{1,2}")) 

outliers %>% group_by(pc_area) %>% summarize(count_area = n())

ggplot(outliers, aes(x = lat, y = long)) + geom_jitter()

# Note that these do not seem to overlap so it is likely that this is just the town centre rather tha PO BOXs. 
# We can take a look again once we have a population estimate for this area AND know whether it is subject to any boundary changes

#Join the postcode counts to the Birmingham 2021 boundary file and add an indicator for large areas

Birmingham_2021 <- Birmingham_2021 %>% left_join(summary_by_LSOA21, by = join_by("LSOA21CD" == "lsoa21")) %>% 
  mutate(is_large = postcode_count>= 75)

#Graph with the large areas indicated

tm_shape(Birmingham_2021) + tm_fill("is_large", palette = "brewer.pu_bu", midpoint = 0.5) + tm_borders(fill_alpha=.4)
tm_shape(Birmingham_2021) + tm_fill("postcode_count", palette = "brewer.pu_bu", style = "pretty") + tm_borders(fill_alpha=.4)

#######################################################################################################
### Outliers for each time point 
#######################################################################################################

## Add activity flags onto the Postcodes_within_Birmingham sf 
Birmingham_postcodes <- Birmingham_postcodes %>% 
  mutate(active_2021 = ifelse((doterm > 202103|is.na(doterm)) & dointr <= 202103, 1, 0)) %>%
  mutate(active_2011 = ifelse((doterm > 201103|is.na(doterm)) & dointr <= 201103, 1, 0)) %>%
  mutate(active_2001 = ifelse((doterm > 200104|is.na(doterm)) & dointr <= 200104, 1, 0)) %>%
  mutate(active_1991 = ifelse((doterm > 199104|is.na(doterm)) & dointr <= 199104, 1, 0) ) %>%
  mutate(active_1981 = ifelse((doterm > 198104|is.na(doterm)) & dointr <= 198104, 1, 0) )

#Checks
Birmingham_postcodes %>% group_by(active_2021) %>% summarize(count = n())
Birmingham_postcodes %>% group_by(active_2011) %>% summarize(count = n())
Birmingham_postcodes %>% group_by(active_2001) %>% summarize(count = n())
Birmingham_postcodes %>% group_by(active_1991) %>% summarize(count = n())

#Active postcodes by 2021 LSOA

active_postcodes_by_LSOA21 <- st_drop_geometry(Birmingham_postcodes) %>% group_by(lsoa21) %>% 
  summarize(postcode_2021 = sum(active_2021), postcode_2011 = sum(active_2011), postcode_2001 = sum(active_2001), 
            postcode_1991 = sum(active_1991), postcode_1981 = sum(active_1981)) %>% pivot_longer(-lsoa21, names_to = "census", values_to = "postcode_count") %>%
  mutate(census = as.factor(str_replace(census, pattern = "postcode_", replacement = "")))

#Exclude outlier area 
active_postcodes_by_LSOA21 <- active_postcodes_by_LSOA21 %>% filter(lsoa21 != "E01033620")

ggplot(active_postcodes_by_LSOA21, aes(x = census, y = postcode_count)) + geom_boxplot() + geom_jitter()
ggplot(active_postcodes_by_LSOA21, aes(x = postcode_count)) + geom_histogram()  +facet_wrap(~census)

#Changes in number of postcodes
change_postcodes_by_LSOA21 <- st_drop_geometry(Birmingham_postcodes) %>% group_by(lsoa21) %>% 
  summarize(change11to21 = sum(active_2021) - sum(active_2011), change01to11 = sum(active_2011) - sum(active_2001), 
            change91to01 = sum(active_2011) - sum(active_1991),
            totalchange = sum(active_2021) - sum(active_1991)) %>% 
  pivot_longer(-lsoa21, names_to = "change", values_to = "postcode_count")

#Summarise number of changes to justify this weighted approach
change_postcodes_by_LSOA21 %>% group_by(change) %>% summarize(total = n(), unchanged = sum(postcode_count == 0))

ggplot(change_postcodes_by_LSOA21, aes(x = change, y = postcode_count)) + geom_boxplot() + geom_jitter()
ggplot(change_postcodes_by_LSOA21, aes(x = postcode_count)) + geom_histogram() + facet_wrap(~change)

#######################################################################################################################
################# create weights ######################################################################################
#######################################################################################################################

##### Create the proportions for 2011 

### Test with a single lsoa21 first 

test <- st_drop_geometry(Birmingham_postcodes) %>% mutate(oa11 = as.factor(oa11), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_2011 == 1) %>% group_by(oa11) %>% 
  mutate(total_pc_in_oa11 = sum(active_2011)) %>% ungroup()

summary(test$total_pc_in_oa11)

test2 <- test %>% filter(lsoa21 == "E01034948") 

ggplot(test2, aes(x = oa11)) + geom_bar()

test2 <- test2 %>% group_by(oa11) %>% mutate(count_pc_in_oa11= sum(active_2011), prop_oa11 = count_pc_in_oa11/total_pc_in_oa11)

test2 %>% select(lsoa21, oa11, prop_oa11) %>% distinct() 

###################################################################################################
#### Weights for 2011
###################################################################################################

weights_df_2011 <- st_drop_geometry(Birmingham_postcodes) %>% mutate(oa11 = as.factor(oa11), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_2011 == 1) %>% group_by(oa11) %>% 
  mutate(total_pc_in_oa11 = sum(active_2011)) %>% ungroup()

weights_df_2011 <- weights_df_2011 %>% group_by(lsoa21, oa11) %>% 
  mutate(prop_oa11 = sum(active_2011)/total_pc_in_oa11) %>% select(lsoa21, oa11, prop_oa11) 

#Check all weights sum to 1
test <- weights_df_2011 %>% filter(prop_oa11 != 1) %>% distinct() %>% group_by(oa11) %>% summarize(check = sum(prop_oa11))

#Remove all of LSOAs not in 2021 geometry (added 13th May)
weights_df_2011 <- weights_df_2011 %>% right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD")) 
  
#Remove all of the duplicated lines to make it easier to use these weights
weights_df_2011 <- weights_df_2011 %>% distinct() %>% arrange(lsoa21)

#List of oa11's in more than one LSOA 21
summary_split_oa11 <- weights_df_2011 %>% filter(prop_oa11 != 1) %>% select(-prop_oa11) %>% 
  distinct() %>% group_by(oa11) %>% summarise(count = n())

#Proportion of OAs requiring to be split 
nrow(summary_split_oa11)/nrow(weights_df_2011 %>% select(oa11) %>% distinct())

###################################################################################################
### Repeat with 2001
###################################################################################################

weights_df_2001 <- st_drop_geometry(Birmingham_postcodes) %>% mutate(oa01 = as.factor(oa01), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_2001 == 1) %>% group_by(oa01) %>% 
  mutate(total_pc_in_oa01 = sum(active_2001)) %>% ungroup()

weights_df_2001 <- weights_df_2001 %>% group_by(lsoa21, oa01) %>% 
  mutate(prop_oa01 = sum(active_2001)/total_pc_in_oa01) %>% select(lsoa21, oa01, prop_oa01) 

#Check all weights sum to 1
test <- weights_df_2001 %>% filter(prop_oa01 != 1) %>% distinct() %>% group_by(oa01) %>% summarize(check = sum(prop_oa01))

#Remove all of LSOAs not in 2021 geometry (added 13th May)
weights_df_2001 <- weights_df_2001 %>% right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD")) 

#Remove all of the duplicated lines to make it easier to use these weights
weights_df_2001 <- weights_df_2001 %>% distinct() %>% arrange(lsoa21)

#List of oa01's in more than one LSOA 21
summary_split_oa01 <- weights_df_2001 %>% filter(prop_oa01 != 1) %>% select(-prop_oa01) %>% 
  distinct() %>% group_by(oa01) %>% summarise(count = n())

#Proportion of OAs requiring to be split 
nrow(summary_split_oa01)/nrow(weights_df_2001 %>% select(oa01) %>% distinct())

######################################################################################################
### Repeat with 1991 - note that some EDs may be partially outside the 2021 city
######################################################################################################

weights_df_1991 <- st_drop_geometry(Birmingham_postcodes) %>% mutate(ED91CD = as.factor(ED91CD), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_1991 == 1, req_91 == 1) %>% group_by(ED91CD) %>% 
  mutate(total_pc_in_ED91 = sum(active_1991)) %>% ungroup()

weights_df_1991 <- weights_df_1991 %>% 
  group_by(lsoa21, ED91CD) %>% 
  mutate(prop_ED = sum(active_1991)/total_pc_in_ED91) %>% select(lsoa21, ED91CD, prop_ED) %>%
  mutate(ED91CD = str_remove(ED91CD, pattern = "^\\d{2}")) #Added 9th May to join to census data

#At this stage these should add to one as they cover all of the 1991 EDs needed
test <- weights_df_1991 %>% filter(prop_ED != 1) %>% distinct() %>% group_by(ED91CD) %>% summarize(check = sum(prop_ED))
sum(test$check) == nrow(test)

#Remove all of LSOAs not in 2021 geometry (added 13th May)
weights_df_1991 <- weights_df_1991 %>% right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD")) 

#Remove all of the duplicated lines to make it easier to use these weights
weights_df_1991 <- weights_df_1991 %>% distinct() %>% arrange(lsoa21)

#List of ED's in more than one LSOA 21
summary_split_ED91 <- weights_df_1991 %>% filter(prop_ED != 1) %>% select(-prop_ED) %>%
  distinct() %>% group_by(ED91CD) %>% summarise(count = n())

#Proportion of EDs requiring to be split 
nrow(summary_split_ED91)/nrow(weights_df_1991 %>% select(ED91CD) %>% distinct())

######################################################################################################
### Repeat with 1981 - note that some EDs may be partially outside the 2021 city
######################################################################################################

weights_df_1981 <- st_drop_geometry(Birmingham_postcodes) %>% 
  mutate(ED81CDO = as.factor(ED81CDO), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_1981 == 1, req_81 == 1) %>% group_by(ED81CDO) %>% 
  mutate(total_pc_in_ED81 = sum(active_1981)) %>% ungroup()

weights_df_1981 <- weights_df_1981 %>% group_by(lsoa21, ED81CDO) %>% 
  mutate(prop_ED = sum(active_1981)/total_pc_in_ED81) %>% 
  select(lsoa21, ED81CDO, prop_ED) #Changed Variable 20/04

#At this stage these should add to one as they cover all of the 1981 EDs needed
test <- weights_df_1981 %>% filter(prop_ED != 1) %>% distinct() %>% group_by(ED81CDO) %>% summarize(check = sum(prop_ED))
sum(test$check) == nrow(test)

#Remove all of LSOAs not in 2021 geometry (added 13th May)
weights_df_1981 <- weights_df_1981 %>% right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD")) 

#Remove all of the duplicated lines to make it easier to use these weights
weights_df_1981 <- weights_df_1981 %>% distinct() %>% arrange(lsoa21)

#List of ED's in more than one LSOA 21
summary_split_ED81 <- weights_df_1981 %>% filter(prop_ED != 1) %>% select(-prop_ED) %>% 
  distinct() %>% group_by(ED81CDO) %>% summarise(count = n())

#Proportion of EDs requiring to be split 
nrow(summary_split_ED81)/nrow(weights_df_1981 %>% select(ED81CDO) %>% distinct())

######################################################################################################
### Repeat with 1971 - note that some EDs may be partially outside the 2021 city
######################################################################################################

weights_df_1971 <- st_drop_geometry(Birmingham_postcodes) %>% 
  mutate(ED71ZCD = as.factor(ED71ZCD), lsoa21 = as.factor(lsoa21)) %>%
  filter(active_1981 == 1, req_91 == 1) %>% group_by(ED71ZCD) %>% 
  mutate(total_pc_in_ED71 = sum(active_1981)) %>% ungroup()

weights_df_1971 <- weights_df_1971 %>% group_by(lsoa21, ED71ZCD) %>% 
  mutate(prop_ED = sum(active_1981)/total_pc_in_ED71) %>% select(lsoa21, ED71ZCD, prop_ED) #Corrected 9th May

#At this stage these should add to one as they cover all of the 1971 EDs needed
test <- weights_df_1971 %>% filter(prop_ED != 1) %>% distinct() %>% group_by(ED71ZCD) %>% summarize(check = sum(prop_ED))
sum(test$check) == nrow(test)

#Remove all of LSOAs not in 2021 geometry (added 13th May)
weights_df_1971 <- weights_df_1971 %>% right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD"))

#Remove all of the duplicated lines to make it easier to use these weights
weights_df_1971 <- weights_df_1971 %>% distinct() %>% arrange(lsoa21)

#List of ED's in more than one LSOA 21
summary_split_ED71 <- weights_df_1971 %>% filter(prop_ED != 1) %>% select(-prop_ED) %>% 
  distinct() %>% group_by(ED71ZCD) %>% summarise(count = n())

#Proportion of EDs requiring to be split 
nrow(summary_split_ED71)/nrow(weights_df_1971 %>% select(ED71ZCD) %>% distinct())

######################################

# Finally (added 9th May) we need a look up to match the 2021 AOs to 2021 LSOA as the data was downloaded for the smaller OAs
# Amended 13th May due to duplicates and to cover only the LSOAs inside the Birmingham 2021 boundaries. 

weights_df_2021 <- st_drop_geometry(Birmingham_postcodes) %>% 
  right_join(List_2021_LSOA, by = c("lsoa21" = "LSOA21CD")) %>% 
  select(oa21, lsoa21) %>% mutate(prop = 1) %>% distinct()

######################################################################################################
### Create weight .csv files for easier use with aggregation 
######################################################################################################

write_csv(weights_df_2021, "weights_2021.csv")
write_csv(weights_df_2011, "weights_2011.csv")
write_csv(weights_df_2001, "weights_2001.csv")
write_csv(weights_df_1991, "weights_1991.csv")
write_csv(weights_df_1981, "weights_1981.csv")
write_csv(weights_df_1971, "weights_1971.csv")

