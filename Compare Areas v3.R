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

#### Load 2021 boundaries 

All_areas_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp")

#Area_014 <- All_areas_2021 %>% filter(str_detect(LSOA21NM, "Birmingham 014")) %>% 
#  mutate(sub_Area = str_extract(LSOA21NM, "[A-Z]$"))

#plot(st_geometry(Area_014))

#### Load Postcode data

All_postcodes <- read_csv("ONSPD_FEB_2025_UK_B.csv") %>% filter(usertype == 0) %>% #small users are typically residential
  select(pcd, dointr, doterm, oa01, lsoa01, oa11, lsoa11, oa21, lsoa21, x = oseast1m, y = osnrth1m, lat, long ) # retain only relevant cols

# Convert to shape file with the same coordinate reference system
All_postcode_sf <- st_as_sf(All_postcodes, coords = c("x","y"))
#st_crs(All_postcode_sf) <- st_crs(Area_014) 

# Join to the reference geography polygon
#Postcodes_within_Birmingham014 <- st_join(All_postcode_sf, Area_014, join = st_within) %>% filter(!is.na(LSOA21CD)) 

#Plot onto 2021 LSOA map
#tm_shape(Area_014) + tm_borders(fill_alpha=.4) +
#  tm_shape(Postcodes_within_Birmingham014) + tm_dots(fill = "red", size = 0.7)  

#Version for Birmingham in general 

Birmingham_2021 <- All_areas_2021 %>% filter(str_detect(LSOA21NM, "Birmingham")) 

plot(st_geometry(Birmingham_2021))

st_crs(All_postcode_sf) <- st_crs(Birmingham_2021) 

#############################################################################################################

# Join to the reference geography polygon - In this case check if any removed!!
B_postcodes <- st_join(All_postcode_sf, Birmingham_2021, join = st_within)
Postcodes_within_Birmingham <- B_postcodes %>% filter(!is.na(LSOA21CD)) 

#Add indicator for whether inside city boundaries for purpose of census
B_postcodes <- B_postcodes %>% mutate(in_city = ifelse(is.na(LSOA21CD), 0, 1))

#Plot only the census postcodes onto 2021 LSOA map
tm_shape(Birmingham_2021) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham) + tm_dots(fill = "red", size = 0.05)

#Plot all postcodes indicating which ones are in the city
tm_shape(Birmingham_2021) + tm_borders(fill_alpha=.4) +
  tm_shape(B_postcodes) + tm_dots(fill = "in_city", size = 0.05)

#Count Birmingham postcodes not inside the 2021 census area
no_match <- B_postcodes[is.na(B_postcodes$LSOA21CD), ] %>% mutate(pc_area = str_extract(pcd, pattern = "^[A-Z][0-9]{2}"))

as.data.frame(no_match) %>% group_by(pc_area) %>% summarise(unused_postcodes = n()) %>% arrange(desc(unused_postcodes))

# Check LSOA 2021 data in postcodes coincides with areas derived from map
1-mean(Postcodes_within_Birmingham$lsoa21 == Postcodes_within_Birmingham$LSOA21CD) # Proportion different 

sum(Postcodes_within_Birmingham$lsoa21 != Postcodes_within_Birmingham$LSOA21CD) #Actual number that differ

Postcodes_within_Birmingham[(Postcodes_within_Birmingham$lsoa21 != Postcodes_within_Birmingham$LSOA21CD),] #List of postcodes that differ
#Note that only one of these is relevant as the others were introduced AFTER March 2021

#############################################################################################################
#### Link postcode to 1981 boundary and plot postcodes on 1981 boundaries 
#############################################################################################################

# Load 1981 ED boundaries
Boundaries_1981 <- read_sf("ED_1981_EW.shp")
Boundaries_1971 <- read_sf("ED_1971_EW.shp")
Boundaries_1991 <- read_sf("england_ed_1991.shp")

#Retain only the boundaries which are entirely or partial within Birmingham 2021 census area 
Boundaries_1981_in_Birmingham <- st_filter(Boundaries_1981, Birmingham_2021, .predicate = st_intersects)
Boundaries_1971_in_Birmingham <- st_filter(Boundaries_1971, Birmingham_2021, .predicate = st_intersects)

Boundaries_1991_in_Birmingham <- st_filter(Boundaries_1991, Birmingham_2021, .predicate = st_intersects) %>% select(-name) %>% rename(ED91CD = label)

plot(st_geometry(Boundaries_1981_in_Birmingham))
plot(st_geometry(Boundaries_1971_in_Birmingham))
plot(st_geometry(Boundaries_1991_in_Birmingham))

###### Only in this case we need to map th names of the wards onto the names of the enumeration districts ####

Wards_91 <- read_sf("england_wa_1991.shp") %>% rename(WD91CD = label, WD91NM = name)

#Simply joining this to the boundaries polygons is causing issues due to the boundaries not being the same level of accuracy so
# The EDs are not strictly nested within the Wards to get round this convert the 1991 boundaries into centroids
centroids_91 <- st_centroid(Boundaries_1991_in_Birmingham)

#Find the '91 Wards that contain each of these centoids
test <- st_join(centroids_91, Wards_91, .predicate = st_intersects) %>% st_drop_geometry()

#Join this data back onto the boundaries file
Boundaries_1991_in_Birmingham <- Boundaries_1991_in_Birmingham %>% left_join(test)

####################################################################################################

#All postcodes within 1981 boundaries (I am thinking that there might be postcodes in these EDs not in the current Birmingham boundaries)

B_postcodes_81 <- st_join(All_postcode_sf, Boundaries_1981_in_Birmingham, join = st_within) %>% 
#  filter(dointr <= 198104) %>% #restrict to postcodes active in 81
  mutate(in_city_81 = ifelse(is.na(LAT), 0, 1)) #Label those inside city boundaries

#These are the postcodes we will need to attach 1981 census data to
Postcodes_within_Birmingham_81 <- B_postcodes_81 %>% filter(in_city_81 == 1)

tm_shape(Boundaries_1981_in_Birmingham) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_81) + tm_dots(fill = "blue", size = 0.05) 

# Label the postcodes that will not contribute to the 2021 area but are needed as weights 
Postcodes_within_Birmingham_81 <- st_join(Postcodes_within_Birmingham_81, Birmingham_2021, join = st_within) %>% mutate(in_city_21 = !is.na(LSOA21CD))  

#Map these so the additional points needed for the weighting is clear
tm_shape(Boundaries_1981_in_Birmingham) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_81) + tm_dots(fill = "in_city_21", size = 0.05) 

tm_shape(Birmingham_2021) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_81) + tm_dots(fill = "in_city_21", size = 0.05) 

#Select only relevant columns - These are all of the Birmingham '81 postcodes that we need 
Postcodes_within_Birmingham_81 <- st_drop_geometry(Postcodes_within_Birmingham_81) %>% select(pcd, ED81CD:WD81NM,in_city_21) %>%
  mutate(req_81 = 1)

summary(Postcodes_within_Birmingham_81$in_city_21)

#####################################################################################################################

#Join 1981 ED information to points within the 2021 city boundaries 
Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% 
  left_join(Postcodes_within_Birmingham_81 %>% select(-in_city_21), by = "pcd") 

#We also need to add the rest of the points within the '81 boundaries for weighting purposes
test <- Postcodes_within_Birmingham_81 %>% anti_join(Postcodes_within_Birmingham, by = "pcd")
Additional_postcodes_81 <- test %>% left_join(All_postcode_sf, by = "pcd")

Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% full_join(Additional_postcodes_81)

#summary(Postcodes_within_Birmingham)

######################################################################################################################

## Extract list of relevant postcodes for 1971
B_postcodes_71 <- st_join(All_postcode_sf, Boundaries_1971_in_Birmingham, join = st_within) %>% 
#  filter(dointr <= 198104) %>% #restrict to postcodes active in 81
  mutate(in_city_71 = ifelse(is.na(LAT), 0, 1)) #Label those inside city boundaries

#These are the postcodes we will need to attach 1971 census data to
Postcodes_within_Birmingham_71 <- B_postcodes_71 %>% filter(in_city_71 == 1)

tm_shape(Boundaries_1971_in_Birmingham) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_71) + tm_dots(fill = "blue", size = 0.05) 

# Label the postcodes that will not contribute to the 2021 area but are needed as weights 
Postcodes_within_Birmingham_71 <- st_join(Postcodes_within_Birmingham_71, Birmingham_2021, join = st_within) %>% mutate(in_city_21 = !is.na(LSOA21CD))  

### This is a downloaded graph
#Map these so the additional points needed for the weighting is clear
tm_shape(Boundaries_1971_in_Birmingham) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_71) + tm_dots(fill = "in_city_21", size = 0.05) 

### This is a downloaded graph
tm_shape(Birmingham_2021) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_71) + tm_dots(fill = "in_city_21", size = 0.05) 

#Select only relevant columns - These are all of the Birmingham 81 postcodes
Postcodes_within_Birmingham_71 <- st_drop_geometry(Postcodes_within_Birmingham_71) %>% 
  select(pcd, ED71CD:WD71NM,in_city_21) %>% mutate(req_71 =1)

summary(Postcodes_within_Birmingham_71$in_city_21)

#####################################################################################################################

#Join 1971 ED information to points within the 2021 city boundaries 
Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% 
  left_join(Postcodes_within_Birmingham_71 %>% select(-in_city_21), by = "pcd") 

#We also need to add the rest of the points within the '71 boundaries for weighting purposes
test <- Postcodes_within_Birmingham_71 %>% anti_join(Postcodes_within_Birmingham, by = "pcd")
Additional_postcodes_71 <- test %>% left_join(All_postcode_sf, by = "pcd")

Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% full_join(Additional_postcodes_71)

############## Add 1991 ED codes using same approach ################################################################

B_postcodes_91 <- st_join(All_postcode_sf, Boundaries_1991_in_Birmingham, join = st_within) %>% 
  #  filter(dointr <= 198104) %>% #restrict to postcodes active in 81
  mutate(in_city_91 = ifelse(is.na(ED91CD), 0, 1)) #Label those inside city boundaries

summary(as.logical(B_postcodes_91$in_city_91))

#These are the postcodes we will need to attach 1991 census data to

Postcodes_within_Birmingham_91 <- B_postcodes_91 %>% filter(in_city_91 == 1)

# Label the postcodes that will not contribute to the 2021 area but are needed as weights 
Postcodes_within_Birmingham_91 <- st_join(Postcodes_within_Birmingham_91, Birmingham_2021, join = st_within) %>% mutate(in_city_21 = !is.na(LSOA21CD))  

#Map these so the additional points needed for the weighting is clear
tm_shape(Boundaries_1991_in_Birmingham) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_91) + tm_dots(fill = "in_city_21", size = 0.05) 

tm_shape(Birmingham_2021) + tm_borders(fill_alpha=.4) +
  tm_shape(Postcodes_within_Birmingham_91) + tm_dots(fill = "in_city_21", size = 0.05) 

#Select only relevant columns - These are all of the Birmingham '91 postcodes that we need 
Postcodes_within_Birmingham_91 <- st_drop_geometry(Postcodes_within_Birmingham_91) %>% select(pcd, ED91CD, WD91CD, in_city_21) %>%
  mutate(req_91 = 1)

summary(Postcodes_within_Birmingham_91$in_city_21)
#################################################################################

#Join 1991 ED & Ward information to points within the 2021 city boundaries 
Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% 
  left_join(Postcodes_within_Birmingham_91, by = "pcd") 

#We also need to add the rest of the points within the '91 boundaries for weighting purposes
test <- Postcodes_within_Birmingham_91 %>% anti_join(Postcodes_within_Birmingham, by = "pcd")
Additional_postcodes_91 <- test %>% left_join(All_postcode_sf, by = "pcd")

Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% full_join(Additional_postcodes_91)

#Replace NAs to make this more readable
Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% replace_na(list(in_city_21 = TRUE, req_81 = 0, req_71 = 0, req_91 =0))

####################### Export shape file so that do not have to keep running this!!! #################################

Postcodes_within_Birmingham <- Postcodes_within_Birmingham %>% select(pcd:long, LSOA21NM, ED81CD:req_81, ED71CD:in_city_21)
st_write(Postcodes_within_Birmingham, "My_pc_lookup.shp", driver = "ESRI Shapefile")







