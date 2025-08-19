library(sf)
library(ggplot2)
library(tidyverse)
library(readr)
library(corrplot)
library(tmap)
library(RColorBrewer)
library(spdep)
library(spgwr)
library(kableExtra)

# Load weights file for 1971 and 2021

weights_1971 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_1971.csv") %>% rename(prop = prop_ED)
weights_2021 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_2021.csv")

# Load original data
setwd("~/R/Birmingham&Walsall/Week4") 

Manufacturing_1971 <- read_csv("Manufacturing_1971.csv")
Manufacturing_2021 <- read_csv("Manufacturing_2021.csv")
Townsend_1971 <- read_csv("Townsend_1971.csv")
Townsend_2021 <- read_csv("Townsend_2021.csv")

# Load reaggregated data
Manufacturing_agg <- read_csv("Manufacturing_aggregated.csv")
Townsend_agg <- read_csv("Townsend_2021.csv")

##########################
#### TERRIBLE VERSION ####
##########################

setwd("~/R/Birmingham&Walsall/Week3") 

# 2021 LSOA boundaries for the Birmingham
Birmingham_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham"))

# ED Level geometry overlapping with city boundaries in 2021
Boundaries_1971 <- read_sf("ED_1971_EW.shp")
Boundaries_1971_in_Birmingham <- st_filter(Boundaries_1971, Birmingham_2021, .predicate = st_intersects)

## Graph on 1971 boundaries 
#create proportions first 
Manufacturing_1971 <- Manufacturing_1971 %>% mutate(prop_manufacturing = Manufacturing/Total_employed) %>% select(zone_code, prop_manufacturing)

geographical_Manufacturing <- Boundaries_1971_in_Birmingham %>% right_join(Manufacturing_1971, by = c("ED71ZCD" = "zone_code"))

tm_shape(geographical_Manufacturing) + tm_fill(col = "prop_manufacturing", style = "cont", palette = "brewer.blues") + tm_borders(alpha = 0.4)

#####################
#### BAD VERSION ####
#####################

setwd("~/R/Birmingham&Walsall/Week3") 
Area084_postcodes <- read_sf("Birmingham_084_pcds.shp") 

##### Restricted boundaries for illustrative purposes

# One of the names gets mangled, lets turn it back into something sensible!
Area084_postcodes <- Area084_postcodes %>% rename(in_orig = In_rgn_) 

# Lists of (partially) covered sub-regions for 1971
List_ED_1971 <- st_drop_geometry(Area084_postcodes) %>% filter(req_71 == 1) %>% select(ED71ZCD) %>% distinct() %>% pull()

# LSOA21 codes for MSOA 084
Birmingham_084 <- c("E01009366", "E01009374", "E01009376", "E01033648")

# 2021 LSOA boundaries for the Birmingham 084 area
Area084_LSOA_Boundaries_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084")) 

# 2021 OA boundaries for the Birmingham 084 area
Area084_OA_Boundaries_2021 <- read_sf("OA_2021_EW_BFE_V9.shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084"))

# 1971 ED geometries
Boundaries_1971_084 <- read_sf("ED_1971_EW.shp") %>% filter(ED71ZCD %in% List_ED_1971) %>% mutate(ED71ZCD = as.factor(ED71ZCD))

#### Restricted graphs 

Manufacturing_1971_area084 <- Manufacturing_1971 %>% filter(zone_code %in% List_ED_1971) %>% filter(zone_code != "e39106406034") #remove NaN field

geographical_Manufacturing084 <- Boundaries_1971_084 %>% right_join(Manufacturing_1971_area084, by = c("ED71ZCD" = "zone_code"))

## Using 1971 boundaries
tm_shape(geographical_Manufacturing084) + tm_fill(col = "prop_manufacturing", style = "cont", palette = "brewer.blues") + tm_borders(alpha = 0.4) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_borders(lwd = 2)

Manufacturing_1971_area084new <- Manufacturing_agg %>% filter(year == 1971) %>% filter(lsoa21 %in% Birmingham_084) %>% 
  mutate(prop_manufacturing = Manufacturing/Total_employed) %>% select(lsoa21, prop_manufacturing)

geographical_Manufacturing084new <- Area084_LSOA_Boundaries_2021 %>% right_join(Manufacturing_1971_area084new , by = c("LSOA21CD" = "lsoa21"))

## Using 2021 boundaries
tm_shape(geographical_Manufacturing084new) + tm_fill(col = "prop_manufacturing", style = "cont", palette = "brewer.blues") + tm_borders(lwd = 2)

######################
#### GOOD VERSION ####
######################

setwd("~/R/Birmingham&Walsall/Week3") 
Birmingham_084C <- "E01009376"

Area084C_postcodes <- read_sf("Birmingham_084C_pcds.shp") 

# One of the names gets mangled, lets turn it back into something sensible!
Area084C_postcodes <- Area084C_postcodes %>% rename(in_orig = In_rgn_) 

# Lists of (partially) covered sub-regions for each year before and including 1991
List_ED_1971C <- st_drop_geometry(Area084C_postcodes) %>% filter(req_71 == 1) %>% select(ED71ZCD) %>% distinct() %>% pull()

Birmingham_084c <-"E01009376"

# 2021 LSOA boundaries for the Birmingham 084 area
Area084C_LSOA_Boundaries_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084C"))

# 2021 OA boundaries for the Birmingham 084 area
Area084C_OA_Boundaries_2021 <- read_sf("OA_2021_EW_BFE_V9.shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084C"))

# Other OA and ED Level geometries
Boundaries_1971C <- read_sf("ED_1971_EW.shp") %>% filter(ED71ZCD %in% List_ED_1971C) %>% mutate(ED71ZCD = as.factor(ED71ZCD))

# Graphs on restricted region 

Graph_variable(proportions_manual, 1971, "prop_manufacturing")
