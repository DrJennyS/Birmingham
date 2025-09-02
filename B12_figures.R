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

# MSOA 084 contains 4 LSOAs (A, B, C, F) in 2021
Birmingham_084 <- c("E01009366", "E01009374", "E01009376", "E01033648")

# All Birmingham postcodes
All_postcodes <- read_sf("My_pc_lookup.shp")

# Add activity flags so only plot postcodes that existed at the correct time points
All_postcodes  <- All_postcodes  %>% 
  mutate(ac_2021 = ifelse((doterm > 202103|is.na(doterm)) & dointr <= 202103, 1, 0)) %>%
  mutate(ac_2011 = ifelse((doterm > 201103|is.na(doterm)) & dointr <= 201103, 1, 0)) %>%
  mutate(ac_2001 = ifelse((doterm > 200104|is.na(doterm)) & dointr <= 200104, 1, 0)) %>%
  mutate(ac_1991 = ifelse((doterm > 199104|is.na(doterm)) & dointr <= 199104, 1, 0) ) %>%
  mutate(ac_1981 = ifelse((doterm > 198104|is.na(doterm)) & dointr <= 198104, 1, 0) )

# Postcodes within LSOA 084 in 2021
Area084_postcodes <- All_postcodes %>% filter(lsoa21 %in% Birmingham_084) %>% st_drop_geometry()

# Lists of (partially) covered sub-regions for each year after 2001
List_OA_2021 <- st_drop_geometry(Area084_postcodes) %>% filter(ac_2021 == 1) %>% select(oa21) %>% distinct() %>% pull()
List_OA_2011 <- st_drop_geometry(Area084_postcodes) %>% filter(ac_2011 == 1) %>% select(oa11) %>% distinct() %>% pull()
List_OA_2001 <- st_drop_geometry(Area084_postcodes) %>% filter(ac_2001 == 1) %>% select(oa01) %>% distinct() %>% pull()
# These three lists contain the same 18 elements. 

# Are there postcodes in these OAs that are not in Birmingham 084 in 2001?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(oa01 %in% List_OA_2001) %>% filter(ac_2001 == 1)
test_postcodes %>% anti_join(Area084_postcodes) # This is empty indicating that there is no boundary changes between 2021 and 2001
# Repeating for 2011 has the same impact. 

# Lists of (partially) covered sub-regions for each year before and including 1991
List_ED_1991 <- st_drop_geometry(Area084_postcodes) %>% filter(ac_1991 == 1) %>% select(ED91CD) %>% distinct() %>% pull()
List_ED_1981 <- st_drop_geometry(Area084_postcodes) %>% filter(ac_1981 == 1) %>% select(ED81CDO) %>% distinct() %>% pull()
List_ED_1971 <- st_drop_geometry(Area084_postcodes) %>% filter(dointr == 198001) %>% select(ED71ZCD) %>% distinct() %>% pull()

# Are there postcodes in these 1991 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED91CD %in% List_ED_1991) %>% filter(ac_1991 == 1)
extra_postcodes <- test_postcodes %>% anti_join(Area084_postcodes)

Area084_postcodes_ext <- bind_rows(Area084_postcodes, extra_postcodes)

# Are there postcodes in these 1981 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED81CDO %in% List_ED_1981) %>% filter(ac_1981 == 1)
extra_postcodes <- test_postcodes %>% anti_join(Area084_postcodes_ext)

Area084_postcodes_ext <- bind_rows(Area084_postcodes_ext, extra_postcodes)

# Are there postcodes in these 1971 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED71ZCD %in% List_ED_1971) %>% filter(dointr == 198001)
extra_postcodes <- test_postcodes %>% anti_join(Area084_postcodes_ext)

Area084_postcodes_ext <- bind_rows(Area084_postcodes_ext, extra_postcodes) 

# Add indicators about which OAs and EDs to use so can get rid of vectors without causing problems

Area084_postcodes_ext <- Area084_postcodes_ext %>% mutate(In_original_area = ifelse(oa21 %in% List_OA_2021, 1, 0)) %>%
  mutate(req_91 = ifelse(ED91CD %in% List_ED_1991, 1, 0)) %>% 
  mutate(req_81 = ifelse(ED81CDO %in% List_ED_1981, 1, 0)) %>% 
  mutate(req_71 = ifelse(ED71ZCD %in% List_ED_1971, 1, 0)) 

# Turn back into a shape file and save this file
Area084_postcode_sf <- st_as_sf(Area084_postcodes_ext , coords = c("long","lat")) %>% 
  select(pcd, ED71ZCD, ED81CDO, ED91CD, oa01, oa11, oa21, lsoa21, LSOA21N, ac_2021, ac_2011, ac_2001, ac_1991, ac_1981, dointr, In_original_area, req_91, req_81, req_71)

# Change coordinate reference system to match boundary files
st_crs(Area084_postcode_sf) <- 4326  # WGS84
Area084_postcode_sf <- st_transform(Area084_postcode_sf, crs = 27700)

# Save this smaller shape file
#st_write(Area084_postcode_sf, "Birmingham_084_pcds.shp", driver = "ESRI Shapefile")

########################################################################################
## Postcode points within Birmingham 084 - This is part of B12 ##
########################################################################################

Area084_postcodes <- read_sf("Birmingham_084_pcds.shp") 

# One of the names gets mangled, lets turn it back into something sensible!
Area084_postcodes <- Area084_postcodes %>% rename(in_orig = In_rgn_) 

# Lists of (partially) covered sub-regions for each year before and including 1991
List_ED_1991 <- st_drop_geometry(Area084_postcodes) %>% filter(req_91 == 1) %>% select(ED91CD) %>% distinct() %>% pull()
List_ED_1981 <- st_drop_geometry(Area084_postcodes) %>% filter(req_81 == 1) %>% select(ED81CDO) %>% distinct() %>% pull()
List_ED_1971 <- st_drop_geometry(Area084_postcodes) %>% filter(req_71 == 1) %>% select(ED71ZCD) %>% distinct() %>% pull()

# LSOA21 codes for MSOA 084
Birmingham_084 <- c("E01009366", "E01009374", "E01009376", "E01033648")

# 2021 LSOA boundaries for the Birmingham 084 area
Area084_LSOA_Boundaries_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084")) %>%
  # Add labels of codes
  mutate(Area = str_extract(LSOA21NM, pattern = "\\w$"))

# 2021 OA boundaries for the Birmingham 084 area
Area084_OA_Boundaries_2021 <- read_sf("OA_2021_EW_BFE_V9.shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084"))

# Other OA and ED Level geometries
Boundaries_1981 <- read_sf("ED_1981_EW.shp") %>% filter(ED81CDO %in% List_ED_1981) %>% mutate(ED81CDO = as.factor(ED81CDO))
Boundaries_1971 <- read_sf("ED_1971_EW.shp") %>% filter(ED71ZCD %in% List_ED_1971) %>% mutate(ED71ZCD = as.factor(ED71ZCD))
Boundaries_1991 <- read_sf("england_ed_1991.shp") %>% filter(label %in% List_ED_1991) %>% rename(ED91CD = label) %>% mutate(ED91CD = as.factor(ED91CD))

# Rough sketch of boundaries

#plot(st_geometry(Area084_LSOA_Boundaries_2021))
#plot(st_geometry(Area084_OA_Boundaries_2021))
#plot(st_geometry(Boundaries_1991))
#plot(st_geometry(Boundaries_1981))
#plot(st_geometry(Boundaries_1971))

# tmaps will be better - sketch options for good, bad, ugly posters!

#### Dreadful version ####

Birmingham_084_mapA <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "red", lwd = 2, lty = "dotted") + 
  tm_shape(Boundaries_1981) + tm_polygons(col = "white", border.col = "blue", lwd = 2, lty = "dashed") +
  tm_shape(Boundaries_1981) + tm_polygons(col = "white", border.col = "darkgreen", lwd = 2, lty = "dotted") +
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2, l) + 
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) 

Birmingham_084_mapB <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "red", lwd = 2, lty = "dotted") + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2, l) + 
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) + 
  tm_shape(Area084_postcodes) + tm_dots(fill = "ac_1991", size = 0.25) + tm_layout(legend.show = FALSE)

tmap_arrange(Birmingham_084_mapA, Birmingham_084_mapB, ncol = 2) 


#### Bad/Medium version ####

# Base map without text
Birmingham_084_map <- tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) 


# 2021 boundaries LSOA boundaries with OA boundaries inside
Birmingham_084_map0 <- tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_text("Area", size = 1, col =  "black", fontface = "bold", bg.color = "v", bg.alpha = 0.7) +
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) 
#Birmingham_084_map 

# 1991 Active Postcodes within 2021 Boundaries
Active_postcodes <- Area084_postcodes %>% filter(ac_1991 == 1 & in_orig == 1)
Birmingham_084_map1 <- Birmingham_084_map + tm_shape(Active_postcodes) + tm_dots(size = 0.25) 

# 2021 Boundaries overlayed on 1991 
Birmingham_084_map2 <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
 tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
 tm_text("Area", size = 1, col =  "black", fontface = "bold", bg.color = "cadetblue1", bg.alpha = 0.5)

# 1991 overlapping postcodes on previous map
Active_postcodes <- Area084_postcodes %>% filter(ac_1991 == 1 & req_91 == 1) %>% mutate(in_orig = as.factor(in_orig))

Birmingham_084_map3 <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_text("Area", size = 1, col =  "black", fontface = "bold", bg.color = "cadetblue1", bg.alpha = 0.5) +
  tm_shape(Active_postcodes) + tm_dots(fill = "black", size = 0.25) + tm_layout(legend.show = FALSE)

# Distinguish between outside and inside 084
Birmingham_084_map4 <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_text("Area", size = 1, col =  "black", fontface = "bold", bg.color = "cadetblue1", bg.alpha = 0.5) +
  tm_shape(Active_postcodes) + tm_dots(fill = "in_orig", size = 0.25) + tm_layout(legend.show = FALSE)

# Distinguish between outside and inside 084
Birmingham_084_map5 <- tm_shape(Boundaries_1991) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.25, border.col = "black", lwd = 2) + 
  tm_text("Area", size = 1, col =  "black", fontface = "bold", bg.color = "cadetblue1", bg.alpha = 0.7) +
  tm_shape(Active_postcodes) + tm_dots(fill = "ED91CD", size = 0.25) + tm_layout(legend.show = FALSE)

tmap_arrange(Birmingham_084_map0, Birmingham_084_map1, Birmingham_084_map2, Birmingham_084_map3, Birmingham_084_map4, Birmingham_084_map5, ncol = 3) 

######################################
#### Best version using only 084C ####
######################################

Birmingham_084C <- "E01009376"

# All Birmingham postcodes
All_postcodes <- read_sf("My_pc_lookup.shp")

# Add activity flags so only plot postcodes that existed at the correct time points
All_postcodes  <- All_postcodes  %>% 
  mutate(ac_2021 = ifelse((doterm > 202103|is.na(doterm)) & dointr <= 202103, 1, 0)) %>%
  mutate(ac_2011 = ifelse((doterm > 201103|is.na(doterm)) & dointr <= 201103, 1, 0)) %>%
  mutate(ac_2001 = ifelse((doterm > 200104|is.na(doterm)) & dointr <= 200104, 1, 0)) %>%
  mutate(ac_1991 = ifelse((doterm > 199104|is.na(doterm)) & dointr <= 199104, 1, 0) ) %>%
  mutate(ac_1981 = ifelse((doterm > 198104|is.na(doterm)) & dointr <= 198104, 1, 0) )

# Postcodes within LSOA 084 in 2021
Area084C_postcodes <- All_postcodes %>% filter(lsoa21 == Birmingham_084C) %>% st_drop_geometry()

# Lists of (partially) covered sub-regions for each year after 2001
List_OA_2021 <- st_drop_geometry(Area084C_postcodes) %>% filter(ac_2021 == 1) %>% select(oa21) %>% distinct() %>% pull()
List_OA_2011 <- st_drop_geometry(Area084C_postcodes) %>% filter(ac_2011 == 1) %>% select(oa11) %>% distinct() %>% pull()
List_OA_2001 <- st_drop_geometry(Area084C_postcodes) %>% filter(ac_2001 == 1) %>% select(oa01) %>% distinct() %>% pull()
# These three lists contain the same elements. 

# Are there postcodes in these OAs that are not in Birmingham 084 in 2001?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(oa01 %in% List_OA_2001) %>% filter(ac_2001 == 1)
test_postcodes %>% anti_join(Area084C_postcodes) # This is empty indicating that there is no boundary changes between 2021 and 2001
# Repeating for 2011 has the same impact. 

# Lists of (partially) covered sub-regions for each year before and including 1991
List_ED_1991 <- st_drop_geometry(Area084C_postcodes) %>% filter(ac_1991 == 1) %>% select(ED91CD) %>% distinct() %>% pull()
List_ED_1981 <- st_drop_geometry(Area084C_postcodes) %>% filter(ac_1981 == 1) %>% select(ED81CDO) %>% distinct() %>% pull()
List_ED_1971 <- st_drop_geometry(Area084C_postcodes) %>% filter(dointr == 198001) %>% select(ED71ZCD) %>% distinct() %>% pull()

# Are there postcodes in these 1981 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED81CD %in% List_ED_1991) %>% filter(ac_1991 == 1)
extra_postcodes <- test_postcodes %>% anti_join(Area084C_postcodes)

Area084C_postcodes_ext <- bind_rows(Area084C_postcodes, extra_postcodes)

# Are there postcodes in these 1981 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED81CDO %in% List_ED_1981) %>% filter(ac_1981 == 1)
extra_postcodes <- test_postcodes %>% anti_join(Area084C_postcodes_ext)

Area084C_postcodes_ext <- bind_rows(Area084C_postcodes_ext, extra_postcodes)

# Are there postcodes in these 1971 EDs that are not in Birmingham 084 in 2021?
test_postcodes <- st_drop_geometry(All_postcodes) %>% filter(ED71ZCD %in% List_ED_1971) %>% filter(dointr == 198001)
extra_postcodes <- test_postcodes %>% anti_join(Area084C_postcodes_ext)

Area084C_postcodes_ext <- bind_rows(Area084C_postcodes_ext, extra_postcodes) 

# Add indicators about which OAs and EDs to use so can get rid of vectors without causing problems

Area084C_postcodes_ext <- Area084C_postcodes_ext %>% mutate(In_original_area = ifelse(oa21 %in% List_OA_2021, 1, 0)) %>%
  mutate(req_91 = ifelse(ED91CD %in% List_ED_1991, 1, 0)) %>% 
  mutate(req_81 = ifelse(ED81CDO %in% List_ED_1981, 1, 0)) %>% 
  mutate(req_71 = ifelse(ED71ZCD %in% List_ED_1971, 1, 0)) 

# Turn back into a shape file and save this file
Area084C_postcode_sf <- st_as_sf(Area084C_postcodes_ext , coords = c("long","lat")) %>% 
  select(pcd, ED71ZCD, ED81CDO, ED91CD, oa01, oa11, oa21, lsoa21, LSOA21N, ac_2021, ac_2011, ac_2001, ac_1991, ac_1981, dointr, In_original_area, req_91, req_81, req_71)

# Change coordinate reference system to match boundary files
st_crs(Area084C_postcode_sf) <- 4326  # WGS84
Area084C_postcode_sf <- st_transform(Area084C_postcode_sf, crs = 27700)

# Save this smaller shape file
#st_write(Area084C_postcode_sf, "Birmingham_084C_pcds.shp", driver = "ESRI Shapefile")

##### CAN RE-RUN ONLY FROM HERE :) ######

Area084_postcodes <- read_sf("Birmingham_084C_pcds.shp") 

# One of the names gets mangled, lets turn it back into something sensible!
Area084_postcodes <- Area084_postcodes %>% rename(in_orig = In_rgn_) 

# Lists of (partially) covered sub-regions for each year before and including 1991
List_ED_1991 <- st_drop_geometry(Area084_postcodes) %>% filter(req_91 == 1) %>% select(ED91CD) %>% distinct() %>% pull()
List_ED_1981 <- st_drop_geometry(Area084_postcodes) %>% filter(req_81 == 1) %>% select(ED81CDO) %>% distinct() %>% pull()
List_ED_1971 <- st_drop_geometry(Area084_postcodes) %>% filter(req_71 == 1) %>% select(ED71ZCD) %>% distinct() %>% pull()

Birmingham_084c <-"E01009376"

# 2021 LSOA boundaries for the Birmingham 084 area
Area084_LSOA_Boundaries_2021 <- read_sf("Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084C"))

# 2021 OA boundaries for the Birmingham 084 area
Area084_OA_Boundaries_2021 <- read_sf("OA_2021_EW_BFE_V9.shp") %>% filter(str_detect(LSOA21NM, "Birmingham 084C"))

# Other OA and ED Level geometries
Boundaries_1981 <- read_sf("ED_1981_EW.shp") %>% filter(ED81CDO %in% List_ED_1981) %>% mutate(ED81CDO = as.factor(ED81CDO))
Boundaries_1971 <- read_sf("~/R/Birmingham&Walsall/Week3/ED_1971_EW.shp") %>% filter(ED71ZCD %in% List_ED_1971) %>% mutate(ED71ZCD = as.factor(ED71ZCD))
Boundaries_1991 <- read_sf("england_ed_1991.shp") %>% filter(label %in% List_ED_1991) %>% rename(ED91CD = label) %>% mutate(ED91CD = as.factor(ED91CD))

### NICE maps
# Base map without text
Birmingham_084_map <- tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) 


# 2021 boundaries LSOA boundaries with OA boundaries inside
Birmingham_084_map0 <- tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_shape(Area084_OA_Boundaries_2021) + tm_borders(lwd = 1) #+ 
  #tm_title("test title", color = "#39464D")
#tm_layout(main.title = "2021 OA Boundaries within Birmingham 084C", main.title.size = 1.2, title.position = c("center", "bottom"))
#Birmingham_084_map 

# 1971 Active Postcodes within 2021 Boundaries
Active_postcodes <- Area084_postcodes %>% filter(ac_1991 == 1 & in_orig == 1)
Birmingham_084_map1 <- Birmingham_084_map + tm_shape(Active_postcodes) + tm_dots(size = 0.5) 

# 2021 Boundaries overlayed on 1991 
Birmingham_084_map2 <- tm_shape(Boundaries_1971) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2)

# 1971 overlapping postcodes on previous map
Active_postcodes <- Area084_postcodes %>% filter(req_71 == 1 & dointr == 198001) %>% mutate(in_orig = as.factor(in_orig))

Birmingham_084_map3 <- tm_shape(Boundaries_1971) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_shape(Active_postcodes) + tm_dots(fill = "black", size = 0.5) + tm_layout(legend.show = FALSE)

# Distinguish between outside and inside 084
Birmingham_084_map4 <- tm_shape(Boundaries_1971) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.5, border.col = "black", lwd = 2) + 
  tm_shape(Active_postcodes) + tm_dots(fill = "in_orig", col = "in_orig", size = 0.5, palette = "brewer.paired") + tm_layout(legend.show = FALSE)

# Distinguish between outside and inside 084 within each ED
#Birmingham_084_map5 <- tm_shape(Boundaries_1971) + tm_polygons(col = "white", border.col = "black", lwd = 1) + 
#  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "cadetblue1", alpha = 0.25, border.col = "black", lwd = 2) + 
#  tm_shape(Active_postcodes) + tm_dots(fill = "ED71ZCD", col = "ED71ZCD", size = 0.5, palette = "brewer.dark2") + tm_layout(legend.show = FALSE)

#tmap_arrange(Birmingham_084_map0, Birmingham_084_map1, Birmingham_084_map2, Birmingham_084_map3, Birmingham_084_map4, Birmingham_084_map5, ncol = 3) 

############################################

## Percentage of each ED in 2021 LSOA 
weights_1971 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_1971.csv") %>% rename(prop = prop_ED)
Boundaries_1971 <- Boundaries_1971 %>% left_join(weights_1971 %>% filter(lsoa21 == Birmingham_084C)) 
Boundaries_1971 <- Boundaries_1971 %>% mutate(Proportion = round(prop, 2))

Birmingham_084_map6 <- tm_shape(Boundaries_1971) + tm_polygons(fill = "Proportion", border.col = "black", lwd = 1, palette = "brewer.blues", alpha = 0.8) + 
  tm_text("Proportion", size = 0.5, col =  "black", fontface = "bold") + tm_layout(legend.position = c("right", "top"), legend.text.size = 0.5,      # Legend text size
  legend.title.size = 0.5)      

#Birmingham_084_map6

## Add on Manufacturing data
Boundaries_1971 <- Boundaries_1971 %>% left_join(Manufacturing_1971_area084, by = c("ED71ZCD" = "zone_code")) # Note this line uses df from "Changing geography figures.R"
Boundaries_1971 <- Boundaries_1971 %>% mutate(Manufacturing = round(prop_manufacturing, 2))

Birmingham_084_map7 <- tm_shape(Boundaries_1971) + tm_polygons(fill = "Manufacturing", border.col = "black", lwd = 1, palette = "brewer.blues") + 
  tm_shape(Area084C_LSOA_Boundaries_2021) + tm_polygons(col = "white", alpha = 0.1, border.col = "black", lwd = 2) + 
  tm_layout(legend.position = c("right", "top"), legend.text.size = 0.5, legend.title.size = 0.5)
   
#Birmingham_084_map7

## Re-aggreated
setwd("~/R/Birmingham&Walsall/Week4") 
Manufacturing_agg <- read_csv("Manufacturing_aggregated.csv") %>% filter(year == 1971 & lsoa21 == Birmingham_084C)

agg_prop <- round(Manufacturing_agg$Manufacturing / Manufacturing_agg$Total_employed, 2)
Area084_LSOA_Boundaries_2021 <- Area084_LSOA_Boundaries_2021 %>% mutate(agg_prop = agg_prop)

Birmingham_084_map8 <- tm_shape(Boundaries_1971) + tm_polygons(col = "white", border.col = "white") + 
  tm_shape(Area084_LSOA_Boundaries_2021) + tm_polygons(col = "agg_prop", border.col = "black", lwd = 2, palette = "brewer.blues") + 
  tm_text("agg_prop", size = 0.9, col =  "black", fontface = "bold")  + tm_layout(legend.show = FALSE)

### Run from line 255 to here in order to create map-figures for RMD file....
tmap_options(outer.bg = TRUE, outer.bg.color = "#D1D9E3")
# Titles are ugly here
panel_maps <- tmap_arrange(Birmingham_084_map0 + tm_title("Step 1: Census OA boundaries 2021.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map1 + tm_title("Step 2: Postcodes active in 1971.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map2 + tm_title("Step 3: Overlay low-level 1971 census boundaries.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map3 + tm_title("Step 4: Identify postcodes in overlapping area.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map4 + tm_title("Step 5: Label the postcodes within the 2021 boundary.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map6 + tm_title("Step 6: Calculate the proportion of postcodes that overlap.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map7 + tm_title("Step 7: Proportion of employed residents working in \n the manufacturing sector from 1971 census.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             Birmingham_084_map8 + tm_title("Step 8: Counts are aggregated over the 2021 LSOA \n to calculate the proportion of employed residents \n working in the manufacturing sector.", color = "#174273", size = 0.8) + tmap_options(component.autoscale = FALSE), #+ tm_layout(title.position = c("right", "top"), title.bg.color = "white", title.bg.alpha = 0.8),
             ncol = 2, 
             nrow = 4, asp = 0, widths = c(0.5, 0.5),    # relative widths
             heights = c(0.2, 0.2, 0.2, 0.3))   # relative heights)

tmap_save(panel_maps, filename = "~/R/Birmingham&Walsall/Poster_materials/test.png", width = 4000, height = 4000, dpi = 500)
