library(tidyverse)
library(readr)
library(corrplot)
library(ggbiplot)


library(tidymodels)
library(infer)
library(devtools)

library(sf)
library(corrplot)
library(tmap)
library(spdep)
library(spgwr)
library(RColorBrewer)

setwd("~/R/Birmingham&Walsall/Week5") 
dir()

####################################
### Load clean and prepared data ###
####################################

Townsend_prepared <- read_csv("Townsend_prepared.csv") %>% filter(!if_any(everything(), ~is.na(.x)))
Manufacturing_prepared <- read_csv("Manufacturing_prepared.csv") %>% filter(!if_any(everything(), ~is.na(.x)))

#######################################
### Cluster Manufacturing 1971 data ###
#######################################

str(Manufacturing_prepared)
test_data <- Manufacturing_prepared %>% filter(year == 1971) %>% select(prop_manufacturing:prop_sick) %>% distinct()

kclust_manufacturing <- kmeans(test_data, centers = 3)

kclust_manufacturing
summary(kclust_manufacturing)

# This adds the cluster information back onto the data
augment(kclust_manufacturing, test_data)

# Per cluster level summary (averages of each variable, size of cluster and withinss)
tidy(kclust_manufacturing)

# Statistics total ss, within and between and number of iterations
glance(kclust_manufacturing)

# It's important that the data is a tibble ?!!?
data <- tibble(test_data)

kclusts <- tibble(k = 1:10) %>%
  mutate(kclust = map(k, ~kmeans(data, .x)), tidied = map(kclust, tidy), glanced = map(kclust, glance), augmented = map(kclust, augment, data))

# Note this is a 10 x 5 object with lists/tibbles in columns 2-5
kclusts 

# This contains a summary of the clusters for each value of k. There are n rows for n =3, 
clusters <- kclusts %>% unnest(cols = c(tidied))

# This contains all of the points for each k, labeled with the cluster they belong to
assignments <- kclusts %>% unnest(cols = c(augmented)) 

# This contains summary stats for each... 
clusterings <- kclusts %>% unnest(cols = c(glanced)) 

### Elbow Plot 
ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + geom_point()
# This suggests that the idea number of clusters is likely to be k =  2

# Lets join this back to the data-set containing LSOA21 labels with these clusters compare with Index

# This is a test case with k = 2
Manufacturing_1971_labelled <- Manufacturing_prepared %>% filter(year == 1971) %>% inner_join(assignments %>% filter(k == 2) %>% select(prop_manufacturing:.cluster))

Manufacturing_1971_labelled <- Manufacturing_prepared %>% filter(year == 1971) %>% 
  inner_join(assignments %>% select(n_cluster = k, prop_manufacturing:.cluster) %>% filter(n_cluster != 1) %>%
               mutate(n_cluster = str_c("k_", as.character(n_cluster))) %>%
               pivot_wider(names_from = "n_cluster", values_from = ".cluster") %>%
               mutate(across(where(is.factor), fct_drop)))

### Tabulate
table(Manufacturing_1971_labelled$k_5, Manufacturing_1971_labelled$Census_Quintile)

## TO DO: Examine features of composite elements for k = 2, k =3 and k = 5

### Graph on map

# Boundary information
Birmingham_2021 <- read_sf("~/R/Birmingham&Walsall/Week3/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% 
  filter(str_detect(LSOA21NM, "Birmingham")) 

geographical_data <- Birmingham_2021 %>% right_join(Manufacturing_1971_labelled, by = c("LSOA21CD" = "lsoa21"))

Categories_map <- tm_shape(geographical_data) + tm_fill(col = "k_3", style = "cat", palette = "brewer.pu_or") + tm_borders(alpha = 0.4)

tmap_save(Categories_map, 
          filename = "~/R/Birmingham&Walsall/Poster_materials/Categories_map.png",
          width = 3000,
          height = 3000,
          dpi = 300)

## Add Label - uses k = 3, Cluster 1: High Manufacturing = TRUE, Cluster 2 or 3: High_Manufacturing = FALSE

Manufacturing_1971_labelled <- Manufacturing_1971_labelled %>% mutate(Label = ifelse(k_3 == 1, TRUE, FALSE))

# Handy sanity check
# table(Manufacturing_1971_labelled$Label, Manufacturing_1971_labelled$k_2)

## Add these labels back onto the Townsend data

Townsend_prepared <- Townsend_prepared %>% left_join(Manufacturing_1971_labelled %>% select(lsoa21, Label))

### Split by Manufacturing in 1971 so can cluster within these groups ###

write_csv(Manufacturing_1971_labelled %>% select(lsoa21, k_2:k_5), "~/R/Birmingham&Walsall/Week5/labels.csv" , append = FALSE)

############################################################
### Visual look at whether these groupings are different ###
############################################################

## These need to be densities for now as total area wide proportions would need to be recalculated. 

# Bar charts of relative quintiles
Townsend_prepared %>% mutate(year = as.factor(year)) %>% filter(!is.na(Label)) %>% group_by(year, Label, Overall_Quintile) %>% summarise(Count = n()) %>%
  ggplot(aes(x = Overall_Quintile, y = Count, fill = Label)) + geom_col() + facet_grid(rows = vars(year))

library(hrbrthemes)
# Densities 
Townsend_prepared %>% mutate(year = as.factor(year)) %>% filter(!is.na(Label)) %>% 
  ggplot(aes(x = prop_unemployed, fill = Label)) + geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') + facet_wrap(~year, nrow = 2) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) + theme_ipsum() + labs(fill="")

##### TO DO - Recalculate Totals here ????

### Mean proportions is a big horrid but lets take a look
Townsend_prepared %>% mutate(year = as.factor(year)) %>% filter(!is.na(Label)) %>% group_by(year, Label) %>% 
  summarise(across(starts_with("prop_"), ~mean(.x))) %>% pivot_longer(starts_with("prop_"), names_to = "Component", values_to = "mean_proportion") %>%
  mutate(Component = str_remove(Component, "prop_")) %>%
  ggplot(aes(x = year, y = mean_proportion, group = Component, color = Component)) + geom_line() + facet_grid(cols = vars(Label)) + theme_ipsum() 


### Mean Townsend Index is a big horrid but lets take a look
Townsend_prepared %>% mutate(year = as.factor(year)) %>% filter(!is.na(Label)) %>% group_by(year, Label) %>% 
  summarise(mean_Townsend = mean(Townsend_index)) %>%
  ggplot(aes(x = year, y = mean_Townsend, group = Label, color = Label)) + geom_line() 

### Spider/Radar Plot??? 

#################################################################################
### Biggest movers & shakers (areas which have gone up/down the ratings most) ###
#################################################################################

Wide_data <- Townsend_prepared %>% select(year, lsoa21, Census_Quintile, Label) %>% 
  mutate(year = str_c("year_", year)) %>%
  pivot_wider(names_from = "year", values_from = "Census_Quintile") %>%
  mutate(Change = year_2021 - year_1971) %>%
  mutate(across(starts_with("year_"), as.factor))

str(Wide_data)
# Bar charts of distribution of change in quintiles
Wide_data %>% filter(!is.na(Label)) %>% group_by(Label, Change) %>% summarise(Count = n()) %>%
  ggplot(aes(x = Change, y = Count, fill = Label)) + geom_col(position = "dodge") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) + theme_ipsum() + labs(fill="")

geographical_data <- Birmingham_2021 %>% right_join(Wide_data, by = c("LSOA21CD" = "lsoa21"))

Changes_map <- tm_shape(geographical_data) + tm_fill(col = "Change", style = "cat", palette = "brewer.pu_or") + tm_borders(alpha = 0.4)

tmap_save(Changes_map, 
          filename = "~/R/Birmingham&Walsall/Poster_materials/Changes_map.png",
          width = 3000,
          height = 3000,
          dpi = 300)

#########################################################################
#########################################################################




