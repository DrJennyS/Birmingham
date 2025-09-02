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
library(ggthemes)

library("factoextra")
library(cluster)

setwd("~/R/Birmingham&Walsall/Week5") 
dir()

Townsend_prepared <- read_csv("Townsend_prepared.csv") %>% filter(!if_any(everything(), ~is.na(.x)))
Manufacturing_prepared <- read_csv("Manufacturing_prepared.csv") %>% filter(!if_any(everything(), ~is.na(.x)))

#tmap_options(outer.bg = TRUE, outer.bg.color = "#D1D9E3")

### Create Wide dataset ###

str(Manufacturing_prepared)

#Function to change names and column bind


Add_year <- function(df, y){
  data_year <- Manufacturing_prepared %>% select(year:prop_sick) %>% filter(year == y) %>% 
    select(-year) %>%
    rename_with(~ paste0(y, "_", .x), prop_manufacturing:prop_sick)
  df %>% left_join(data_year)
}

Add_year2 <- function(df, y){
  data_year <- Townsend_prepared %>% select(year:prop_no_car) %>% filter(year == y) %>% 
    select(-year) %>%
    rename_with(~ paste0(y, "_", .x), prop_unemployed:prop_no_car)
  df %>% left_join(data_year)
}

# list of LSOA21s as a df and add relevant sets of data

df1 <-  Manufacturing_prepared %>% select(lsoa21) %>% distinct() %>% Add_year2(2021)
df2 <-  Manufacturing_prepared %>% select(lsoa21) %>% distinct() %>% Add_year2(2021) %>% Add_year2(2011) %>% Add_year2(2001)
df3 <-  Manufacturing_prepared %>% select(lsoa21) %>% distinct() %>% Add_year2(2021) %>% Add_year(2021)
df4 <-  Manufacturing_prepared %>% select(lsoa21) %>% distinct() %>% Add_year2(2021) %>% Add_year(2021) %>% Add_year2(2011) %>% Add_year(2011)
df5 <-  Manufacturing_prepared %>% select(lsoa21) %>% distinct() %>% Add_year2(2021) %>% Add_year2(2011) 

#df <- df %>% Add_year(2021) #%>% Add_year(2011) %>% Add_year(2001) %>% Add_year(1991) %>% Add_year(1981) #%>% Add_year(1971)
#df <- df %>% Add_year2(2021) #%>% Add_year2(2011) #%>% Add_year2(2001) #%>% Add_year2(1991) %>% Add_year2(1981) %>% Add_year2(1971)

df <- df3
#str(df)

#############################################
#### Load labels from other file 
#############################################

data <- read_csv("~/R/Birmingham&Walsall/Week5/labels.csv")

## create labelled wide dataframe 

df_labels <- df %>% inner_join(data %>% select(lsoa21, k_3)) %>% filter(!if_any(everything(), ~is.na(.x))) %>% rename(group = k_3) 
str(df_labels)

## Strip off identifiers ready for kmeans, nest data and expand out to have a row for each number of clusters within each group

df_labels_nested <- df_labels %>% select(-lsoa21) %>% nest(data = -group)

######## Interlude gap statistic ##########

#temp <- df_labels_nested %>% filter(group == 1) %>% unnest(c(data))
#fviz_nbclust(temp, kmeans, nstart = 25,  method = "gap_stat", nboot = 100, k.max = 10)

###########################################

df_labels_nested <- df_labels_nested %>% left_join(expand.grid(1:3, 1:10) %>% rename(group = Var1, k = Var2))

## Apply kmeans algorithm and store useful representations 

df_labels_nested <- df_labels_nested %>% mutate(kclust = map2(.x = data, .y = k, ~kmeans(.x, .y, nstart = 25)), tidied = map(kclust, tidy), glanced = map(kclust, glance), augmented = map2(kclust, data, augment))

####  Details 
# This contains a the representative point in each of the clusters for each value of k. There are n rows for n =3, 
clusters <- df_labels_nested %>% unnest(cols = c(tidied))

# This contains all of the points for each k, labeled with the cluster they belong to
assignments <- df_labels_nested %>% unnest(cols = c(augmented)) 

# This contains summary stats for each... 
clusterings <- df_labels_nested %>% unnest(cols = c(glanced)) %>% mutate(group = as.factor(group))

### Elbow Plot (grouped)

clusterings %>% filter(k <= 10) %>% ggplot(aes(k, tot.withinss, group = group, color = group)) + geom_line(linewidth = 1) + geom_point(size = 2 ) + 
  theme_economist_white() + scale_color_economist() + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(title = "Elbow plot: determining optimal k", 
       subtitle = "Total within-cluster sum of squares vs number of clusters", 
       color = "1971 manufacturing group",
       caption = "While interpreting Elbow plots can be ambiguous this plot suggests that for \n groups 1 and 3, k = 3 is optimal while for group 2, k = 2 is optimal. ", 
       x = "", y = "") + 
  theme(plot.title = element_text(color = "#174273", size = 21, hjust = 0.5), legend.position = "bottom", 
        , legend.text = element_text(color = "#39464D", size = 12), legend.background = element_rect(fill = "#D1D9E3"),
        plot.subtitle = element_text(color = "#39464D", margin = margin(t = 8, b = 12)), plot.background = element_rect(fill = "#D1D9E3"))


## Function to calculate silhouette scores
calc_sil_scores <- function(aug_data, k){
  if(k >= 2){
    # Split augmented data into original data and a vector of cluster assignments
    cluster_assignments <- aug_data$.cluster
    data_matrix <- aug_data %>% select(-.cluster) %>% as.matrix()
    # Calculate silhouette scores
    sil_scores <- silhouette(as.numeric(cluster_assignments), dist(data_matrix))
    # Return as tibble
    as_tibble(sil_scores)
  } else {
    NULL
  }
}

## Add Silhouette scores to df
df_labels_nested <- df_labels_nested %>% mutate(silhouette_scores = map2(.x = augmented, .y = k, ~calc_sil_scores(.x,.y)), 
avg_silhouette_width = map2(silhouette_scores, k, ~ifelse(.y>=2, mean(.x$sil_width), NA)))

# Sanity check 
#df_labels_nested 

# Summary of average silhouette widths by group and k
silhouette_summary <- df_labels_nested %>%
  filter(k >= 2) %>%
  select(group, k, avg_silhouette_width) %>% mutate(avg_silhouette_width = as.numeric(avg_silhouette_width)) %>% 
  arrange(group, k)

#str(silhouette_summary)

silhouette_summary %>% filter(k <= 10) %>% ggplot(aes(x = k, y = avg_silhouette_width, color = factor(group))) +
  geom_line(linewidth = 1) + geom_point(size = 2) + 
  theme_economist_white() + scale_color_economist() + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(title = "Silhouette Plot: determining optimal k", 
       subtitle = "Silhouette score vs number of clusters", 
       color = "1971 manufacturing group",
       caption = "The optimal choice of k occurs when the silhouette score is maximised. \n For each group this occured when k = 2.", 
       x = "", y = "") + 
  theme(plot.title = element_text(color = "#174273", size = 21, hjust = 0.5), legend.position = "bottom", 
        , legend.text = element_text(color = "#39464D", size = 12), legend.background = element_rect(fill = "#D1D9E3"),
        plot.subtitle = element_text(color = "#39464D", margin = margin(t = 8, b = 12)), plot.background = element_rect(fill = "#D1D9E3"))

##### EASY VERSION OF GAP STATISTIC #####

### This is so much quicker it is insane!! but you can see from the above how the original algorithm is structured. 

#gap_stat2 <- clusGap(temp, kmeans, nstart = 25,  B = 100, K.max = 10, d.power = 1, spaceH0 = "scaledPCA")
#optimal_k <- maxSE(gap_stat2$Tab[, "gap"], gap_stat2$Tab[, "SE.sim"], method = "Tibs2001SEmax", SE.factor = 1)

#gap_stat <- clusGap(temp, kmeans, nstart = 25,  B = 100, K.max = 10, d.power = 1, spaceH0 = "original")
#optimal_k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method = "Tibs2001SEmax", SE.factor = 1)

df_labels_nestedv2 <- df_labels_nested %>% mutate(clusters = k) %>% nest(original_clusters = c(k:augmented), silhouette_data = c(clusters, silhouette_scores, avg_silhouette_width)) 

make_gap_df <- function(data, K.max, d.power, spaceH0){
  set.seed(123)
  gap_stat <- clusGap(data, kmeans, nstart = 25,  B = 500, K.max = K.max, d.power = d.power, spaceH0 = spaceH0) # rerun with B = 500 while having breackfast!
  data.frame(k = 1:K.max, gap_stat$Tab)
}

#test <- make_gap_df(temp, K.max = 10, d.power = 1, spaceH0 = "original")

df_labels_nestedv2 <- df_labels_nestedv2 %>% mutate(gap_stat_orig = map(data, ~make_gap_df(.x, K.max = 10, d.power = 1, spaceH0 = "original")), 
                                                    gap_stat_PCA = map(data, ~make_gap_df(.x, K.max = 10, d.power = 1, spaceH0 = "scaledPCA")))

df_labels_nestedv2 %>% unnest(c(gap_stat_orig)) %>% group_by(group) %>% summarize(Tibs_k = maxSE(gap, SE.sim, method = "Tibs2001SEmax", SE.factor = 1)) 
optimal_k <- df_labels_nestedv2 %>% unnest(c(gap_stat_PCA)) %>% group_by(group) %>% summarize(viz_k = maxSE(gap, SE.sim, method = "firstSEmax", SE.factor = 1)) 


gap_data <- df_labels_nestedv2 %>% unnest(c(gap_stat_orig)) %>% group_by(group) %>% select(group, k, gap, SE.sim) %>% mutate(Hypothesis = "Uniform")
gap_data <- gap_data %>% bind_rows(df_labels_nestedv2 %>% unnest(c(gap_stat_PCA)) %>% group_by(group) %>% select(group, k, gap, SE.sim) %>% mutate(Hypothesis = "Scaled PCA"))

gap_data %>% mutate(Hypothesis = as.factor(Hypothesis)) %>% filter(group == 1) %>% 
  ggplot(aes(x= k, y = gap, color = Hypothesis)) +
  geom_crossbar( aes(ymin=gap-SE.sim, ymax=gap+SE.sim), alpha=0.9, linewidth=0.3) + 
  geom_line(linewidth = 0.9) + geom_point() +
  theme_economist_white() + scale_color_economist() + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(title = "Gap statistic vs number of clusters", 
       subtitle = "Determining the optimal number of clusters for 1971 manufacturing group 1", 
       color = "Null hypothesis distribution",
       caption = "Error bars are included as Tibshirani et al (2001) proposed using the \n smallest k such that: gap(k) > gap(k+1) - simulated.se(k+1).", 
       x = "", y = "") + 
  theme(plot.title = element_text(color = "#174273", size = 21, hjust = 0.5), legend.position = "bottom", 
        , legend.text = element_text(color = "#39464D", size = 12), legend.background = element_rect(fill = "#D1D9E3"),
        plot.subtitle = element_text(color = "#39464D", margin = margin(t = 8, b = 12)), plot.background = element_rect(fill = "#D1D9E3"))

###################################
##### Additional Graphs ###########
###################################

### Get correct clusters of data

cluster_choice <- data.frame(group = 1:3, k = c(1, 3, 4))

### Stick LSOAs back on 

graph_data <- cluster_choice %>% left_join(df_labels_nested) %>% select(group, k, augmented) %>%
  unnest(c(augmented)) %>% left_join(df_labels) 

#%>% select(group, k = .cluster, lsoa21) 

# Tablulate - count of areas in each group/cluster

table(graph_data$group, graph_data$k)

### Index information

Index_data <- Townsend_prepared %>% select(year, lsoa21, Overall_Quintile, Census_Quintile) %>% filter(year == 2021 | year == 1971) %>% 
  pivot_wider(names_from = "year", values_from = c("Overall_Quintile", "Census_Quintile")) %>% 
  mutate(Change_relative = Overall_Quintile_2021 - Overall_Quintile_1971, Change_rank = Census_Quintile_2021 - Census_Quintile_1971)
  
#Index_data2 <- Manufacturing_prepared  %>% select(year, lsoa21, Overall_Quintile, Census_Quintile) %>% filter(year == 2021 | year == 1971) %>% 
#  pivot_wider(names_from = "year", values_from = c("Overall_Quintile", "Census_Quintile")) %>% 
#  mutate(Change_relative = Overall_Quintile_2021 - Overall_Quintile_1971, Change_rank = Census_Quintile_2021 - Census_Quintile_1971)

# Link index data and cluster data 

table_data <- graph_data %>% select(group, k = .cluster, lsoa21) %>% left_join(Index_data)

# Count per cluster per group - note k =1 in group 1 does not necessarily have a relation to k = 1 in group 2 or 3 
table(table_data$k, table_data$group)

# Tabulate - count of 2021 Townsend index per group - THIS ONE!!
table(table_data$group, table_data$Census_Quintile_2021)

Table_Group_Totals <- table_data %>% group_by(group, k) %>% summarize(total = n()) 

Table_Group_clusters <- table_data %>% group_by(group, k, Census_Quintile_2021) %>% summarize(count = n()) %>% 
  left_join(Table_Group_Totals) %>% mutate(proportion = count/total) %>% select(-count, -total) %>%
  mutate(Census_Quintile_2021 = str_c("Quintile ", Census_Quintile_2021), proportion = round(proportion, 2)) %>%
  pivot_wider(names_from = Census_Quintile_2021, values_from = "proportion", values_fill = 0 ) 

library(kableExtra)
kab <- kable(Table_Group_clusters, caption = "Proportion of neighbourhoods in each cluster within each 2021 Townsend index quintile.", booktabs=T)
kable_classic_2(kab, full_width=F, latex_options="hold_position") %>% save_kable("~/R/Birmingham&Walsall/Poster_materials/cluster_table.png")

# table(table_data$group, table_data$Overall_Quintile_2021)
# table(table_data$group, table_data$Overall_Quintile_1971)

# Tabulate Changes in Townsend index
table(table_data$group, table_data$Change_rank)

Table_Changes <- table_data %>% group_by(group, k, Change_rank) %>% summarize(count = n()) %>% 
  left_join(Table_Group_Totals) %>% mutate(proportion = count/total) %>% select(-count, -total) %>%
  mutate(proportion = round(proportion, 2)) %>%
  pivot_wider(names_from = Change_rank, values_from = "proportion", values_fill = 0 ) 

kab <- kable(Table_Changes, caption = "Proportion of neighbourhoods in each cluster grouped by change in Townsend quintile between 1971 and 2021.", booktabs=T)
kable_classic_2(kab, full_width=F, latex_options="hold_position") %>% save_kable("~/R/Birmingham&Walsall/Poster_materials/changes_table.png")

### Graph 1 - Panels of mean Townsend trajectories 

Trajectory_data <- Townsend_prepared %>% select(year, lsoa21, Townsend_index) %>%  
  left_join(graph_data %>% select(group, k = .cluster, lsoa21)) %>% filter(!is.na(group)) %>% mutate(group = as.factor(group))

# Mean across each group
Trajectory_data %>% group_by(year, group) %>% summarize(mean = mean(Townsend_index)) %>% 
  ggplot(aes(x = factor(year), y = mean, group =group, color = group)) + geom_line()

## This one!!!
Trajectory_data %>% mutate(group = str_c("Group ", group)) %>% group_by(year, group, k) %>% summarize(mean = mean(Townsend_index)) %>% 
  ggplot(aes(x = factor(year), y = mean, group = k, color = k)) +
  geom_line(linewidth = 0.9) + geom_point() +
  theme_economist_white() + scale_color_economist() + facet_wrap(~group, ncol = 1) + 
  labs(title = "Local Deprivation Trajectories", 
       subtitle = "Mean Townsend Index by 1971 Manufacturing group and clusters within each group", 
       color = "Cluster",
       caption = "The Townsend index used in this figure is calculated over all (area, year) pairs \n 
       High Townsend index indicates higher than average levels of deprivation", 
       x = "", y = "") + 
  theme(plot.title = element_text(color = "#174273", size = 21, hjust = 0.5), legend.position = "bottom", 
        , legend.text = element_text(color = "#39464D", size = 12), legend.background = element_rect(fill = "#D1D9E3"),
        plot.subtitle = element_text(color = "#39464D", margin = margin(t = 8, b = 12)), plot.background = element_rect(fill = "#D1D9E3"))

# I do not have time to break this down by underlying factors

### Graph 2 - Geographical map of clusters

# Boundary information

Birmingham_2021 <- read_sf("~/R/Birmingham&Walsall/Week3/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp") %>% 
  filter(str_detect(LSOA21NM, "Birmingham")) 

geographical_data <- Birmingham_2021 %>% right_join(graph_data %>% select(group, k = .cluster, lsoa21), by = c("LSOA21CD" = "lsoa21")) %>% 
  mutate(Label = as.factor(ifelse(group == 1, "Group 1", str_c("Group ", group, ": k = ", k))))

## manual_palette 
#Manual_palette <- c("Group 1" = "#F8F8F8", "Group 2: k = 1" = "#BCBDDC","Group 2: k = 2" = "#9E9AC8","Group 2: k = 3" = "#807DBA","Group 3: k = 1" = "#74A9CF","Group 3: k = 2" = "#3690C0","Group 3: k = 3" = "#0570B0","Group 3: k = 4" = "#034E7B")
Manual_palette <- c("Group 1" = "#FFFFFF", "Group 2: k = 1" = "#AF8DC3","Group 2: k = 2" = "#7B68AA","Group 2: k = 3" = "#543B82", "Group 3: k = 1" = "#6BAED6",  "Group 3: k = 2" = "#2171B5", "Group 3: k = 3" = "#08519C", "Group 3: k = 4" = "#08306B")
summary(geographical_data$Label)

Grouped_cluster_map <- tm_shape(geographical_data) + tm_fill(col = "Label", style = "cat", palette = Manual_palette, title = "Cluster") + tm_borders(alpha = 0.4) + 
  tm_layout(legend.position = c("left", "top"), legend.text.size = 0.6, legend.title.size = 0.6, legend.text.color = "#39464D") 

tmap_save(Grouped_cluster_map, 
          filename = "~/R/Birmingham&Walsall/Poster_materials/clustered_map.png",
          width = 3000,
          height = 3000,
          dpi = 600)

#######################################################################################################################################################
                                                    ####### Slow - to check each step - Gap Statistic #########

set.seed(123)

## Make Gap results table
make_gap_results <- function(g){
gap_results <- df_labels_nested %>% filter(group == g) %>% 
  unnest(c(glanced)) %>% select(k, wk = tot.withinss)
gap_results
} 

# Min/Max of data for generating bootstrap samples
find_ranges <- function(data){
  temp <- data %>% summarise(across(everything(), list(min = min, max = max))) %>% pivot_longer(everything()) %>% 
    mutate(type = ifelse(str_detect(name, "_min"), "min", "max"), name = str_remove(name, "_m.{2}$")) %>% 
    pivot_wider(names_from = "type", values_from = "value")
  temp
}

generate_data <- function(p, n, ranges){
  bootstrap_data <- matrix(nrow = n, ncol = p)
  for(i in 1:p){
    bootstrap_data[, i] <- runif(n, min = ranges$min[i], max = ranges$max[i])
  }
  colnames(bootstrap_data) <- ranges$name
  as.data.frame(bootstrap_data)
}

# Make bootstrap data
generate_samples <- function(n, ranges){
  p <- nrow(ranges)
  bootstrap <- tibble(iter = 1:n_bootstraps) %>% mutate(data = map(.x = iter, ~generate_data(p, n, ranges)))
  bootstrap
}

log_wss <- function(data, max_k){
  temp <- tibble(k = 1:max_k) %>% mutate(km = map( .x = k, ~kmeans(data, .x, nstart = 25)), glanced = map(km, glance)) %>% unnest(c(glanced)) %>%
    select(k, tot.withinss) %>% mutate(k = str_c("k_", k), log_wss = log(tot.withinss)) %>% select(-tot.withinss) %>% 
    pivot_wider(everything(), names_from = "k", values_from = "log_wss")
  temp
}

log_wss_means <- function(df){
  df %>% select(-samples_iter, -samples_data) %>% summarise(across(everything(), ~mean(.x))) %>% pivot_longer(everything()) %>% 
    mutate(k = as.numeric(str_remove(name, "k_")), mean_log_wss = value) %>% select(-name, -value)
}

log_wss_sd <- function(df){
  df %>% select(-samples_iter, -samples_data) %>% 
    summarise(across(everything(), ~sd(.x))) %>% pivot_longer(everything()) %>% 
    mutate(k = as.numeric(str_remove(name, "k_")), sd_log_wss = value) %>% select(-name, -value)
}

link_dfs <- function(df1, df2){
  df1 %>% left_join(df2) 
}

# Generate bootstrap datasets
n_bootstraps <- 100

#drop unnecessary data
df_labels_nested_short <- df_labels_nested %>% select(group:k, glanced)

## Add started results df
df_nested <- df_labels_nested_short %>% mutate(gap_results = map(.x = group, ~make_gap_results(.x))) %>% select(-k,-glanced) %>%
  # Add ranges to nested df 
  mutate(min_max = map(.x = data, ~find_ranges(.x))) %>% 
  #Add bootstrap samples to df
  mutate(samples = map2(.x =  map_int(data, nrow), .y = min_max, ~generate_samples(.x, .y))) %>% unnest(c(samples), names_sep = "_") %>% 
  # Calculate wss for each sample
  mutate(log_wss = map(samples_data, ~log_wss(.x, 20))) %>%
  #Rearrange for better manipulation 
  unnest(c(log_wss)) %>% nest(sim_wss = samples_iter:k_20) %>% 
  # Calculate means of bootstrap wss 
  mutate(log_wss_means = map(sim_wss, ~log_wss_means(.x))) %>%
  # Calculate sd of bootstrap wss
  mutate(log_wss_sd = map(sim_wss, ~log_wss_sd(.x))) %>% 
  #Extend gap_results tibbles
  mutate(gap_results = map2(gap_results, log_wss_means, ~link_dfs(.x, .y))) %>%
  mutate(gap_results = map2(gap_results, log_wss_sd, ~link_dfs(.x, .y))) %>% 
  # Tidy up a bit
  nest(gap_data = min_max:log_wss_sd) %>% unnest(c(gap_results)) %>%
  # Calculate gap statistics and se
  mutate(log_wss = log(wk), gap = mean_log_wss - log_wss, gap_se = sd_log_wss * sqrt(1 +(1/n_bootstraps)))

# Optimal number of clusters - method B 
optimal_k_max <- df_nested  %>% group_by(group) %>% slice_max(order_by = gap, n=1) %>% select(group, k)

#Optimal number of clusters - method A
df_nested <- df_nested %>% mutate(group = as.factor(group)) %>% group_by(group) %>% mutate(stat = lead(gap) - lead(gap_se), gap_stop = gap >= stat) 

optimal_k_alt <- df_nested %>% filter(gap_stop == TRUE) %>% slice_min(order_by = k, n = 1) %>% select(group, k)

df_nested %>% filter(group == 1) %>% ggplot(aes(x= k, y = gap)) + 
  geom_errorbar( aes(ymin=gap-gap_se, ymax=gap+gap_se), alpha=0.9, linewidth=0.3) + 
  geom_line() + geom_point() 


