############################################################
### LOAD FILES
############################################################

library(ggplot2)
library(tidyverse)
library(readr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week4") 
dir() #check files in working directory

weights_1981 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_1981.csv") %>% rename(prop = prop_ED)
weights_1971 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_1971.csv") %>% rename(prop = prop_ED)
weights_1991 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_1991.csv") %>% rename(prop = prop_ED)
weights_2001 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_2001.csv") %>% rename(prop = prop_oa01)
weights_2011 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_2011.csv") %>% rename(prop = prop_oa11)
weights_2021 <- read_csv("~/R/Birmingham&Walsall/Week3/weights_2021.csv")

#We need to include the 2021 weights to turn OAs back into LSOAs

Type <- c("Townsend", "Manufacturing")
year <- c("2021", "2011", "2001", "1991", "1981", "1971")
weight_code <- c("oa21", "oa11", "oa01", "ED91CD", "ED81CDO", "ED71ZCD")
data_code <- c("OA21CD", "OA11CD", "mnemonic", "ED91CD", "ED81CDO", "zone_code")

codes_df <- data.frame(year, weight_code, data_code)

files <- expand.grid(Type, year) %>% mutate(df_name = str_c(Var1, "_", Var2), file_name = str_c(df_name, ".csv")) %>% 
  rename(index = Var1, year = Var2) %>% left_join(codes_df) 

#This puts all of the data in one tibble 
all_data <- files %>%
  mutate(data = map(file_name, read_csv))

##### Pull list of zero EDs here ###### 

get_zero_areas <- function(index, data_code, data){
  if(index == "Townsend"){
    zero_area <- data %>% filter(total_residents == 0|total_households == 0) %>% pull(data_code)
  } else {
    zero_area <- data %>% filter(total_residents == 0|residents_10 == 0) %>% pull(data_code)
  }
return(zero_area)
}

original_zeros <- all_data %>% mutate(zero_area = pmap(list(index = index, data_code = data_code, data = data), get_zero_areas)) %>% 
  select(index, weight_code, data_code, zero_area) %>% unnest(cols = c(zero_area))

write_csv(original_zeros, "unaggregated_zeros.csv")

############################################################################################################################
#Function to Join data to weights and return re-aggregated data 
############################################################################################################################

aggregate_data <- function(year, zone_code1, zone_code2, data){
  df_name <- str_c("weights_", year)
  weights <- get(df_name, envir = .GlobalEnv)
  join_condition <- setNames(zone_code2, zone_code1)
  agg_data <- weights %>% left_join(data, by =join_condition) 
  agg_data <- agg_data %>% mutate(across((ncol(weights)):ncol(agg_data), ~ .x * prop)) %>%  #Apply weights across data fields
  group_by(lsoa21) %>% #Need to group by 2021 LSOA
  summarize(across((ncol(weights)):(ncol(agg_data)-1),  \(x) sum(x, na.rm = TRUE))) %>% #This reaggregates over the 2021 lSOA's
  return(agg_data)
}

# For testing 
# test <- read_csv("~/R/Birmingham&Walsall/Week4/Townsend_2001.csv")

# temp <- aggregate_data("2001", "oa01", "mnemonic", test)

############################################################################################################################

# Alter tibble to contain re-aggregated data

all_data <- all_data %>%
  mutate(aggregated_data = pmap(list(year = year,
      zone_code1 = weight_code,
      zone_code2 = data_code,
      data = data),
    aggregate_data
  )) %>% select(-c(df_name:data))

###############################################################

### Replace zeros in Townsend data 

data_to_replace <- all_data %>% filter(index == "Townsend") %>% unnest(cols = c(aggregated_data)) %>% 
  filter(total_households == 0) %>% filter(lsoa21 != "E01034950" & lsoa21 != "E01009438") #Manual adjustment 
# this central Birmingham LSOA is a correct zero as it was non-residential, this area of Sutton Coldfield was developed in the 90's
# the Rednal area might also be build between 1971 - 1981 OR the postcodes were previously in a different county but this is not clear so I'm going to backfill it!

replacement_data  <- all_data %>% filter(index == "Townsend") %>% unnest(cols = c(aggregated_data)) %>% 
  right_join(data_to_replace %>% select(year, lsoa21) %>% mutate(year = as.character(as.numeric(year) + 10))) %>% 
  mutate(year =  as.character(as.numeric(year) - 10)) 

all_Townsend_data <- all_data %>% filter(index == "Townsend") %>% unnest(cols = c(aggregated_data)) %>%
  anti_join(data_to_replace) %>% bind_rows(replacement_data %>% mutate(updated = TRUE)) %>%
  nest(aggregated_data = lsoa21:updated)

### Replace zeros in Manufacturing Data

data_to_replace <- all_data %>% filter(index == "Manufacturing") %>% unnest(cols = c(aggregated_data)) %>% 
  filter(total_residents == 0) %>% filter(lsoa21 != "E01034950" & lsoa21 != "E01009438") #Manual adjustment 
# this central Birmingham LSOA is a correct zero as it was non-residential, this area of Sutton Coldfield was developed in the 90's
# the Rednal area might also be build between 1971 - 1981 OR the postcodes were previously in a different county but this is not clear so I'm going to backfill it!

replacement_data  <- all_data %>% filter(index == "Manufacturing") %>% unnest(cols = c(aggregated_data)) %>% 
  right_join(data_to_replace %>% select(year, lsoa21) %>% mutate(year = as.character(as.numeric(year) + 10))) %>% 
  mutate(year =  as.character(as.numeric(year) - 10)) 

all_Manufacturing_data <- all_data %>% filter(index == "Manufacturing") %>% unnest(cols = c(aggregated_data)) %>%
  anti_join(data_to_replace) %>% bind_rows(replacement_data %>% mutate(updated = TRUE)) %>%
  nest(aggregated_data = lsoa21:updated)

###############################################################




######## Unnested data for testing purposes ###################

Townsend_aggregated <- all_Townsend_data %>% unnest(cols = c(aggregated_data))
Manufacturing_aggregated <- all_Manufacturing_data %>% unnest(cols = c(aggregated_data))

write_csv(Townsend_aggregated, "Townsend_aggregated.csv")
write_csv(Manufacturing_aggregated, "Manufacturing_aggregated.csv")
