library(ggplot2)
library(tidyverse)
library(readr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3") 

weights_1981 <- read_csv("weights_1981.csv") 
#weights_1971 <- read_csv("weights_1971.csv")

#List of 1981 EDs in Birmingham
ED_1981 <- weights_1981 %>% select(ED81CDO) %>% distinct()

setwd("~/R/Birmingham&Walsall/Week3/1981") 
dir() #check files in working directory

#Vectors of the table/files we are interested in 
tables_1981 <- paste0("81sas", c("09", "10", "20", "46", "48", "45", "50"))
files_1981 <- paste0(tables_1981, "ews_4.csv")

#We need the following table for the descriptions of what is in each column
Variable_lookup <- read_csv("1981_variable_lookup.csv") %>% 
  mutate(table_code = str_extract(table_column_name, pattern = "^81sas\\d{2}")) %>% 
  filter(table_code %in% tables_1981) %>%
  mutate(variable_code = str_remove(table_column_name, pattern = "^81sas\\d{2}"))

Variable_lookup <- Variable_lookup %>%  bind_cols(str_split(Variable_lookup$description, pattern = "\\s//\\s", simplify = TRUE))

# Inspected data to extract codes for relevant tables 
Test <- Variable_lookup %>% filter(table_code == tables_1981[7]) %>% filter(`...6` == "Manufacturing"|`...6` == "Total in employment")

#################################################
### Table 45 - ## Compare 10% to 100% samples ###
#################################################

Variable <- c("total_residents", "residents_10") # Includes children!!!

code <- c("4442", "4448")
name_col <- paste0(tables_1981[6], code)

code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[6]) 

####Select only the Birmingham EDs
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  rename(!!!code_to_variable) 

Townsend_1981 <- data_1981 %>% select(-residents_10)
Manufacturing_1981 <- data_1981

###################################################
### Table 20 - ## Economically Active/Unemployed ##
###################################################

# Note using Table 20 not 9 as this seems to have the required figures in 

Variable <- c("total_working_age", "total_Active", "total_Inactive", "unemployed", "InActive_sick")

code <- c("1629", "1634", "1659", "1649", "1664")
name_col <- paste0(tables_1981[3], code)

code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[3]) 

####Select only the Birmingham EDs
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  rename(!!!code_to_variable)

Townsend_1981 <- Townsend_1981 %>% left_join(data_1981 %>% select(ED81CDO, total_Active, unemployed))
Manufacturing_1981 <- Manufacturing_1981 %>% left_join(data_1981 %>% select(ED81CDO, InActive_sick))

########################################################
### Table 10
### The next three factor are all from Table 10 which makes handling them easier
### Overcrowded, Owned & No car & Exclusive Bathroom 
########################################################

Variable <- c("total_households", "Overcrowded_high", "Overcrowded_med", "Owned", "no_car", "Bathroom")

code <- c("0929", "0945", "0946", "0967", "0949", "0930")
name_col <- paste0(tables_1981[2], code)

code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[2]) 

####Select only the Birmingham EDs
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  rename(!!!code_to_variable) %>% mutate(Overcrowded = Overcrowded_high + Overcrowded_med)

Townsend_1981 <- Townsend_1981 %>% left_join(data_1981 %>% select(ED81CDO, total_households, Overcrowded, Owned, no_car))

############################################
### Table 46 - ## Industry of Employment ###
############################################

Test <- Variable_lookup %>% filter(table_code == tables_1981[4]) 

### total people employed reported in the 10% sample

total_employee_code <- Test %>% filter(`...6` == "Total in employment") %>% arrange(variable_code) %>% pull(variable_code)

Variable <- Test %>% filter(`...6` == "Total in employment") %>% mutate(variable_name = str_c(`...5`, `...7`, sep = " ")) %>% 
  arrange(variable_code) %>% pull(variable_name)

name_col <- paste0(tables_1981[4], total_employee_code)
code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[4]) 

####Select only the Birmingham EDs ##### 

#I have left the age columns in at the moment to look at the distribution across age ranges
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
 mutate(Total_employed = rowSums(across(all_of(name_col)))) %>% rename(!!!code_to_variable) 

Manufacturing_1981 <- Manufacturing_1981 %>% left_join(data_1981 %>% select(ED81CDO, Total_employed))

###################################################################

### total people employed in Manufacturing 
manufacturing_code <- Test %>% filter(`...6` == "Manufacturing") %>% arrange(variable_code) %>% pull(variable_code)

Variable <- Test %>% filter(`...6` == "Manufacturing") %>% mutate(variable_name = str_c(`...5`, `...7`, sep = " ")) %>% 
  arrange(variable_code) %>% pull(variable_name)

name_col <- paste0(tables_1981[4], manufacturing_code)
code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[4]) 

####Select only the Birmingham EDs ##### 

#I have left the age columns in at the moment to look at the distribution across age ranges
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  mutate(Manufacturing = rowSums(across(all_of(name_col)))) %>% rename(!!!code_to_variable) 

Manufacturing_1981 <- Manufacturing_1981 %>% left_join(data_1981 %>% select(ED81CDO, Manufacturing))

#################################################
### Table 50 - ## SEG from 10% sample ###
#################################################

Variable <- c("SEG8m", "SEG8f1", "SEG8f2", 
              "SEG9m", "SEG9f1", "SEG9f2",
              "SEG10m", "SEG10f1", "SEG10f2", 
              "SEG11m", "SEG11f1", "SEG11f2") 

code <- as.character(seq(5266, 5277, 1))
name_col <- paste0(tables_1981[7], code)

code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[7]) 

####Select only the Birmingham EDs
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  rename(!!!code_to_variable) %>% mutate(Foreman = SEG8m + SEG8f1 + SEG8f2, Skilled =  SEG9m + SEG9f1 + SEG9f2, 
                                         Semi_skilled = SEG10m + SEG10f1 + SEG10f2, Unskilled =SEG11m + SEG11f1 + SEG11f2 ) %>%
  select(ED81CDO, Foreman:Unskilled)

Manufacturing_1981 <- Manufacturing_1981 %>% left_join(data_1981)

############################################
### Table 47 - ## Education Levels ###
############################################

Variable <- c("Age_18", "Age_30", "Age_45", "Age_65")

code <- c("4805", "4808", "4811", "4814")
name_col <- paste0(tables_1981[5], code)

code_to_variable <- setNames(name_col, Variable)

####This is the ED data for the whole country relevant table
data_1981 <- read_csv(files_1981[5]) 

####Select only the Birmingham EDs
data_1981 <- left_join(ED_1981, data_1981, by = c("ED81CDO" = "zoneid")) %>% select(ED81CDO, all_of(name_col)) %>%
  rename(!!!code_to_variable) %>% mutate(Well_Edu = rowSums(across(Age_18:Age_65)))

Manufacturing_1981 <- Manufacturing_1981 %>% left_join(data_1981 %>% select(ED81CDO, Well_Edu)) %>% 
  relocate(Manufacturing, .after = Unskilled) %>%
  relocate(Total_employed, .after = Unskilled) %>%
  mutate(Disabled = NA, total_persons = NA, poor_health = NA)

# Check col names
vars == colnames(Manufacturing_1981)

###########################################################
### Check distribution of population ### Added 13th May ###
###########################################################

Townsend_1981 %>% ggplot(aes(x = total_residents)) +geom_histogram()

summary(Townsend_1981$total_residents)

Count_zeros <- Townsend_1981 %>% summarise(zero_residents = sum(total_residents == 0, na.rm = TRUE), 
                                           zero_active = sum(total_Active == 0, na.rm = TRUE), 
                                           zero_households = sum(total_households == 0, na.rm = TRUE))

# I have checked that we do not need an or statement here! This is a list of all of the EDs with missing data
#Townsend_1981 <- Townsend_1981 %>% mutate(flag_missing = total_households == 0) 

#Number of EDs with problematic 10% data
sum(Manufacturing_1981$residents_10 == 0, na.rm = TRUE)

# Flag these in data
#Manufacturing_1981 <- Manufacturing_1981 %>% mutate(flag_missing = residents_10 == 0)

#### Geography of missingness

library(sf)
library(corrplot)
library(tmap)
library(spdep)
library(spgwr)
library(RColorBrewer)

# This file contains the boundary information
setwd("~/R/Birmingham&Walsall/Week3") 
Boundaries_1981 <- read_sf("ED_1981_EW.shp")

Check <-Boundaries_1981 %>% right_join(Townsend_1981) 
Check <-Boundaries_1981 %>% right_join(Manufacturing_1981) 
#tm_shape(Check) + tm_fill("flag_missing", fill.scale = tm_scale_categorical("brewer.purples")) + tm_borders(fill_alpha=0.4)

##### Write files
setwd("~/R/Birmingham&Walsall/Week4") 

write_csv(Townsend_1981, "Townsend_1981.csv")
write_csv(Manufacturing_1981, "Manufacturing_1981.csv")

###########################################################################################################
###########################################################################################################
###### 1971 DATA 
###########################################################################################################
###########################################################################################################
setwd("~/R/Birmingham&Walsall/Week4") 

temp <- read_csv("Manufacturing_2001.csv")

setwd("~/R/Birmingham&Walsall/Week3") 

weights_1971 <- read_csv("weights_1971.csv")

#List of 1981 EDs in Birmingham
ED_1971 <- weights_1971 %>% select(ED71ZCD) %>% distinct()

setwd("~/R/Birmingham&Walsall/Week3/1971") 
dir()

#Data by PERSONS for all Birmingham EDs (all variables)
data_1971 <- read_csv("c71_c_p.csv") %>% right_join(ED_1971, by = c("zone_code" = "ED71ZCD"))

# Total residents - table 1 field 037
vars <- data.frame(table = 1, field = "037", label = "total_residents")

# Breakdown by age - table 7
# Total Children table 7 - 269 & 297
# Over retirement age - male 280, 281, 282, 294, 295, 296; female 307 to 310 and 321 to 324
# Total by gender to check - male 325, 326 , female 327, 328
# Working - 329, 334, 339, 344
# Seeking work - 330, 335, 340, 345
# Sick - 331, 336, 341, 346

field <- c(269, 297, 280, 281, 282, 294, 295, 296, 307, 308, 309, 310, 321, 322, 323, 324, 325, 326, 327, 328, 329, 334, 339, 344, 
           330, 335, 340, 345, 331, 336, 341, 346)
label <- c("male_child", "female_child", "male_r1", "male_r2", "male_r3", "male_r4", "male_r5", "male_r6", 
           "female_r1", "female_r2", "female_r3", "female_r4", "female_r5", "female_r6", "female_r7", "female_r8", 
           "total_m1", "total_m2", "total_f1", "total_f2", "working_m1", "working_m2", "working_f1", "working_f2", 
           "seeking_m1", "seeking_m2", "seeking_f1", "seeking_f2", "sick_m1", "sick_m2", "sick_f1", "sick_f2")

vars <- bind_rows(vars, data.frame(table = 7, field = as.character(field), label))

#Economically Active breakdown - table 5
# Totals (all ages) - 257, 258, 259
# Seeking work - 261, 264, 267
# Sick - 262, 265, 268

field <- c(257, 258, 259, 261, 264, 267, 262, 265, 268)
label <- c("total_EA_m", "total_EA_f1", "total_EA_f2", "seeking_EA_m1", "seeking_EA_f1", "seeking_EA_f2", "sick_EA_m1", "sick_EA_f1", "sick_EA_f2")

vars <- bind_rows(vars, data.frame(table = 5, field = as.character(field), label))

##### 
vars <- vars %>% mutate(name_col = paste0("c71s0", table, "_", field)) 
name_col <- vars %>% pull(name_col)
variable <- vars %>% pull(label)
code_to_variable <- setNames(name_col, variable)

####Select only the variables of interest ##### 

data_1971 <- data_1971 %>% 
  select(zone_code, all_of(name_col)) %>% rename(!!!code_to_variable) 

data_1971 <- data_1971 %>% #Create sums as not interested in gender/martial status breakdown
  mutate(children = male_child + female_child, retired = rowSums(across(matches("r\\d$"))), 
         total_working_age = total_residents - children - retired, working = rowSums(across(matches("^working_[mf]"))), 
         seeking = rowSums(across(matches("^seeking_[mf]"))), sick = rowSums(across(matches("^sick_[mf]"))), 
         total_EA = total_EA_m + total_EA_f1 + total_EA_f2, seeking_EA = seeking_EA_m1 + seeking_EA_f1 + seeking_EA_f2, 
         sick_EA = sick_EA_m1 + sick_EA_f1 + sick_EA_f2) %>%
  select(zone_code, total_residents, children:sick_EA)

# Adjust total residents, seeking and sick
data_1971 <- data_1971 %>% mutate(total_residents = ifelse(total_residents < total_EA, total_EA, total_residents), 
                seeking = ifelse(seeking > seeking_EA, seeking, seeking_EA), 
                InActive_sick = ifelse(sick > sick_EA, sick, sick_EA)) 

Townsend_1971 <- data_1971 %>% select(zone_code, total_residents, total_Active = total_EA, unemployed = seeking)
Manufacturing_1971 <- data_1971 %>% select(zone_code, total_residents, InActive_sick)

# Households and amenities - table 18

ED_1971 <- weights_1971 %>% select(ED71ZCD) %>% distinct()

setwd("~/R/Birmingham&Walsall/Week3/1971") 
dir()

#Data by PERSONS for all Birmingham EDs (all variables)
data_1971 <- read_csv("c71_c_h.csv") %>% right_join(ED_1971, by = c("zone_code" = "ED71ZCD"))

#Table 16 - This data seems inconsistent with table 18 - so I'm not using it anymore!

##Table 18
# Tenure - Owner Occupied - 53
# Tenure - Council 54
# Tenure - Private rented 55, 56

# Overcrowded -65 and 66
# No car - 77

field <- c(53, 54, 55, 56, 65, 66, 77)
label <- c("Owned", "Council", "Rent_unfurn", "Rent_furn", "Overcrowded_high", "Overcrowded_med", "no_car")

vars <- data.frame(table = 18, field = str_pad( as.character(field), 3, side = "left", pad = "0"), label)

# Create named vector in order to extract correct columns and rename
vars <- vars %>% mutate(name_col = paste0("c71s", table, "_", field)) 
name_col <- vars %>% pull(name_col)
variable <- vars %>% pull(label)
code_to_variable <- setNames(name_col, variable)

####Select only the variables of interest ##### 

data_1971 <- data_1971 %>% 
  select(zone_code, all_of(name_col)) %>% rename(!!!code_to_variable) 

data_1971 <- data_1971 %>% 
  mutate(total_households = Owned + Council + Rent_unfurn + Rent_furn, Overcrowded = Overcrowded_high + Overcrowded_med) %>%
  select(-c("Council", "Rent_unfurn", "Rent_furn", "Overcrowded_high", "Overcrowded_med")) %>%
  select(zone_code, total_households, Overcrowded, Owned, no_car)

Townsend_1971 <- Townsend_1971 %>% left_join(data_1971)

# Small sample - Industry, SEG and education levels

setwd("~/R/Birmingham&Walsall/Week3/1971") 
dir()

#Data by PERSONS/Households for all Birmingham EDs (all variables)
data_1971 <- read_csv("c71_x_ph.csv") %>% right_join(ED_1971, by = c("zone_code" = "ED71ZCD"))

#Table 23 

# Total in 10% sample (listing a type of work in which they are employed) - 359, 360
## Manual SEGs

# 8 Supervisors & foremen - 210, 227
# 9 Skilled - 211, 228
# 10 Semi-Skilled - 212, 229
# 11 Unskilled - 213, 230

# Further Ed - 363
# Higher Ed inc A'level - 362

field <- c(359, 360, 210, 227, 211, 228, 212, 229, 213, 230, 363, 362, 399, 398, 395, 396)
label <- c("total_m_10", "total_f_10", "SEG8M", "SEG8F", "SEG9M", "SEG9F", "SEG10M", "SEG10F", 
          "SEG11F", "SEG11M", "further_ed_EA", "higher_ed_EA","further_ed_EI", "higher_ed_EI", "totalm10", "totalf10")

vars <- data.frame(table = 23, field = as.character(field), label)

# Table 28 - Industry

# Manufacturing column - 193, 200, 207, 214, 221, 228, 235, 242, 249, 256
# Group SEG1, SEG3, SEG5, SEG6, SEG8_9, SEG7_10, SEG11 SEG12, SEG16, Inside_LAA

field <- c( 193, 200, 207, 214, 221, 228, 235, 242, 249)
label <- c("ind1", "ind3", "ind5", "ind6", "ind8_9", "ind7_10", "ind11", "ind12", "ind16")

vars <- bind_rows(vars, data.frame(table = 28, field = as.character(field), label))

# Create named vector in order to extract correct columns and rename
vars <- vars %>% mutate(name_col = paste0("c71s", table, "_", field)) 
name_col <- vars %>% pull(name_col)
variable <- vars %>% pull(label)
code_to_variable <- setNames(name_col, variable)

####Select only the variables of interest ##### 

data_1971 <- data_1971 %>% 
  select(zone_code, all_of(name_col)) %>% rename(!!!code_to_variable) 

data_1971 <- data_1971 %>% 
  mutate(Total_employed= total_m_10 + total_f_10, Foreman = SEG8M + SEG8F, Skilled = SEG9M + SEG9F, 
         Semi_skilled = SEG10M + SEG10F, 
         Unskilled = SEG11M + SEG11F, 
         Manufacturing = rowSums(across(matches("ind\\d+"))), 
         Well_Edu = further_ed_EA + higher_ed_EA + further_ed_EI + higher_ed_EI,
         residents_10 = totalm10 + totalf10) %>%
  select(zone_code, Total_employed:residents_10 )

Manufacturing_1971 <-Manufacturing_1971 %>% left_join(data_1971) %>% relocate(residents_10, .before = InActive_sick) %>% 
  relocate(Total_employed, .after = InActive_sick) %>% relocate(Total_employed, .before = Well_Edu) %>% 
  relocate(Manufacturing, .before = Well_Edu) %>% mutate(Disabled = NA, total_persons = NA, poor_health = NA)

### Check distribution of population 
Townsend_1971 %>% ggplot(aes(x = total_residents)) +geom_histogram()

summary(Townsend_1971$total_residents)

Count_zeros <- Townsend_1971 %>% summarise(zero_residents = sum(total_residents == 0, na.rm = TRUE), 
                                           zero_active = sum(total_Active == 0, na.rm = TRUE), 
                                           zero_households = sum(total_households == 0, na.rm = TRUE))

# I have checked that we do not need an or statement here! This is a list of all of the EDs with missing data
#Townsend_1971 <- Townsend_1971 %>% mutate(flag_missing = total_households == 0) 

#Number of EDs with problematic 10% data
sum(Manufacturing_1971$residents_10 == 0, na.rm = TRUE)

#Check variable names for consistency
colnames(Manufacturing_1971) == colnames(Manufacturing_1981)

# Flag these in data
#Manufacturing_1971 <- Manufacturing_1971 %>% mutate(flag_missing = residents_10 == 0)

#### Geography of missingness

# This file contains the boundary information
setwd("~/R/Birmingham&Walsall/Week3") 
Boundaries_1971 <- read_sf("ED_1971_EW.shp")

Check <-Boundaries_1971 %>% right_join(Townsend_1971, by = c("ED71ZCD" = "zone_code")) 
Check <- Boundaries_1971 %>% right_join(Manufacturing_1971, by = c("ED71ZCD" = "zone_code")) 
#tm_shape(Check) + tm_fill("flag_missing", fill.scale = tm_scale_categorical("brewer.purples")) + tm_borders(fill_alpha=0.4)

######### Write csv file ########

setwd("~/R/Birmingham&Walsall/Week4") 

write_csv(Townsend_1971, "Townsend_1971.csv")
write_csv(Manufacturing_1971, "Manufacturing_1971.csv")

