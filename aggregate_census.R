
library(ggplot2)
library(tidyverse)
library(readr)

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3") 
dir() #check files in working directory

############################################################
### Load 2021 data by LSOA
############################################################

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3/2021") 
dir()

### Economically active residents
data_2021 <- read_csv("TS063_EconActive.csv", skip = 6, n_max = 3325) %>% 
  rename(OA21CD = `2021 output area`, total_residents  = `Total: All usual residents aged 16 years and over`, 
                     total_Active = `Economically active (excluding full-time students)`, 
                     total_inactive = `Economically inactive`, 
                     unemployed = `Economically active (excluding full-time students): Unemployed`) %>%
  select(OA21CD:total_Active,unemployed, total_inactive:`Economically inactive: Other`) %>%
  rename_with(~ str_remove(., pattern = "^Economically\\sinactive:\\s"), everything()) %>% mutate(residents_10 = NA)

colnames(data_2021)

#Start dataframe of Townsend index components
Townsend <- data_2021 %>% select(OA21CD, mnemonic, total_residents, total_Active, unemployed)

#Start dataframe of other index components 
Manufacturing <- data_2021 %>% select(OA21CD, mnemonic, total_residents, residents_10, InActive_sick = `Long-term sick or disabled`)

### Overcrowding

data_2021 <- read_csv("TS052_Overcrowding.csv", skip = 6, n_max = 3325) %>% 
  rename(total_households = `Total: All households`, OA21CD = `2021 output area`) %>% 
  mutate(Overcrowded = `Occupancy rating of bedrooms: -1`+`Occupancy rating of bedrooms: -2 or less`) 

Townsend <- Townsend %>% left_join(data_2021 %>% select(OA21CD, mnemonic, total_households, Overcrowded))

### Tenure

data_2021 <- read_csv("TS054_Tenure.csv", skip = 6, n_max = 3325) %>% 
  rename(OA21CD = `2021 output area`) 

Townsend <- Townsend %>% left_join(data_2021 %>% select(OA21CD, mnemonic, Owned))

### Cars and vans

data_2021 <- read_csv("TS045_Car.csv", skip = 6, n_max = 3325) %>% 
  rename(OA21CD = `2021 output area`, no_car = `No cars or vans in household`, total_households = `Total: All households`) %>%
  rename_with(~ str_extract(., pattern = "^\\d"), -c(OA21CD, no_car, mnemonic, total_households)) 

Townsend <- Townsend %>% left_join(data_2021 %>% select(OA21CD, no_car))

## Manufacturing professions

## Sector of employment

data_2021 <- read_csv("TS060A _Industry.csv", skip = 5, n_max = 3325)

Industry_lookup21 <- data.frame(text = colnames(data_2021)[4:21]) %>% 
  mutate(code = str_extract(text, pattern = "^[A-Z]"), 
         industry = str_remove(text, pattern = "^[A-Z]\\s")) %>%
  select(-text)

data_2021 <- data_2021 %>% rename(OA21CD = `2021 output area`) %>% 
  rename_with(~ str_extract(., "^[A-Z]"), -c(OA21CD, mnemonic, Total)) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, mnemonic, Total_employed = Total, Manufacturing = C))

## Type of Employment 

data_2021 <- read_csv("TS063_Occupation.csv", skip = 6, n_max = 3325)

Occupation_lookup21 <- data.frame(text = colnames(data_2021)[4:12]) %>% 
  mutate(code = str_extract(text, pattern = "^\\d"), 
         occupation = str_remove(text, pattern = "^\\d\\.\\s")) %>%
  mutate(occupation = str_remove(occupation, pattern = "\\soccupations$")) %>%
  select(-text)

data_2021 <- data_2021 %>% rename(OA21CD = `2021 output area`, 
                                  total = `Total: All usual residents aged 16 years and over in employment the week before the census`) %>% 
  rename_with(~ str_extract(., "^\\d"), -c(OA21CD, mnemonic, total)) %>% mutate(Foreman = NA) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, Foreman, Skilled = `5`, Semi_skilled = `8`, Unskilled = `9`)) %>%
  relocate(Total_employed, .after = Unskilled) %>% relocate(Manufacturing, .after = Total_employed)

## Education

data_2021 <- read_csv("TS067_Education.csv", skip = 6, n_max = 3325) 

data_2021 <- data_2021 %>% rename(OA21CD = `2021 output area`, 
                                  total_working_age = `Total: All usual residents aged 16 years and over`) %>% 
  rename_with(~ str_extract(., pattern = "^(\\w+)(?=\\s+[A-Za-z])|^(\\w+\\s+\\d+)(?=\\s+)"), -c(OA21CD, mnemonic, total_working_age, Apprenticeship)) %>%
  mutate(Well_Edu = total_working_age - No - `Level 1`)

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, Well_Edu))

## Health 

data_2021 <- read_csv("TS037_General health.csv", skip = 6, n_max = 3325) %>% 
  rename(OA21CD = `2021 output area`, total_persons = `Total: All usual residents`) %>%
  mutate(poor_health = `Very bad health` + `Bad health`)

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, total_persons , poor_health ))

data_2021 <- read_csv("TS038_Disability.csv", skip = 6, n_max = 3325) %>% rename(OA21CD = `2021 output area`, 
                                  Disabled = `Disabled under the Equality Act`) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, Disabled)) %>% relocate(Disabled, .after = Well_Edu)

Manufacturing_2021 <- Manufacturing %>% select(-mnemonic)
Townsend_2021 <- Townsend %>% select(-mnemonic)

vars <- colnames(Manufacturing_2021)

### Check distribution of population 
Townsend_2021 %>% ggplot(aes(x = total_residents)) +geom_boxplot()

summary(Townsend_2021$total_residents)

############################################################
### Load 2011 data by OA
############################################################

#Set working directory
setwd("~/R/Birmingham&Walsall/Week3/2011") 
#dir()

### Economically active residents
data_2011 <- read_csv("KS601_EconActivity.csv", skip = 8, n_max = 3223) %>% 
  rename(OA11CD = `2011 output area`, total_residents  = `All usual residents aged 16 to 74`, 
         total_Active = `Economically active`,
         students = `Economically active: Full-time student`,
         total_inactive = `Economically Inactive`, 
         unemployed = `Economically active: Unemployed`, 
         InActive_sick = `Economically inactive: Long-term sick or disabled`) %>%
  mutate(total_Active = total_Active - students, residents_10 = NA) #This is required to make comparable with 2021

#Start dataframe of Townsend index components
Townsend <- data_2011 %>% select(OA11CD, mnemonic, total_residents, total_Active, unemployed)

#Start dataframe of other index components 
Manufacturing <- data_2011 %>% select(OA11CD, mnemonic, total_residents, residents_10, InActive_sick)

### Overcrowding
data_2011 <- read_csv("QS409_Overcrowding.csv", skip = 8, n_max = 3223) %>% 
  rename(total_households = `All categories: Number of persons per room in household`, OA11CD = `2011 output area`) %>% 
  mutate(Overcrowded = `Over 1.0 and up to 1.5 persons per room`+`Over 1.5 persons per room`) 

Townsend <- Townsend %>% left_join(data_2011 %>% select(OA11CD, mnemonic, total_households, Overcrowded))

### Tenure
data_2011 <- read_csv("KS402_Tenure.csv", skip = 8, n_max = 3223) %>% 
  rename(OA11CD = `2011 output area`) 

Townsend <- Townsend %>% left_join(data_2011 %>% select(OA11CD, mnemonic, Owned))

### Cars and vans
data_2011 <- read_csv("KS404_Car.csv", skip = 7, n_max = 3223) %>% 
  rename(OA11CD = `2011 output area`, no_car = `No cars or vans in household`, 
         total_households = `All categories: Car or van availability`) %>%
  rename_with(~ str_extract(., pattern = "^\\d"), -c(OA11CD, no_car, mnemonic, total_households)) 

Townsend <- Townsend %>% left_join(data_2011 %>% select(OA11CD, no_car))

## Manufacturing professions

## Sector of employment
data_2011 <- read_csv("KS605_Industry.csv", skip = 8, n_max = 3223)

Industry_lookup11 <- data.frame(text = colnames(data_2011)[4:21]) %>% 
  mutate(code = str_extract(text, pattern = "^[A-Z]"), 
         industry = str_remove(text, pattern = "^[A-Z]\\s")) %>%
  select(-text)

data_2011 <- data_2011 %>% rename(OA11CD = `2011 output area`, Total_employed = `All categories: Industry`) %>% 
  rename_with(~ str_extract(., "^[A-Z]"), -c(OA11CD, mnemonic, Total_employed)) 

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, mnemonic, Total_employed, Manufacturing = C))

## Type of Employment 

data_2011 <- read_csv("KS608_Occupation.csv", skip = 8, n_max = 3223)

Occupation_lookup11 <- data.frame(text = colnames(data_2011)[4:12]) %>% 
  mutate(code = str_extract(text, pattern = "^\\d"), 
         occupation = str_remove(text, pattern = "^\\d\\.\\s")) %>%
  mutate(occupation = str_remove(occupation, pattern = "\\soccupations$")) %>%
  select(-text)

data_2011 <- data_2011 %>% rename(OA11CD = `2011 output area`, 
                                  total = `All categories: Occupation`) %>% 
  rename_with(~ str_extract(., "^\\d"), -c(OA11CD, mnemonic, total)) 

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, Skilled = `5`, Semi_skilled = `8`, Unskilled = `9`)) %>%
  mutate(Foreman = NA) %>% relocate(Foreman, .before = Skilled) %>% relocate(Total_employed, .after = Unskilled) %>%
  relocate(Manufacturing, .after = Total_employed)

## Education
data_2011 <- read_csv("QS501_Education.csv", skip = 8, n_max = 3223) 

data_2011 <- data_2011 %>% rename(OA11CD = `2011 output area`, 
                                  total_working_age = `All categories: Highest level of qualification`) %>% 
  rename_with(~ str_extract(., pattern = "^(\\w+)(?=\\s+[A-Za-z])|^(\\w+\\s+\\d+)(?=\\s)"), -c(OA11CD, mnemonic, total_working_age, Apprenticeship)) %>%
  mutate(Well_Edu = total_working_age - No - `Level 1`)

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, Well_Edu))

## Health 

data_2011 <- read_csv("KS301_Health.csv", skip = 8, n_max = 3223) %>% 
  rename(OA11CD = `2011 output area`, total_persons = `All categories: Long-term health problem or disability`) %>%
  mutate(poor_health = `Bad health`+ `Very bad health`)

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, total_persons, poor_health))

data_2011 <- read_csv("QS303_LLTI.csv", skip = 8, n_max = 3223) %>% rename(OA11CD = `2011 output area`) %>%
  mutate(Disabled = `Day-to-day activities limited a little` + `Day-to-day activities limited a lot`)
                                                                                 
Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, Disabled)) %>% relocate(Disabled, .after = Well_Edu)

Manufacturing_2011 <- Manufacturing %>% select(-mnemonic)
Townsend_2011 <- Townsend %>% select(-mnemonic)

### Check distribution of population 
Townsend_2011 %>% ggplot(aes(x = total_residents)) +geom_boxplot()

summary(Townsend_2011$total_residents)

#Check Manufacturing column names
vars == colnames(Manufacturing_2011)
############################################################
### Load 2001 data by OA
############################################################

setwd("~/R/Birmingham&Walsall/Week3/2001") 
## Townsend component 1 

#Economically Active Residents
data_2001 <- read_csv("KS009a_EconActivity.csv", skip = 6, n_max = 3127) %>% 
  rename(OA01CD = `2001 output area`, total_residents  = `All usual residents aged 16 to 74`, 
         students = `Economically active: Full-time student`, total_Active = `Economically active`, 
         unemployed = `Economically active: Unemployed`, InActive_sick = `Economically inactive: Long-term sick or disabled`) %>%
  mutate(total_Active = total_Active - students)

#Start dataframe of Townsend index components
Townsend <- data_2001 %>% select(OA01CD, mnemonic, total_residents, total_Active, unemployed)

#Start Manufacturing dataframe

Manufacturing <- data_2001 %>% select(OA01CD, mnemonic, total_residents, InActive_sick)

## Townsend component 2 (Overcrowding)
data_2001 <- read_csv("CS052_Overcrowding.csv", skip = 8, n_max = 3127) %>% 
  rename(total_households = `All categories: Number of persons per room in household`, 
                                  OA01CD = `2001 output area`, VeryLow = `Up to 0.5 persons per room`, 
                                  Low = `Over 0.5 and up to 1.0 persons per room`,
                                  Medium = `Over 1.0 and up to 1.5 persons per room`,
                                  High = `Over 1.5 persons per room`) %>%
  mutate(Overcrowded = Medium+High) 

Townsend <- Townsend %>% left_join(data_2001 %>% select(OA01CD, total_households, Overcrowded))

## Townsend component 3

data_2001 <- read_csv("KS018_Tenure.csv", skip = 6, n_max = 3127)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, total = `All categories: Tenure`) %>%
  mutate(Owned = (`Owned outright`+`Owned with a mortgage or loan`)) 

Townsend <- Townsend %>% left_join(data_2001 %>% select(OA01CD, Owned))

## Townsend component 4
data_2001 <-read_csv("KS017_CarsVans.csv", skip = 6, n_max = 3127)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, no_car = `No cars or vans in household`) %>%
  rename_with(~ str_extract(., pattern = "\\d"), -c(OA01CD, no_car, mnemonic)) 
  
Townsend <- Townsend %>% left_join(data_2001 %>% select(OA01CD, no_car))

## Manufacturing professions

## Sector of employment

data_2001 <- read_csv("KS011a_ Industry.csv", skip = 6, n_max = 3127)

Industry_lookup <- data.frame(text = colnames(data_2001)[4:18]) %>% 
  mutate(code = str_extract(text, pattern = "^[A-Z]"), 
                           industry = str_remove(text, pattern = "^.*\\s")) %>%
  select(-text)
                                      
data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, Total_employed = `All categories: Industry`) %>% 
  rename_with(~ str_extract(., "^[A-Z]"), -c(OA01CD, mnemonic, Total_employed)) 

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, mnemonic, Total_employed, Manufacturing = D))

## Type of Employment 

data_2001 <- read_csv("KS012a_Occupation.csv", skip = 6, n_max = 3127)
str(data_2001)

Occupation_lookup <- data.frame(text = colnames(data_2001)[4:12]) %>% 
  mutate(code = str_extract(text, pattern = "^\\d"), 
         occupation = str_remove(text, pattern = "^\\d\\.\\s")) %>%
  mutate(occupation = str_remove(occupation, pattern = "\\soccupations$")) %>%
  select(-text)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, total = `All categories: Occupation`) %>% 
  rename_with(~ str_extract(., "^\\d"), -c(OA01CD, mnemonic, total)) 

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, Skilled = `5`, Semi_skilled = `8`, Unskilled = `9`)) 

## Education
                                                                                                           
data_2001 <- read_csv("KS013_Qualifications.csv", skip = 6, n_max = 3127) 
str(data_2001)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, total_working_age = `All people aged 16-74`) %>% 
rename_with(~ str_remove(., "^Highest\\slevel\\sof\\squalification:\\s"), -c(OA01CD, mnemonic, total_working_age)) %>% 
  rename_with(~ str_remove(., "\\squalifications.*$"), -c(OA01CD, mnemonic, total_working_age)) %>% 
  mutate(Well_Edu = `Level 2` + `Level 3` + `Level 4` + Other)

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, Well_Edu))

## Health 

data_2001 <- read_csv("UV020_GeneralHealth.csv", skip = 6, n_max = 3127)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`)

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, total_persons = Total, poor_health = `Not good health`))

data_2001 <- read_csv("UV022_LLTI.csv", skip = 6, n_max = 3127)
data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, 
                                  Disabled = `With a limiting long-term illness`, 
                                  Healthy = `Without a limiting long-term illness`) 

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, Disabled)) 

Manufacturing <- Manufacturing %>% mutate(residents_10 = NA, Foreman = NA) %>%
  relocate(residents_10, .after = total_residents) %>%
  relocate(Foreman, .after = InActive_sick) %>%
  relocate(Total_employed, .after = Unskilled) %>%
  relocate(Manufacturing, .after = Total_employed) %>%
  relocate(Disabled, .after = Well_Edu)

Manufacturing_2001 <- Manufacturing %>% select(-OA01CD)
Townsend_2001 <- Townsend %>% select(-OA01CD)

### Check distribution of population 
Townsend_2001 %>% ggplot(aes(x = total_residents)) +geom_boxplot()

summary(Townsend_2001$total_residents)

# Check col names
vars == colnames(Manufacturing_2001)

############################################################
### Load 1991 data by OA
############################################################

setwd("~/R/Birmingham&Walsall/Week3/1991") 
dir()

## Townsend

# Economic Activity
data_1991 <- read_csv("SAS08_EconActivity.csv", skip = 6, n_max = 1966)

#Handy table for column names
lookup <- data.frame(col = colnames(data_1991)) %>%
  mutate(code = str_extract(col, pattern = "^S\\d{2}:\\d*"), description = str_remove(col, pattern = "^S\\d{2}:\\d*\\s"))

data_1991 <- data_1991 %>% rename_with(~ str_extract(., pattern = "^S\\d{2}:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  mutate(total_residents = rowSums(across(`S08:12`:`S08:155`)), #Because there is no total males!! 
         total_Active = `S08:12` + `S08:166` - `S08:89`, #Net of EA students (males only as females not recorded?!)
         unemployed = `S08:122` + `S08:232`, InActive_sick = `S08:122` + `S08:276`)

Townsend <- data_1991 %>% select(`enumeration districts`, mnemonic, total_residents:unemployed)
Manufacturing <- data_1991 %>% select(`enumeration districts`, mnemonic, total_residents, InActive_sick)

# Overcrowding
data_1991 <- read_csv("SAS23_Overcrowding.csv", skip = 6, n_max = 1966) 

data_1991 <- data_1991 %>% rename_with(~ str_extract(., pattern = "^S23:\\d"), -c(`enumeration districts`, mnemonic)) %>% 
  mutate(Overcrowded = `S23:3` + `S23:4`)

Townsend <- Townsend %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, total_households = `S23:1`, Overcrowded))

# Tenure & Car Access
data_1991 <- read_csv("SAS20_TenureCars.csv", skip = 6, n_max = 1966) %>%
  rename_with(~ str_extract(., pattern = "^S20:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  mutate(Owned = `S20:2` + `S20:3`, no_car = `S20:10`)

Townsend <- Townsend %>% left_join(data_1991 %>% select(`enumeration districts`, Owned, no_car)) 

## Manufacturing

# Industry 

str(data_1991)
data_1991 <- read_csv("S73_Industry.csv", skip = 6, n_max = 1966) %>%
  mutate(Total_employed = `S73:1 (Total persons : Total persons )`, Manufacturing = `S73:5 (Total persons : Manufacturing metal etc )` + `S73:6 (Total persons : Other manufacturing )`)

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Total_employed, Manufacturing))

# Occupation

data_1991 <- read_csv("S92_Occupation.csv", skip = 6, n_max = 1966) %>% 
  rename_with(~ str_extract(., pattern = "^S\\d{2}:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  mutate(Foreman = `S92:89` + `S92:90`, Skilled = 	`S92:97`+ `S92:98`, Semi_skilled = `S92:105` + `S92:106`, Unskilled = `S92:113`+ `S92:114`) #These are all manual only for 1991

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Foreman:Unskilled))

# Education - in 1991 this is in the 100% sample 

data_1991 <- read_csv("S12_education.csv", skip = 6, n_max = 1966) %>% 
  rename_with(~ str_extract(., pattern = "^S\\d{2}:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  rename(total_over18 = `S84:1`) %>% mutate(Well_Edu = `S84:7` + `S84:10` + `S84:13`)

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic,Well_Edu))

# Disabled

data_1991 <- read_csv("S12_LLTI.csv", skip = 6, n_max = 1966) %>%
  select(`enumeration districts`, mnemonic, Disabled = `S12:1 (All ages : Total persons )`)

Manufacturing <- Manufacturing %>% #mutate(very_poor_Health = NA, poor_Health = NA) %>% #No self reported health on 1991 or before
  left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Disabled))

##Compare 100 and 10% samples - require total residents in 10% sample

data_1991 <- read_csv("S71_compare.csv", skip = 6, n_max = 1966) %>%
  select(`enumeration districts`, mnemonic, residents_10 = `S71:7 (10 % sample count : Total residents )`)

Manufacturing <- Manufacturing %>% left_join(data_1991) 

Manufacturing <- Manufacturing %>% relocate(residents_10, .after = total_residents) %>% relocate(Total_employed, .after = Unskilled) %>%
  relocate(Manufacturing, .after = Total_employed)

##### There is an issue with linking this data to the weights so we need to change the enumeration district field

Townsend_1991 <- Townsend %>% mutate(ED91CD = str_remove(`enumeration districts`, pattern = " (.*)$")) %>% 
  select(ED91CD, everything()) %>% select(-c("mnemonic", "enumeration districts")) 

Manufacturing_1991 <- Manufacturing %>% mutate(ED91CD = str_remove(`enumeration districts`, pattern = " (.*)$")) %>% 
  select(ED91CD, everything()) %>% select(-c("mnemonic", "enumeration districts")) %>% mutate(total_persons = NA, poor_health = NA)

# Check col names
vars == colnames(Manufacturing_1991)

############################################################
### Check distribution of population - add flag 13th May ###
############################################################

Townsend_1991 %>% ggplot(aes(x = total_households)) +geom_histogram()

Count_zeros <- Townsend_1991 %>% summarise(zero_households = sum(total_households == 0, na.rm = TRUE))

zero_ED_1991 <- Townsend_1991 %>% filter(total_households == 0) %>% pull(ED91CD)
write_csv(zero_ED_1991, "~/R/Birmingham&Walsall/Week4/zero_ED_1991.csv")

#### Geography of missingness

library(sf)
library(corrplot)
library(tmap)
library(spdep)
library(spgwr)
library(RColorBrewer)

# This file contains the boundary information
setwd("~/R/Birmingham&Walsall/Week3") 
Boundaries_1991 <- read_sf("england_ed_1991.shp") %>% mutate(ED91CD = str_remove(label, pattern = "^\\d{2}"))

#Check this files geometry is okay - do not run again as takes forever
### plot(st_geometry(Boundaries_1991))

Check <-Boundaries_1991 %>% right_join(Townsend_1991) 
tm_shape(Check) + tm_fill("flag_missing", fill.scale = tm_scale_categorical("brewer.purples")) + tm_borders(fill_alpha=0.4)

######### Save files

Type <- c("Townsend", "Manufacturing")
Year <- c("2021", "2011", "2001", "1991")

files <- expand.grid(Type, Year) %>% mutate(df_name = str_c(Var1, "_", Var2), file_name = str_c(df_name, ".csv")) %>% select(df_name, file_name)

setwd("~/R/Birmingham&Walsall/Week4") 

files %>% rowwise() %>% pwalk(function(df_name, file_name){
  df_object <- get(df_name, envir = .GlobalEnv)  
  write_csv(df_object, file_name)})


