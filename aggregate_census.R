
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
  rename_with(~ str_remove(., pattern = "^Economically\\sinactive:\\s"), everything())

colnames(data_2021)

#Start dataframe of Townsend index components
Townsend <- data_2021 %>% select(OA21CD, mnemonic, total_residents, total_Active, unemployed)

#Start dataframe of other index components 
Manufacturing <- data_2021 %>% select(OA21CD, mnemonic, total_residents, InActive_sick = `Long-term sick or disabled`)

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

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, mnemonic, Manufacturing = C))

## Type of Employment 

data_2021 <- read_csv("TS063_Occupation.csv", skip = 6, n_max = 3325)

Occupation_lookup21 <- data.frame(text = colnames(data_2021)[4:12]) %>% 
  mutate(code = str_extract(text, pattern = "^\\d"), 
         occupation = str_remove(text, pattern = "^\\d\\.\\s")) %>%
  mutate(occupation = str_remove(occupation, pattern = "\\soccupations$")) %>%
  select(-text)

data_2021 <- data_2021 %>% rename(OA21CD = `2021 output area`, 
                                  total = `Total: All usual residents aged 16 years and over in employment the week before the census`) %>% 
  rename_with(~ str_extract(., "^\\d"), -c(OA21CD, mnemonic, total)) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, Skilled = `5`, Semi_skilled = `8`, Unskilled = `9`))

## Education

data_2021 <- read_csv("TS067_Education.csv", skip = 6, n_max = 3325) 

data_2021 <- data_2021 %>% rename(OA21CD = `2021 output area`, 
                                  total_working_age = `Total: All usual residents aged 16 years and over`) %>% 
  rename_with(~ str_extract(., pattern = "^(\w+)(?=\s+[A-Za-z])|^(\w+\s+\d+)(?=:)"), -c(OA21CD, mnemonic, total_working_age, Apprenticeship)) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, total_working_age, No_edu = No, low_edu = `Level 1`))

## Health 

data_2021 <- read_csv("TS037_General health.csv", skip = 6, n_max = 3325) %>% rename(OA21CD = `2021 output area`, total = `Total: All usual residents`) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, total , very_poor_health = `Very bad health`, poor_health = `Bad health`))

data_2021 <- read_csv("TS038_Disability.csv", skip = 6, n_max = 3325) %>% rename(OA21CD = `2021 output area`, 
                                  Disabled = `Disabled under the Equality Act`) 

Manufacturing <- Manufacturing %>% left_join(data_2021 %>% select(OA21CD, Disabled))

Manufacturing_2021 <- Manufacturing 
Man_vars <- colnames(Manufacturing_2021)

Townsend_2021 <- Townsend
Townsend_vars <- colnames(Townsend_2021)

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
  mutate(total_Active = total_Active - students) #This is required to make comparable with 2021

#Start dataframe of Townsend index components
Townsend <- data_2011 %>% select(OA11CD, mnemonic, total_residents, total_Active, unemployed)

#Start dataframe of other index components 
Manufacturing <- data_2011 %>% select(OA11CD, mnemonic, total_residents, InActive_sick)

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

data_2011 <- data_2011 %>% rename(OA11CD = `2011 output area`, Total = `All categories: Industry`) %>% 
  rename_with(~ str_extract(., "^[A-Z]"), -c(OA11CD, mnemonic, Total)) 

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, mnemonic, Manufacturing = C))

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

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, Skilled = `5`, Semi_skilled = `8`, Unskilled = `9`))

## Education
tail(data_2011)
str(data_2011)
data_2011 <- read_csv("QS501_Education.csv", skip = 8, n_max = 3223) 

data_2011 <- data_2011 %>% rename(OA11CD = `2011 output area`, 
                                  total_working_age = `All categories: Highest level of qualification`) %>% 
  rename_with(~ str_extract(., pattern = "^(\\w+)(?=\\s+[A-Za-z])|^(\\w+\\s+\\d+)(?=\\s)"), -c(OA11CD, mnemonic, total_working_age, Apprenticeship)) 

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, total_working_age, No_edu = No, low_edu = `Level 1`))

## Health 

data_2011 <- read_csv("KS301_Health.csv", skip = 8, n_max = 3223) %>% 
  rename(OA11CD = `2011 output area`, total = `All categories: Long-term health problem or disability`) 

Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, total , very_poor_health = `Very bad health`, poor_health = `Bad health`))

data_2011 <- read_csv("QS303_LLTI.csv", skip = 8, n_max = 3223) %>% rename(OA11CD = `2011 output area`) %>%
  mutate(Disabled = `Day-to-day activities limited a little` + `Day-to-day activities limited a lot`)
                                                                                 
Manufacturing <- Manufacturing %>% left_join(data_2011 %>% select(OA11CD, Disabled))

Manufacturing_2011 <- Manufacturing
Townsend_2011 <- Townsend

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

Townsend <- Townsend %>% left_join(data_2001 %>% select(OA01CD, Own_Home))

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
                                      
data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, total = `All categories: Industry`) %>% 
  rename_with(~ str_extract(., "^[A-Z]"), -c(OA01CD, mnemonic, total)) 

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, mnemonic, Manufacturing = D))

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
  rename(None = No, Students_under18 = `Schoolchildren and full-time students: Age 16 to 17`, 
         Students_over18 = `Schoolchildren and full-time students: Age 18 to 74`) %>%
  select(-c(`Full-time students: Age 18 to 74: Economically active: In employment`, `Full-time students: Age 18 to 74: Economically active: Unemployed`, 
           `Full-time students: Age 18 to 74: Economically inactive`))

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, total_working_age, No_edu = None, low_edu = `Level 1`))

## Health 

data_2001 <- read_csv("UV020_GeneralHealth.csv", skip = 6, n_max = 3127)

data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`) %>% mutate(very_poor_Health = NA)

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, Total, very_poor_Health, poor_Health = `Not good health`))

data_2001 <- read_csv("UV022_LLTI.csv", skip = 6, n_max = 3127)
data_2001 <- data_2001 %>% rename(OA01CD = `2001 output area`, 
                                  Disabled = `With a limiting long-term illness`, 
                                  Healthy = `Without a limiting long-term illness`) 

Manufacturing <- Manufacturing %>% left_join(data_2001 %>% select(OA01CD, Disabled))

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
  mutate(total_residents = rowSums(across(`S08:12`:`S08:155`)), total_Active = `S08:12` + `S08:166` - `S08:89`, 
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
dir()
tail(data_1991)
str(data_1991)

# Industry 

data_1991 <- read_csv("S73_Industry.csv", skip = 6, n_max = 1966) %>%
  mutate(Manufacturing = `S73:5 (Total persons : Manufacturing metal etc )` + `S73:6 (Total persons : Other manufacturing )`)

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Manufacturing))

# Occupation

data_1991 <- read_csv("S92_Occupation.csv", skip = 6, n_max = 1966) %>% 
  rename_with(~ str_extract(., pattern = "^S\\d{2}:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  mutate(Skilled = 	`S92:97`+ `S92:98`, Semi_skilled = `S92:105` + `S92:106`, Unskilled = `S92:113`+ `S92:114`) #These are all manual only for 1991

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Skilled:Unskilled))

# Education

data_1991 <- read_csv("S12_education.csv", skip = 6, n_max = 1966) %>% 
  rename_with(~ str_extract(., pattern = "^S\\d{2}:\\d*"), -c(`enumeration districts`, mnemonic)) %>%
  rename(total_over18 = `S84:1`) %>% mutate(No_edu = NA, low_edu = total_over18 - `S84:7` - `S84:10` - `S84:13`)

Manufacturing <- Manufacturing %>% left_join(data_1991 %>% select(`enumeration districts`, mnemonic, total_over18, No_edu,low_edu))

# Disabled

data_1991 <- read_csv("S12_LLTI.csv", skip = 6, n_max = 1966) %>%
  select(`enumeration districts`, mnemonic, Disabled = `S12:1 (All ages : Total persons )`)

Manufacturing <- Manufacturing %>% mutate(very_poor_Health = NA, poor_Health = NA) %>% #No self reported health on 1991 or before
  left_join(data_1991 %>% select(`enumeration districts`, mnemonic, Disabled))

Townsend_1991 <- Townsend
Manufacturing_1991 <- Manufacturing

############################################################
### Reaggregate 2021 LSOA
############################################################

weights_2011 <- read_csv("weights_2011.csv")
weights_2001 <- read_csv("weights_2001.csv")
weights_1991 <- read_csv("weights_1991.csv")
weights_1981 <- read_csv("weights_1981.csv")
weights_1971 <- read_csv("weights_1971.csv")


############################################################
### Join to 2021 dataset in LONG format
############################################################

