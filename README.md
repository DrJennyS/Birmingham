# Birmingham

This project is split into two parts: 

################################################################################
### PART (A) Preprocesses the data used for an example project for OLDA5302A ###
################################################################################

The full data for 1971 and 1981 are not available as they are too large to upload. Please see the course material or email j.l.sexton@leeds.ac.uk for access to these. 

The R files included can be summarised as follows: 

aggregate_census.R - This file combines the files listed in the folders named `2021`, `2011`, `2001` and `1991` into two .csv files named `Townsend_YYYY` and `Manufacturing_YYYY`. Some data is combined and renamed. This file uses dplyr verbs. This code could have been improved using purrr (and this might get done in future). 

Census_tables_81_71.R - This file extracts the relevant fields from the 1981 and 1971 data. The fields needed to be manually selected using information on the contents of the SAS tables/filed IDs. The original .CSV files are not available as they are too large. These are renamed and aggregated as appropriate and outputed as the files: `Townsend_1981`, `Manufacturing_1981`, `Townsend_1971` and `Manufacturing_1971` respectively. This file uses dplr and purrr. 

**** THE OUTPUT FROM THESE FILES IS PROVIDED AS THE SOURCE DATA FOR THIS PROJECT. STUDENTS WOULD NOT BE EXPECTED TO DOWNLOAD OR PROCESS MORE DATA UNLESS THEY WANTED TO AND THIS CODE WOULD BE PROVIDED FOR REFERENCE ONLY. ****

###############################################################
##### PART (B) The example project - SPLIT INTO SECTIONS ######
###############################################################

The .shp files are not available as they are too large to upload. Please see the course material or email j.l.sexton@leeds.ac.uk for access to these. 

The R files included can be summarised as follows: 

Compare Areas v3.R - This file extends the ONS Postcode directory (ONSPD) to include the Enumeration district and Ward codes for 1991, 1981 and 1971. This is done by combining the postcode centroid location information in the ONSPD, the dates that postcodes were introduced and the census geography .shp files. The total area covered for 1991, 1981 and 1971 exceeds the city boundaries as some ED's span the current city borders. The file outputs a .shp file. This file uses geographical joins from the sp package and tmaps for visualisations. 

Geography_EDA.R - Uses the postcode lookup .shp file to determine "how much" of a lowest level OA/ED is in each 2021 LSOA. If an OA/ED is entirely within a 2021 LSOA its weight is 1. If an OA/ED is split between more than one 2021 LSOA, the weights list the proportion of postcodes in each 2021 LSOA. The file outputs weight files for each year to be used for the aggregation in Final_data.R. This file uses dplyr verbs. This code could have been improved using purrr (and this might get done in future). 

Final_data.R - This file creates a nested data_frame from all of the prepared data. This data is then efficiently aggregated into a consistent geography and this data is outputted as .csv files. More use of dplyr and purrr. Some zeros are imputted using the most recently recorded data, others are ignored as Enumeration Districts were non-residential in 1991 or earlier as the boundaries were designed for ease of data collection rather than as statistical units.  

Week4_EDA.R - This file is split into four parts: 
(1) Examines missingness and unusual 2021 LSOAs: - This code was used to identify and correct a critical error about the postcodes in areas which only partially overlap with the current city boundaries. (lots of dplyr, ggplot and tmaps)
(2) EDA on Townsend variables and creates a time consistent Townsend Index for Birmingham (lots of dplyr, ggplot and tmaps)
(3) EDA on the Manufacturing variables and creates a time consistent Manufacturing Index for Birmingham.  (lots of dplyr, ggplot and tmaps)

Note that we can not use the overall Indices for any kind of regression model or in the Kmeans clustering algorithm due to data-leakage. 

TBC...

Week5_Kmeans (TidyModels)

Week5_(GW)Regression (TidyModels)

Week5_Infer

Week6_pretty_graphs 

Validate - compare with IMD
