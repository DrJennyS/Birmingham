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

Week4_EDA - Townsend.R - This file is split into two parts: 
(1) Examines missingness and unusual 2021 LSOAs: - This code was used to identify and correct a critical error about the postcodes in areas which only partially overlap with the current city boundaries. (lots of dplyr, ggplot and tmaps)
(2) EDA on Townsend variables and creates two versions of the Townsend Index for Birmingham (lots of dplyr, ggplot and tmaps)
          (a) Townsend variables are proportions of population. Unemployment and Overcrowding are log-transformed as these are skewed variables. All components are normalised and the z scores are added to form the index. 
          (b) Adjustments have been made when missing/inconsistent total residents/households generate proportions >1. Missing data is left as NA replacing with zeros could misrepresent these areas as close to "average".
          (c) Townsend index is created using (year, lsoa21) to identify points and averages over all of these pairs. As a result the number of lsoa's in each quintitle can vary by year if poverty levels are decreasing/increasing in timee. Townsend index (year)            is created for each individual cennsus date, hence for each time point ~1/5 of the data is placed within each quintile.
  
Week4_EDA - Manufacturing.R - This file is split into two parts: (lots of dplyr, ggplot and tmaps)
(1) EDA on the Manufacturing variables - deals with outliers using winstorisation (x2.5 IQR) 
(2) A manufacturing index is created uses min/max scaling - Manufacturing index adds these factors together so should be one the range [0,5]. As before both a whole sample and census-year approach have been used.   

*This Manufacturing index is of minimal use as we can see that the highest two quintiles only feature at the earlier census dates. This indicates that Manufacturing dependence is significantly decreasing in time over the whole Birmingham area. Within each census, the manufacturing_index appears to be uniformally distributed so is not adding much information!

Week5_Kmeans (Tidymodels, dbscan and factoextra) - I have experimented with a few approaches to k-means clustering here: 

(1) clustering the pairs (year, lsoa21)
(2) clustering lsoa21 points within each census
(3) clustering lsoa21 points using data from multiple dates
(4) clustering the lsoa21 points using the 1971 manufacturing data to identify areas which were most dependent on manufacturing. Then finding clusters within these groups within the remaining data. 

** This is MUCH more work than I would anticipate you being able to do in 10 hours!!! Any one of these three approaches, and either the dbscan algorithm or k-means would be sufficient. The only important thing to note is that non of them are really that useful and (1) is no better than the others due to the breakdown of the clusters over time. The only approach that does not work is clustering the (lsoa21, year) pairs over (Townsend_index, Manufacturing_index) because the transformation of the variables going into Townsend_index generate a normal-random variable. We get "stripes" in the resulting clusters (eek!)

We see that the overall clusters are "high manufacturing", "low manufacturing" combined with "high deprivation" and "low deprivation". Deprevation levels being stable over the 60 year period, but manfacturing decreasing over time. This makes the clusters in (1) not particularly useful. 

The single census clusters are dominated by the Townsend factors into 2 groups with manufacturing being relatively uniformally distributed. This is not a feature of scaling as the original distribution should be preserved. This is reflected in the PCA analysis indicating that ~70% of the variability can be attributed to PC1 (Poverty). 

Clustering with lagged variables does not significantly improve the analysis due to the features being fairly highly correlated over time. 

Approach (4) did give some more interesting clusters of Manfacturing/Non-manfacturing focused areas which have been more/less stable over-time. The data split for (4) is in the additional file: Revised_Modelling.R

I used wss/elbow plot to decide on optimal numbers of clusters, alongside silhouette score. The dominance of two groups could be due to the groups being non-elliptic or spatially dependent. 

I used DBSCAN as a non-parametric model to test my assumption about elliptic-clusters being a poor assumption. DBSCAN identified a large number of "outlier" variables and 1 or 2 other clusters. A graphical representation of these areas indicates that the outliers are geographically clustered in the areas with higher manufacturing levels. 

I have crudely added a coordinate for each area to try to introduce some idea of geographical closeness but this also did not improve the clustering. 

Finally, I have looked at the Moran I score and local-Moran score and note that spatial clustering is identified with a high level of significance. If I had more time this could be represented in a single table neatly. 

Revised_Modelling.R - Here I have written a bootstrap approach to testing whether changes in the Townsend Index rating are different between the high/low manufacturing areas. The initial EDA suggests that higher manufacturing areas have lower variability - implying that poverty is more entrenched. I mentioned on the sample assessment 1 that the infer package is not covered on this programme. While this is true, the ideas are related to those on Statistical Computing.  
