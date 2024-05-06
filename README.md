# NGeoTrack 

### Harmonize Norwegian Administrative-Unit Identifiers across time. 

Identifiers of administrative units (municipalities or BSU) are subject to change. Most changes occur because of top-level mergers or splits. 
Inconsistent identifiers can inhibit the use of simple fixed effects or make it impossible to distinctly identify movers. 

This package harmonizes administrative units into clusters that are uniquely tractable across the whole period, specified by the user. It assigns new, stable identifiers 
to each administrative unit, and allows for joint harmonizing of a low- and top-level type of administrative unit. 


***Important Note***:  The procedure is an extension to, and largely based on the following two repositories. In case you use these functions, make sure to cite the original authors. ~ 
[Norgeo](https://youtu.be/8bh238ekw3](https://github.com/helseprofil/norgeo)https://github.com/helseprofil/norgeo "@embed"), and [NoRwayGeo](https://github.com/eirikberger/NoRwayGeo "@NoRwayGeo") \

The official changes and lists of valid location codes are published by Statistics Norway ([SSB](https://www.ssb.no/klass/klassifikasjoner/1 "@SSB"))

## Installation 
```R 
# install.packages("devtools") 
# library(devtools)
devtools::install_github("https://github.com/LaurinCurschellas/NGeoTrack")
```

**Dependencies**: [`dplyr`](https://dplyr.tidyverse.org/ "@dplyr"), [`httr2`](https://httr2.r-lib.org/ "@httr2"), and [`igraph`](https://igraph.org/ "@igraph") 

*An application of tractable clusters is described in the [Appendix](Appendix/Appendix.md)*


## Description 
#### [`gather_change()`](https://github.com/LaurinCurschellas/NGeoTrack/blob/main/gather_change.R "@GatherChange")  
Calls the API of Statistics Norway and gathers all the official changes reported to the selected administrative unit in the defined period. \
Syntax:
```R
data1 <- gather_change(type = "grunnkrets" , from = 1990 , to = 2022)
```
The function applies to BSU (```"grunnkrets"```) and municipalities (```"kommune"```). \
The current release of [SSB](@SSB) supports data for the years 1980-2023 and 1950-2023 for BSU and municipalities respectively. 


#### [`status_quo()`](https://github.com/LaurinCurschellas/NGeoTrack/blob/main/Status_quo_type.R "@StatusQuo")
Calls the API and gathers all the valid codes for the full panel of the specified time period.
```R
data2 <- status_quo(type = "grunnkrets" , from = 1990 , to = 2022) 
```


 #### [`EtE_changes()`](https://github.com/LaurinCurschellas/NGeoTrack/blob/main/EtE_Change.R "@EtEChanges") 
**This is the core function**: It generates tractable clusters, and provides a key assigning allowing to assign all observations to their respective cluster. The input for this function is the datasets generated by the above functions.
```R
EtE_changes(df_status = data2 , df_change = data1 , from = 1990 , to = 2022, jointly = FALSE) 
```
**Joint harmonization of BSU and Municipality identifiers**:             
The changes and status quo data sets have to be generated for each type individually, and supplied as a list to `EtE_changes()` - the harmonizing function. 

```R
changes_list <- list(changes_bsu, changes_mun) # The order in list is not relevant. 
status_list <- list(status_bsu, status_mun)

EtE_changes(df_status = status_list , df_change = changes_list , from = 1990 , to = 2022, jointly = TRUE) 
```


#### [`check_overlap()`](https://github.com/LaurinCurschellas/NGeoTrack/blob/main/R/check_overlap.R "@Overlap") 
This function analyses the generated key and returns information on BSU-clusters that overlap the borders of municipality clusters. 
```R
overlap <- check_overlap(df_key) # Prints an overview to the console, summarising the detailed output
# Vector of affected Grunnkrets Clusters
head(overlap$candidates)
[1] 100124 100125 102230 100016 100373
# Dataframe of municipality details
head(overlap$details)
# A tibble: 5 × 5
  Gcluster_id Ccluster_id_1 Ccluster_id_2 Cname_1     Cname_2          
        <int>         <int>         <int> <chr>       <chr>            
1      100124           528           529 Østre Toten Vestre Toten     
2      100125           528           529 Østre Toten Vestre Toten     
3      102230          1444          1449 Hornindal   Stryn            
4      100016          1911        500023 Kvæfjord    Sortland - Suortá
5      100373           704        500016 Tønsberg    Sandefjord  
```


## Background Administrative Units in Norway

-  Basic Statistical Units (BSU/Grunnkrets) 
The Basic Statistical Units (BSU), also referred to as Grunnkrets, are the most granular administrative units in the Norwegian administration. \ As of January 2022, there are a total of 14'100 such units. These units are largely used for administrative purposes by the municipality and national administrators and have a median population size of 250 inhabitants as of 2018. 

- Municipalities (Kommune)
The municipalities are on the higher level administrative unit with their own administration. Municipalities provide some of the public infrastructure independently and serve as a political unit. \ As of January 2022, there are a total of 356 municipalities in Norway. The median population size of a municipality lies at 4'669 inhabitants as of 2018. 

- County (Fylke) 
The counties are the highest-order administrative unit in Norway. They serve as a political unit and provide certain public administrative and service tasks. Norway is comprised of a total of 15 counties. 


