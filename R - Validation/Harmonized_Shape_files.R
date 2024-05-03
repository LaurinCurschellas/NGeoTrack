Sys.setenv(LANG = "en", encoding = "UTF-8")

reqpack <- c("dplyr", "tidyr", "ggplot2", "here", "haven", "sf")

for(package_name in reqpack){
  
  library(package_name, character.only = TRUE)
  cat("Loaded package:", package_name, "\n")
  
} 

rm(reqpack, package_name) 



# Read the raw Shape Files and key 

df_key <- read.csv(here("02Descriptives", "Data", "df_key_joint_1990_2021.csv"), header = TRUE)


# BSU
df_key_2021_bsu <- df_key %>%
  filter(year == 2021) %>%
  dplyr::select(c(geoID,Gcluster_id,Gname))

shp_2021_BSU <- here("03Shape_Files", "00 Jointly_Harmonized", "2021", "01Raw_Shape_File", "Grunnkrets") %>%
  read_sf() 

shp_2021_BSU <- shp_2021_BSU[,c(4,11,15)] %>%
  mutate(
    geoID = as.integer(GRUNNKRETS)
  ) %>%
  dplyr::select(c(geoID,geometry)) 


shp_2021_BSU <- left_join(shp_2021_BSU,df_key_2021_bsu, join_by("geoID")) 

st_write(shp_2021_BSU , here("03Shape_Files", "00 Jointly_Harmonized", "2021", "02Cluster_Level", "Grunnkrets", "shp_2021_cluster_bsu.shp"), append = FALSE)


#Kommune 

# Do Not dissolve via the municipality ID, as this would ignore the redrawing of the municipality borders, according to the 
# Manual re-definition of the overlapping grunnkrets. 


df_key_2021_mun <- df_key %>%
  filter(year == 2021) %>%
  dplyr::select(c(geoID,mun_id,Ccluster_id,Cname)) 

shp_2021_mun <- here("03Shape_Files", "00 Jointly_Harmonized", "2021", "01Raw_Shape_File", "Grunnkrets") %>%
  read_sf() 

shp_2021_mun <- shp_2021_mun[,c(4,11,15)] %>%
  mutate(
    geoID = as.integer(GRUNNKRETS)
  ) %>%
  dplyr::select(c(geoID,geometry)) 

shp_2021_mun <- left_join(shp_2021_mun,df_key_2021_mun, join_by("geoID")) 

st_write(shp_2021_mun , here("03Shape_Files", "00 Jointly_Harmonized", "2021", "02Cluster_Level", "Kommuner", "shp_2021_cluster_mun.shp"), append = FALSE)
