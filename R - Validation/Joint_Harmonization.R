Sys.setenv(LANG = "en", encoding = "UTF-8")

reqpack <- c("dplyr", "here", "raster", "rasterVis"
             , "ncdf4", "igraph", "sf", "tidyr")

for(package_name in reqpack){
  
  library(package_name, character.only = TRUE)
  cat("Loaded package:", package_name, "\n")
  
} 

rm(reqpack, package_name) 


source(here("01Source", "NGeoTrack", "R", "gather_change.R"))
source(here("01Source", "NGeoTrack", "R", "Status_quo_type.R"))
source(here("01Source", "NGeoTrack", "R", "EtE_changes.R"))
source(here("01Source", "NGeoTrack", "R", "check_overlap.R"))


## ----------------------- Municipality Data Sets ---------------------- 

df_change_mun <- gather_change(type = "kommune", from = 1990 , to = 2022)

df_status_mun <- status_quo(type = "kommune", from = 1990 , to = 2022)


## ---------------------- Grunnkrets Data Sets  -------------------------- 

df_change_bsu <- gather_change(type = "grunnkrets", from = 1990, to = 2022)

df_status_bsu <- status_quo(type = "grunnkrets", from = 1990, to = 2022)


## ------------ With the expanded function - generate a jointly harmonised key ------------------------------ 

change_list <- list(df_change_mun , df_change_bsu)
status_list <- list(df_status_bsu , df_status_mun)

df_key <- EtE_changes(df_status = status_list, df_change = change_list , from = 1990, to = 2022, jointly = TRUE) 


## --------------------- Check for existence of overlapping grunnkrets and assign manually ----------

# Function checks for Grunnkrets that are lying in more than one municipality and returns the details 
# of location and municipality candidates. 
candidates <- check_overlap(df_key)$details

# The assignment is made to the municipality in which the majority of the grunnkrets' area lies. 
# This has to be manually validated and re-assigned
assignement <- data.frame(
  candidate = sort(candidates$Gcluster_id),
  assigned_Ccluster = c(500084, 500002, 500002, 500126, 500028, 500157, 500157, 500022, 500196, 500251, 500166 ),
  assigned_mun_id = c(3016, 3403, 3403, 3411, 1870, 3442, 3442, 3804, 4216, 4651, 3453), 
  assigned_Cname  = c("Rakkestad", "Hamar", "Hamar", "Ringsaker", "Sortland - Suortá", "Østre Toten", "Østre Toten", "Sandefjord", "Birkenes", "Stryn", "Øystre Slidre")
) 

df_key <- df_key %>% 
  mutate(
    indicator = ifelse(Gcluster_id %in% assignement$candidate, 1, 0),
    Ccluster_id = ifelse(Gcluster_id %in% assignement$candidate, assignement$assigned_Ccluster[match(Gcluster_id, assignement$candidate)],
                         Ccluster_id),
    mun_id = ifelse(Gcluster_id %in% assignement$candidate, assignement$assigned_mun_id[match(Gcluster_id, assignement$candidate)],
                    mun_id),
    Cname  = ifelse(Gcluster_id %in% assignement$candidate, assignement$assigned_Cname[match(Gcluster_id, assignement$candidate)],
                    Cname)
    ) 

# Last check 

df_check <- df_key %>% 
  group_by(Gcluster_id, year) %>%
  mutate(
    n_Mun = n_distinct(Ccluster_id)
  ) 

if (mean(df_check$n_Mun) == 1) { 
  
  rm(df_check)
  write.csv(df_key, here("02Descriptives", "Data", "df_key_joint_1990_2021.csv"), row.names = FALSE) 
  
  cat("\nManual updating complete",
      "\nThe joint key is generated and free of over-lapping clusters.",
      "\n\nFile Saved:",
      paste("\n\t",here("02Descriptives", "Data", "df_key_joint_1990_2021.csv"), sep = "")) 
  
} else{cat("Double check the observations with more than 1 municipality per year!")}




