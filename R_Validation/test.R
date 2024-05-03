library(devtools)
library(roxygen2)

devtools::install_github("https://github.com/LaurinCurschellas/NGeoTrack")
library(NGeoTrack)
library(dplyr)

change_bsu <- gather_change(type = "grunnkrets", from = 1999, to = 2015)
status_bsu <- status_quo(type = "grunnkrets", from = 1999, to = 2015)

status_mun <- status_quo(type = "kommune", from = 1999, to = 2015)
change_mun <- gather_change(type = "kommune", from = 1999, to = 2015)


stat_list <- list(status_mun, status_bsu)
chg_list <- list(change_bsu, change_mun)


key_joint <- EtE_changes(df_status = stat_list, df_change = chg_list, from = 1999 , to = 2015, jointly = TRUE)

check_overlap(key_joint)
