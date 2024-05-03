library(devtools)
library(roxygen2)

devtools::install_local("C:/Users/s16062/OneDrive - Norwegian School of Economics/Dokumenter/03_Packages/NGeoTrack_0.1.0.tar.gz")
library(NGeoTrack)

change_bsu <- gather_change(type = "grunnkrets", from = 1999, to = 2015)
status_bsu <- status_quo(type = "grunnkrets", from = 1999, to = 2015)

status_mun <- status_quo(type = "kommune", from = 1999, to = 2015)
change_mun <- gather_change(type = "kommune", from = 1999, to = 2015)


stat_list <- list(status_mun, status_bsu)
chg_list <- list(change_bsu, change_mun)


key_joint <- EtE_changes(df_status = stat_list, df_change = chg_list, from = 1999 , to = 2015, jointly = FALSE)

check_overlap(key_joint)
