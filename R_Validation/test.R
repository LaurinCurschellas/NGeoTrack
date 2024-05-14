library(devtools)
library(roxygen2)

devtools::install_github("https://github.com/LaurinCurschellas/NGeoTrack")
library(NGeoTrack)


chg_bsu <- gather_change(type = "grunnkrets", from = 1999, to = 2018)
stat_bsu <- status_quo(type = "grunnkrets", from = 1999, to = 2018)

stat_mun <- status_quo(type = "kommune", from = 1999, to = 2018)
chg_mun <- gather_change(type = "kommune", from = 1999, to = 2018)

stat_flk <- status_quo(type = "fylket", from = 1999, to = 2018)
chg_flk <- gather_change(type = "fylket", from = 1999, to = 2018)

stat_list <- list(stat_mun, stat_flk)
chg_list <- list(chg_flk, chg_mun)


key_joint <- EtE_changes(df_status = stat_list, df_change = chg_list, from = 1999 , to = 2018, jointly = TRUE)

overlap <- check_overlap(key_joint)
