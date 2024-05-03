# Information on status
stat_mun <- status_quo(type = "kommune", from = 1999, to = 2018)
# Information on changes
chg_mun <- gather_change(type = "kommune", from = 1999, to = 2018)

#Generate key
key_mun <- EtE_changes(df_status = stat_mun, df_change = chg_mun, from = 1999, to = 2018 , jointly = FALSE)


# Joint harmonization
# repeat information for BSU:
stat_bsu <- status_quo(type = "grunnkrets", from = 1999, to = 2018)
chg_bsu <- gather_change(type = "grunnkrets", from = 1999, to = 2018)

#combine in list
stat_list <- list(stat_bsu, stat_mun) # function handles the ordering independently
chg_list <- list(chg_mun, chg_bsu)

key_joint <- EtE_changes(df_status = stat_list, df_change = chg_list, from = 1999, to = 2018 , jointly = TRUE)
