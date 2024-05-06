df_key_join <- NGeoTrack::df_key_join
expl <- check_overlap(df_key = df_key_joine)

expl$candidates
# [1] 100124 100125 100016 100373

expl$details
# Gcluster_id Ccluster_id_1 Ccluster_id_2 Cname_1     Cname_2
#    <int>         <int>         <int> <chr>       <chr>
# 100124           528           529  Østre Toten   Vestre Toten
# 100125           528           529  Østre Toten   Vestre Toten
# 100016        500023          1911  Sortland      Kvæfjord
# 100373           704        500016  Tønsberg      Sandefjord
