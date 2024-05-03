Sys.setenv(LANG = "en", encoding = "UTF-8")

reqpack <- c("dplyr", "tidyr", "ggplot2", "here", "haven", "httr2")

for(package_name in reqpack){
  
  library(package_name, character.only = TRUE)
  cat("Loaded package:", package_name, "\n")
  
} 

rm(reqpack, package_name) 

dtaURL <- "/data/prosjekt/human_capital"

# Load Data Sets 
source(here("77Merger_Codes", "NGeoTrack", "gather_change.R"))

df_change <- gather_change(type = "grunnkrets", from = 1990, to = 2022)

df_key <- read.csv(here("02Descriptives", "Data" , "df_key_1990_2021.csv"), header = TRUE) 

df_population <- read.csv(here("02Descriptives", "Data", "df_population.csv"), header = TRUE)  

## ------ Check the NAs of the constructed data set ---------- 


plot_dat <- df_population %>%
  select(cluster_id, year) %>% 
  distinct() 

plot_dat <- as.data.frame(table(plot_dat$year)) %>% 
  mutate(
    year = as.integer(Var1) + 1989,
    dev = round(Freq - mean(Freq))
  ) %>% select(-Var1)

Fluctuations <- ggplot(data = plot_dat  , aes(x = year, y = dev)) +
  geom_bar(stat = "identity", fill = "#6C2233") +
  geom_hline(yintercept = 0 , linetype = "dashed", color = "grey15") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Deviation from Mean"
  ) +
  scale_x_continuous(breaks = seq(1990, 2021, by = 4)) +
  scale_y_continuous(breaks = seq(min(plot_dat$dev), 51, by = 10)) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  ) +
  annotate("text",
           x = 2015.8, y = -33.9,
           label = "Mean - 13'130",
           color = "grey15",
           size = 4,
           fontface = 2)

ggsave(
  here("02Descriptives", "Output" , "Fluctuations_Panel.pdf"),
  plot = Fluctuations,
  width = 4.5,
  height = 3.5,
  unit = "in"
)

rm(plot_dat, Fluctuations ) 

# Check whether the missing clusters have an entry for the koomune 

NA_s <- df_population %>% 
  filter(is.na(cluster_id)) %>% 
  select(npid, year , grunnkrets , bostedskommune)

NA_s <- NA_s %>% 
  group_by(npid) %>%
  arrange(npid, year ) %>%
  mutate(
    length_spell = n(),
    first_year = min(year),
    consecutive = ifelse(max(year) == (first_year + length_spell - 1)
                         ,  1, 0)
  ) %>% 
  ungroup() %>%
  mutate(
    # Determine if kommune is always correctly included in grunnkrets
    # As the grunnkrets is stored as an integer it can have lengths of 3,4,7 or 8
    str_l = as.integer(nchar(grunnkrets) ),
    # Read the kommune identifier based according to length
    kommune = ifelse(str_l != 7 , as.integer(substr(grunnkrets, 0, 4)), 
                     as.integer(substr(grunnkrets, 0, 3))),
    is_kom  = ifelse(kommune - bostedskommune == 0, 1, 0)
  ) %>% 
  select(-str_l)

# Length of spell 
round(mean(NA_s$length_spell ), digits = 1)

# Share more than one spell 
round(length(unique(NA_s$npid[NA_s$consecutive != 1]))/ length(unique(NA_s$npid))*100, digits = 1)

# Share correct lead of municipality id in grunnkrets 
round(mean(NA_s$is_kom, na.rm = TRUE)*100, digits = 2)

# -------------------- Closer Look ---------------  
# 
## ------- Clusters with Entrance Mid-Panel

plot_dat <- df_population %>% 
  group_by(cluster_id) %>% 
  mutate(
    first_year = min(year)
  ) %>% 
  filter(first_year != 1990 & year == first_year)

plot_dat <- plot_dat %>% 
  select(c(npid, cluster_id, prev_Gid, prev_Cid, mover_to , mover_aw , population , 
           N_origin, YoY, diff_pop , OtO , Split, Merger, n_members, first_year)) %>% 
  group_by(cluster_id) %>% 
  summarise(
    mover_to = mean(mover_to, na.rm =TRUE),
    mover_aw = mean(mover_aw),
    population = mean(population),
    N_origin = mean(N_origin),
    YoY = mean(YoY),
    diff_pop = mean(diff_pop),
    n_members = mean(n_members),
    first_year = mean(first_year)
  ) 

bins <- seq(0,300, length.out = 41) 

plot_dat <- plot_dat %>% 
  mutate(
    population = ifelse(population >= 300, 300, population),
    bin = cut(population, breaks = bins, labels = FALSE, include.lowest = TRUE),
    bin = ifelse(population >= 300 , 40, bin)
  ) 

plot_dat1 <- plot_dat %>% group_by(first_year) %>% mutate(count = n()) %>% 
  select(first_year, count) %>% distinct()

plot_dat2 <- plot_dat %>% 
  group_by(bin) %>%
  summarise(
    count = n(),
    population = mean(population),
    share_1995 = round(mean((first_year == 1995))*100, digits = 2)
  )

Year_of_Entries <- ggplot(data = plot_dat1  ) +
  geom_bar(aes(x = first_year, y = count), stat = "identity", fill = "grey15") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(1990,2021, by = 4)) +
  scale_y_continuous(breaks = seq(0, 110 , by = 15  )) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  ) +
  annotate("text",
           x = 2015.8, y = 82.5,
           label = "Total Entries - 346",
           color = "grey15",
           size = 4,
           fontface = 1)


New_Entries_Pop_Size <- ggplot(data = plot_dat2  ) +
  geom_bar(aes(x = population, y = count, fill = "Total"), stat = "identity", width = 5) +
  geom_bar(aes(x = population, y = share_1995*count/100, fill = "Share 1995")
           , stat = "identity", width = 5) +
  scale_fill_manual(name = "Frequency",
                    values = c("Total" = "grey15", "Share 1995" = "#DA4C00"))+
  theme_minimal() +
  labs(
    x = "Start - Population",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(0,300, by = 30)) +
  scale_y_continuous(breaks = seq(0, 165 , by = 15  )) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  ) 

ggsave(
  here("02Descriptives", "Output" , "Populations_Size_New_Entries.pdf"),
  plot = New_Entries_Pop_Size,
  width = 4.5,
  height = 3.5,
  unit = "in"
)

ggsave(
  here("02Descriptives", "Output" , "Year_of_Entries.pdf"),
  plot = Year_of_Entries,
  width = 4.5,
  height = 3.5,
  unit = "in"
)


rm(plot_dat, plot_dat1 , plot_dat2, New_Entries_Pop_Size, Year_of_Entries)

## ---- clusters with interrupted Time Series ---- 

Time_Series <- df_population %>% 
  group_by(cluster_id) %>% 
  mutate(
    first_year = min(year)
  ) %>% 
  ungroup() %>% 
  filter(is.na(YoY) & year != first_year) %>% 
  select(c(npid, cluster_id, year, mover_to , mover_aw , population , 
           N_origin, YoY, diff_pop , OtO , Split, Merger, n_members, first_year)) %>% 
  group_by(cluster_id, year) %>% 
  summarise(
    mover_to = mean(mover_to),
    mover_aw = mean(mover_aw),
    population = mean(population),
    N_origin = mean(N_origin),
    YoY = mean(YoY),
    diff_pop = mean(diff_pop),
    n_members = mean(n_members),
    first_year = mean(first_year),
    .groups = "drop_last"
  )

culprits <- unique(Time_Series$cluster_id )

# The Spells are indeed tracked correctly and each interruption is labelled with an NA in the YoY column 
# # For a descriptive of the average number of spells with no observation, its average duration
# and the average population size before the spell, we have to first collapse the data 
# on a year-cluster_id level and then take the average for those rows in which a spell occured 
# (the NA of YoY is always in the first year out of the spell)

df_culprits_Panel <- df_population %>% 
  filter(cluster_id %in% culprits) %>% 
  group_by(cluster_id , year) %>%
  summarise(
    across(c(mover_to, mover_aw, population , prev_pop, YoY, N_origin, N_destin, first_year), 
           list(mean = ~mean(. , na.rm =TRUE)),
           .names = "{.col}"
    ), .groups = "drop_last"
  ) %>% 
  group_by(cluster_id) %>% 
  mutate(
    last_obs = ifelse(is.na(YoY), lag(year), NA_integer_),
    spell_dur = ifelse(is.na(YoY), year - last_obs, NA_integer_),
    n_spells = ifelse(is.na(YoY), sum(!is.na(spell_dur)), NA_integer_)
  )

# Share of Y-O-Y changes that are a Time-Series interruption
df <- df_population %>% select(c(cluster_id, year, population)) %>% distinct()
nrow(df_culprits_Panel[!(is.na(df_culprits_Panel$n_spells)) , ])
round(nrow(df_culprits_Panel[!(is.na(df_culprits_Panel$n_spells)) , ]) / nrow(df)*100, digits = 2)

# Compute simple Overview Statistics 
table <- df_culprits_Panel %>% 
  ungroup() %>% 
  summarise(
    across(c(n_spells, spell_dur, prev_pop),
           list(mean = ~mean(. , na.rm = TRUE)),
           .names = "{.col}")
  )  


# In order to get a picture for the prevalence of spells across years,
# we have to create an empty full panel and merge the information on spells etc.
years <- unique(df_population$year ) 

merge <- df_culprits_Panel %>% 
  select(c(cluster_id,year,population, prev_pop, YoY, first_year) )

plot_dat <- data.frame(
  cluster_id = rep(culprits, each = length(years)),
  year = rep(years, times = length(culprits))
)

plot_dat <- left_join(plot_dat , merge , join_by("cluster_id", "year")) 

plot_dat <- plot_dat %>% 
  group_by(cluster_id) %>% 
  mutate(
  # We only want to count the years as spells if they are after the 
  # cluster has entered the panel for the first time. 
    first_year = min(first_year, na.rm = TRUE)
  ) %>% 
  ungroup() 

plot_dat <- plot_dat %>% 
  filter(year >= first_year) %>%
  mutate(
    # Actual indicator of the year that the spell is active
    spell = ifelse(!is.na(population), 0, 1)
  ) %>% 
  select(c(cluster_id, year, spell)) %>% 
  distinct() %>% 
  group_by(year) %>%  
  summarise(
    count = sum((spell == 1))
  )

Prevalence_And_Start <- ggplot()+
  geom_area(data = plot_dat , aes(x = year , y = count, fill = "Prevalence")
            , stat = "identity", alpha = 0.7) +
  geom_bar(data = df_culprits_Panel[!is.na(df_culprits_Panel$spell_dur), ] ,
           aes(x = last_obs , fill = "Starting")) +
  scale_fill_manual(
    name = "Statistics",
    values = c("Prevalence" = "#6C2233" , "Starting" = "#DA4C00")
  ) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Count"
  ) + 
  scale_x_continuous(breaks = seq(1990,2021, by = 4)) +
  scale_y_continuous(breaks = seq(0, 80 , by = 10  )) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.2,0.8) 
  ) 

ggsave(
  here("02Descriptives", "Output" , "Prevalence_Spell.pdf"),
  plot = Prevalence_And_Start,
  width = 4.5,
  height = 3.5,
  unit = "in"
)

# Second Graph
    
plot_dat2 <- df_culprits_Panel %>% 
  mutate(
    prev_pop_mean = ifelse(is.na(YoY), ifelse(prev_pop > 40, 40 , prev_pop) , prev_pop)
  )

Pop_Size_Pre_Spell <- ggplot(data = plot_dat2[!is.na(plot_dat2$spell_dur), ] )+
  geom_bar(aes(x = prev_pop_mean)) +
  theme_minimal() +
  labs(
    x = "Population Size before Spell",
    y = "Frequency"
  ) +
  scale_x_continuous(breaks = seq(0,40, by = 4)) +
  scale_y_continuous(breaks = seq(0, 125 , by = 10  )) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  ) +
  annotate("text",
           x = 34.2, y = 95,
           label = "Total Spells - 258",
           color = "grey15",
           size = 4,
           fontface = 1)

ggsave(
  here("02Descriptives", "Output" , "Pop_Size_Pre_Spell.pdf"),
  plot = Pop_Size_Pre_Spell,
  width = 4.5,
  height = 3.5,
  unit = "in"
) 

rm(years, table, years, culprits, merge, plot_dat, plot_dat2, Prevalence_And_Start, Pop_Size_Pre_Spell)

## ---- clusters with large Fluctuations ---- 

Large_Fluctuation <- df_population %>% 
  filter(abs(YoY) > 50 & abs(diff_pop) > 20) %>% 
  filter(!is.na(cluster_id)) %>% 
  group_by(cluster_id , year) %>%
  summarise(
    across(c(mover_to, mover_aw, population , prev_pop, diff_pop, YoY, N_origin, N_destin, n_members, first_year), 
           list(mean = ~mean(. , na.rm =TRUE)),
           .names = "{.col}"
    ), .groups = "drop_last"
  ) 


# share of year-on-year changes that are experiencing large fluctuations 
df <- df_population %>% select(c(cluster_id, year, population)) %>% distinct()
round((nrow(Large_Fluctuation) / nrow(df) )*100, digits = 2) # 0.26%
rm(df)


plot_dat1 <- Large_Fluctuation %>% 
  mutate(
    diff_pop = ifelse(diff_pop < -300 , -300, ifelse(diff_pop > 300, 300, diff_pop)) ,
    YoY = ifelse(YoY < -200, -200, ifelse(YoY > 200 , 200, YoY)) ,
    N_origin = ifelse(N_origin > 200 , 200, N_origin),
    N_destin = ifelse(N_destin > 200, 200, N_destin) ,
    flag = ifelse(diff_pop < 0 & N_destin <= 3, 1 , 
                  ifelse(diff_pop > 0 & N_origin <= 3, 1, 0))
  )

Dest_and_Origin <- ggplot( ) +
  geom_histogram(data = plot_dat1[plot_dat1$diff_pop > 0,  ], aes(x = N_origin, fill = "Origins"), position = "dodge", binwidth = 2) +
  geom_histogram(data = plot_dat1[plot_dat1$diff_pop < 0,  ], aes(x = N_destin, fill = "Destinations"), position = "dodge", binwidth = 2) +
  scale_fill_manual(
    name = "Legend",
    values = c("Origins" = "#6C2233" , "Destinations" = "#DA4C00")
  ) +
  theme_minimal() +
  scale_y_continuous( breaks =  seq(0 , 90, by = 10)) +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  labs(
    x = "Num. of Destinations/Origins",
    y = "Count"
  ) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  )
  
  
Size_Fluctuations <- ggplot( ) +
  geom_histogram(data = plot_dat1, aes(x = diff_pop, fill = "Fluctuations"), binwidth = 5) + 
  geom_histogram(data = plot_dat1[plot_dat1$flag == 1,] , 
                 aes(x = diff_pop, fill = "Flagged"), binwidth = 5 ) +
  geom_vline(xintercept = 0, linetype = "dashed" , colour = "grey15") + 
  scale_fill_manual(
     name = "Legend",
     values = c("Fluctuations" = "grey", "Flagged" = "#6C2233")
   ) +
  theme_minimal() +
  scale_y_continuous( breaks =  seq(0 , 80, by = 10)) +
  scale_x_continuous(breaks = seq(-300, 300, by = 50)) +
  labs(
    x = "Difference in Population",
    y = "Frequency"
  ) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8) 
  ) +
  annotate("text",
           x = -200, y = 75 ,
           label = "1337 - Fluctuations",
           color = "grey15",
           size = 4,
           fontface = 1)

ggsave(
  here("02Descriptives", "Output" , "Size_Fluctuations.pdf"),
  plot = Size_Fluctuations,
  width = 4.5,
  height = 3.5,
  unit = "in"
) 


ggsave(
  here("02Descriptives", "Output" , "Dest_and_Origin.pdf"),
  plot = Dest_and_Origin,
  width = 4.5,
  height = 3.5,
  unit = "in"
) 


Large_Fluctuation <- Large_Fluctuation %>% 
  mutate(
    flag = ifelse(diff_pop < 0 & N_destin <= 3, 1 , 
                  ifelse(diff_pop > 0 & N_origin <= 3, 1, 0))
  )

Inspection <- Large_Fluctuation %>%
  filter(flag == 1)
