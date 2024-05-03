Sys.setenv(LANG = "en", encoding = "UTF-8")

reqpack <- c("dplyr", "tidyr", "ggplot2", "here", "haven", "httr2")

for(package_name in reqpack){
  
  library(package_name, character.only = TRUE)
  cat("Loaded package:", package_name, "\n")
  
} 

if (here() != "/home/laurin/Air_Pollution"){
  set_here("/home/laurin/Air_Pollution")
  
  cat("The here() directory is set properly \n ")
} 

rm(reqpack, package_name) 


dtaURL <- "/data/prosjekt/human_capital"

# Load Data Sets 
source(here("77Merger_Codes", "NGeoTrack", "gather_change.R"))

df_change <- gather_change(type = "grunnkrets", from = 1990, to = 2022)

df_key <- read.csv(here("02Descriptives", "Data" , "df_key_1990_2021.csv"), header = TRUE) 

df_bustad <- read_dta(file.path(dtaURL, "population/bustad_1968-2022.dta")) %>%
	mutate(
    		year = as.integer(year)
  		) %>%
  	filter(!is.na(grunnkrets) & year %in% c(1990:2021) & !is.na(npid)) %>% 
  	select(c(npid, year, grunnkrets, bostedskommune)) 

df_pearn <- read_dta(file.path(dtaURL, "income/pinnt_1967-2018.dta")) %>% 
	filter(year %in% c(1990:2021)) 

CPI <- read_dta(here("03Data", "Meta_Data" , "cpi.dta")) %>% 
	filter(!is.na(cpi)) %>%
	mutate(
		cpi__multiplier = cpi / 100
	)

cat("All Data Sets are loaded \n")

## ---------- Transformation of Population Data ---------------------

# Variables of interest individual level 
# 	1. prev_Cid     - Previous residence                                                
# 	2. fut__Cid     - Future residence
#	  3. end_9        - Indicator of Unknown Statistical Tract
#	  4. mover_to     - Mover Indicator (Away, To, Any)
#	     mover_aw
#	     move_any     

# Variables of interest on cluster_level 
# 	Population
# 	1. population   - Population Size
#	  2. prev_pop     - Previous Population Size
# 	3. YoY          - Year-on-Year Relative Change Population Size 
# 	4. diff_pop     - Year-on-Year Absolute Population Change
# 
# 	Of Movers
# 	5. N_origin     - Number of distinct origin BSU, abstracting from unknown BSU (To-Movers) 
#	  6. N_destin     - Number of distinct destination BSU, abstracting from unknown BSU (Away-Movers) 
#
# 	Change and Network    - Permanent labels
# 	8. OtO          - One-to-One change
# 	9. Merger       - Merger
#  10. Split        - Split
#  11. n_members    - Network Size

# Merge the Cluster IDs 
df_clusters <- left_join(df_bustad, df_key, join_by("year" == "year", "grunnkrets" == "geoID")) 

# Check missing clusters over time 

mean(is.na(df_clusters$grunnkrets))*100
mean(is.na(df_clusters$cluster_id))*100 

Share_NA <- df_clusters %>% 
  mutate(
    end_9 = ifelse(grepl("9999$", as.character(grunnkrets)) | grepl("9900$", as.character(grunnkrets)), 1, 0),
    end_0 = ifelse(grepl("0000$", as.character(grunnkrets)) , 1, 0)
  ) %>%
	group_by(year) %>%
	summarise(
		Share_NA = round(mean(is.na(cluster_id))*100, digits = 2),
		mean_9  = round(mean(end_9)*100, digits = 2) ,
		mean_0  = round(mean(end_0)*100, digits = 2)
	) 


# After 1999 individuals which are not assigned to a Cluster, perfectly overlap with 
# those that end in 9999. 
# In the period 1990 - 1999 they do not, presumably this is due to a change in the way that SSB reported 
# unknown tracts, before 1999 there was 1 code designated for that use, after the municipality code remained and the 
# individual's grunnkrets ID ended in 9999.

NA_Clusters <- ggplot( ) +
	geom_point(data = Share_NA, 
	           aes(x = year, y = Share_NA, colour = "Share NA") , size = 2) +
  geom_point(data = Share_NA[Share_NA$year > 1999, ], 
             aes(x = year, y = mean_9, colour = "End - 9999")  , size = 1.6) +
  geom_point(data = Share_NA[Share_NA$year < 2000, ],
             aes(x = year, y = mean_0, colour = "End - 0000")  , size = 1.6) +
	labs(
		x = "Year",
		y = "Share Missing Cluster"
	) +
	theme_minimal() + 
  scale_x_continuous(breaks = seq(1990, 2021, by = 4))+
  scale_y_continuous(breaks = seq(0, 1.8, by = 0.2)) +
  scale_colour_manual(
    name = "Encoding of Unkown",
    values = c("Share NA" = "grey15" ,
               "End - 9999" = "#D3156B" ,
               "End - 0000" = "#DA4C00" )
  ) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8)
  ) 

ggsave(
  here("02Descriptives", "Output", "NA_Clusters.pdf"),
  plot = NA_Clusters,
  width = 4.5,
  height = 3.5,
  unit = "in"
)
  

# There is a predominant designation of codes that make up the missing clusters before 1999 

test <- df_clusters %>% 
  filter(is.na(cluster_id) & year %in% c(1990:1999)) %>% 
  mutate(
    end_0 = ifelse(grepl("0000$", as.character(grunnkrets)), 1, 0)
  ) 

round(mean(test$end_0 )*100, digits = 2) # 79.6%

	
rm(Share_NA, test, NA_Clusters)

## ---------------------- Individual Level Transformations -----------

df_pop_dat <- df_clusters %>% 
	group_by(npid) %>% 
	arrange(npid, year) %>%
	mutate(
    
	  # On Grunnkrets level - needed for indicator of unknown tract
	  prev_Gid = lag(grunnkrets, default = first(grunnkrets)),
	  fut_Gid = lead(grunnkrets, default = last(grunnkrets)),
	  
		# On Cluster level - needed for mover indicator
		prev_Cid = lag(cluster_id, default = first(cluster_id)),
		fut_Cid = lead(cluster_id, default = last(cluster_id)),

		# Indicator of Unknown Statistical Tract 
		end_9 = ifelse(grepl("9999$", as.character(grunnkrets)), 1, 0),
			# These are needed for the population level-measures
		prev_9 = ifelse(grepl("9999$", as.character(prev_Gid)), 1, 0),
		fut_9 = ifelse(grepl("9999$", as.character(fut_Gid)), 1, 0),

		# Mover Indicators 
		mover_to = ifelse(cluster_id != prev_Cid, 1, 0), 
		mover_aw = ifelse(cluster_id != fut_Cid , 1, 0),
		move_any = ifelse(mover_to == 1 | mover_aw == 1 , 1, 0)
	) %>%
	ungroup()  


cat("Individual-Level Transformations are completed \n")

# Cluster Level Transformations 

# Only the Population itself has to be computed on the (cluster_id x year) - cell basis
# Thereafter, we need the transformation to happen on the cluster_id level, as to have access to the panel 

df_C_level <- df_pop_dat %>% 
	group_by(cluster_id, year) %>% 
	mutate(
		population = n(), 
		N_origin   = n_distinct(prev_Cid[mover_to == 1 & prev_9 != 1]), 
		N_destin   = n_distinct(fut_Cid[mover_aw == 1 & fut_9 != 1] )
	) %>%
	ungroup() %>% 
  	select(c(cluster_id, year, population, N_origin, N_destin)) %>% 
	distinct() %>% 
	group_by(cluster_id , year) %>% mutate(count = n()) %>% ungroup()

if (all(df_C_level$count == 1)) {
	
	df_C_level <- df_C_level %>% select(-count) 

} else { 

  # Number of Clusters with more than one C-level observation per year 
  
  Num <- length(unique(df_C_level$cluster_id[df_C_level$count > 1 ])) 

  stop(sprintf("Error: There are %d clusters with more than 1 observation per year", Num )) 
}


df_C_level <- df_C_level %>%
	group_by(cluster_id) %>%
       	arrange(year, .by_group = TRUE) %>%
	mutate(
		
		# To ensure we do not assign a YoY change in incomplete time series 
		# and assign a 0-growth to the first year of observation
		first_year = min(year),
		year_diff = ifelse(year == first_year ,1 , year - lag(year)),
 		

		# Changes in population 
		prev_pop = ifelse(year == first_year, population, lag(population)),
		YoY = ifelse(year_diff == 1 , ((population - prev_pop) / prev_pop)*100 , NA), 
		diff_pop = ifelse(year_diff == 1 , population - prev_pop , NA)
		
	) %>% 
  select(-year_diff) %>% 
  ungroup()

df_test <- df_C_level %>% group_by(cluster_id, year ) %>% mutate( count = n()) 

if (mean(df_test$count, na.rm = TRUE) != 1 ) {
  
  Num <- round(mean(df_test$count, na.rm = TRUE) , 2)
  
	cat(sprintf("There are some observations C-level observations that are off \
		    \
		    Mean rate of observation: %.2f \n" , Num)) 
} else{ rm(df_test) } 


df_C_level <- df_C_level %>% 
       select(c(cluster_id, year, population, prev_pop, YoY, diff_pop, N_origin , N_destin, first_year)) 

df_pop_dat <- left_join(df_pop_dat, df_C_level, join_by("cluster_id", "year")) 	

cat("C-level (1.0) are merged to main data frame \n")

## ---------------------- Cluster Level Level Transformations -----------
# Change Type and Network Size 


Chg_Type <- df_change %>% 
  mutate(
    from = as.integer(from),
    to = as.integer(to)
  ) %>% 
  group_by(from, year) %>% 
  mutate(
    n_from = n(),
    dist_from_from = n_distinct(from),
    dist_to_from   = n_distinct(to)
  )  %>% 
  group_by(to , year) %>% 
  mutate(
    n_to = n(),
    dist_from_to = n_distinct(from),
    dist_to_to = n_distinct(to)
  ) %>% 
  ungroup() %>% 
  mutate(
    OtO = ifelse(n_from == 1 & n_to == 1, 1, 0),
    Split = ifelse(dist_from_from == 1 & dist_to_from > 1 , 1, 0),
    Merger = ifelse(dist_from_to  > 1 & dist_to_to == 1, 1, 0)
  ) %>% 
  select(-c(n_from, dist_from_from, dist_to_from, n_to, dist_from_to, dist_to_to))

# Manual check seems to suggest that the assigning works as intended.

Chg_Type <- left_join(Chg_Type , df_key %>% mutate(year = year + 1), join_by("year", "from" == "geoID")) 

Chg_Type <- Chg_Type %>% 
  group_by(cluster_id, year) %>% 
  mutate(
    Merger = max(Merger),
    Split = max(Split),
    OtO = max(OtO), 
    chg_year = year 
  ) %>% 
  ungroup() %>% 
  group_by(cluster_id) %>% 
  mutate(
    n_changes = n_distinct(year)
  ) %>% 
  ungroup() 


# We only carry the type of change in a given year a cluster is subject to  a change, and the total number of changes
# a cluster has ever experienced over to the main data set

df_merge <- Chg_Type %>% 
  select(c(cluster_id , year, OtO, Split, Merger, n_changes)) %>% 
  distinct()

df_pop_dat <- left_join(df_pop_dat , df_merge , join_by("year", "cluster_id"))  

df_pop_dat <- df_pop_dat %>% 
  mutate(
    OtO = ifelse(is.na(OtO), 0, OtO),
    Split = ifelse(is.na(Split), 0, Split),
    Merger = ifelse(is.na(Merger), 0, Merger)
  )

rm(df_merge) 

cat("C-level (1.1) are merged to the main data frame \n")


Networksize <- Chg_Type %>% 
  select(cluster_id, year, Merger, Split ) %>% 
  filter(!(Split == 0 & Merger == 0)) %>% 
  mutate(
    cluster = 1
  ) %>% 
  select(-c(Merger, Split)) %>% 
  distinct()

Networksize <- left_join(df_key, Networksize , join_by("year", "cluster_id")) %>% 
  mutate(
    cluster = ifelse(!is.na(cluster), cluster, 0)
  ) %>% 
  group_by(cluster_id , year) %>% 
  mutate(
    n_members = ifelse(cluster == 1, n_distinct(geoID), 1)
  ) %>% 
  ungroup()

Networksize <- Networksize %>% 
  select(c(cluster_id, year, n_members)) %>% 
  distinct() 

df_pop_dat <- left_join(df_pop_dat , Networksize , join_by("year", "cluster_id"))  

cat("C-level (1.2) are merged to the main data frame \n")

# Plot the Freq and Type of Changes over time

plot_dta <- Chg_Type %>% 
  select(cluster_id, year, OtO, Merger, Split ) %>% 
  distinct() %>% 
  mutate(
    Type = case_when(
      (Split == 1 & Merger == 0) ~ "Splits",
      (Split == 0 & Merger == 1) ~ "Mergers",
      (Split == 1 & Merger == 1) ~ "Both",
      TRUE ~ NA_character_
    ),
    Type = as.factor(Type)
  ) %>%
  filter(!is.na(Type))

Freq_Chg_Type <-  ggplot(data = plot_dta, aes(x = year, fill = Type)) +
  geom_bar(stat = "count", position = "stack") +
  scale_fill_manual(name = "Type of Change",
                    values = c("Splits" = "#6C2233", "Mergers" = "#D3156B", "Both" = "#DA4C00"))+
  theme_minimal() +
  labs(
    x = "Year",
    y = "Frequency"
  ) +
scale_x_continuous(breaks = seq(min(plot_dta$year), max(plot_dta$year), by = 4)) +
  scale_y_continuous(breaks = seq(0, 55, by = 5)) +
  theme(
    text = element_text(size =11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9, family = "sans"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8,0.8)
  ) +
  annotate("text",
           x = 2015.8, y = 33.9,
           label = "Total - 258",
           color = "grey15",
           size = 4,
           fontface = 2)

ggsave(
  here("02Descriptives", "Output" , "Frequency_Change_by_Year.pdf"),
  plot = Freq_Chg_Type,
  width = 4.5,
  height = 3.5,
  unit = "in"
)


rm(plot_dta, Freq_Chg_Type)


## - Networksize 

N_size <- ggplot(data = Networksize %>% filter(n_members != 1), aes(x = n_members)) +
  geom_bar(fill = "grey15") +
  labs(
    y = "Count",
    x = "Members"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  scale_y_continuous(breaks = seq(0 , 188, by = 20)) 

ggsave(
  here("02Descriptives", "Output" , "Cluster_Size_Distr.pdf"),
  plot = N_size,
  width = 4.5,
  height = 3.5,
  unit = "in"
)


# -------------------- Transformation of Earnings Data --------- 

df_pearn <- left_join(df_pearn, CPI, join_by(year)) %>% 
  filter(year %in% c(1990:2021))

df_pearn <- df_pearn %>% 
  mutate(
    # Move the zeroes to retain possibility of taking logs
    pearn = ifelse(pearn <= 0, 0.00001, pearn),
    
    # Adjust income to consumer price index
    pearn_cpi = pearn / cpi__multiplier
  ) %>% 
  select(-c("cpi", "cpi__multiplier", "pearn")) %>% 
  group_by(npid, year) %>%
  mutate(
    
    # Only retaining one observation per year and individual (the larges pearn)
    count = n(),
    pearn_cpi = ifelse(count == 1, pearn_cpi, max(pearn_cpi))
  ) %>%
  ungroup() %>%
  select(-count) %>%
  distinct() %>%
  group_by(npid, year) %>%
  mutate(
    count = n()
  ) %>%
  ungroup() 

cat("Earnings Data is cleaned of duplicated observations \n")
  
if(all(df_pearn$count == 1 )){
  
  # If there are no individuals with more than one reported income per year, we compute the income 
  # distribution within each year.
  
  df_pearn <- df_pearn %>%
    group_by(year) %>%
    mutate(
      pearn_pct = ifelse(is.na(pearn_cpi), NA_integer_ , ecdf(pearn_cpi)(pearn_cpi)*100)
    ) %>%
    ungroup() %>%
    select(-count) 
  
  cat("Percentiles within-year are calculated \n")
  
} else{
  # Number of individuals that violate the no-duplicate condition 
  Num <- length(unique(df_pearn$npid[df_pearn$count > 1]))
  
  # Stop the Script and report error with the number of individuals 
  stop(sprintf("There are still duplicated incomes for %d individuals.", Num))
} 
  
# Join the earnings data to the population registry 

df_pop_dat <- left_join(df_pop_dat , df_pearn , join_by("year", "npid")) 


cat("Data Set is complete and ready for saving \n")

write.csv(df_pop_dat , here("02Descriptives", "Data", "df_population.csv"), row.names = FALSE)


cat(paste("Data Set is complete and saved to \"", here("02Descriptives", "Data", "df_population.csv"), "\" \
          \
          End of Script \n", sep = ""))
