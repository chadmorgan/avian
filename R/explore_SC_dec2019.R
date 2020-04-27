library(tidyverse)
library(auk)
library(ggmap)
library(lubridate)


# All sightings in South Carolina, December 2019 
ebd_path <- '/Users/chad/Documents/avian/data/ebd_US-SC_201912_201912_relMar-2020/'
sc_ebird_file <- paste(ebd_path,'ebd_US-SC_201912_201912_relMar-2020.txt',sep='')

sc_ebird_all <- sc_ebird_file %>% read_ebd()

bcr_code <- read.delim(paste(ebd_path,'BCRCodes.txt',sep=''),sep='\t')
sc_ebird_all <- merge(sc_ebird_all,bcr_code,by.x = 'bcr_code', by.y='BCR_CODE')

# --- exploratory plots and descriptives --- #
date_count <-  sc_ebird_all %>% group_by(observation_date) %>% summarise(obs = n())
ggplot(date_count,aes(observation_date,obs)) + theme_bw() + geom_point(aes(color=weekdays(observation_date)))+geom_line()

species_count <-  sc_ebird_all %>% 
  group_by(taxonomic_order,scientific_name,common_name) %>% 
  summarise(obs = n()) %>% 
  arrange(desc(obs))



# --- mapping --- #
sc_map <- get_map('south carolina',source='google',maptype='terrain',zoom=7)
sc_bw <- get_map('south carolina',source='google',maptype='terrain',zoom=7,color='bw')

scmap_base <- ggmap(sc_bw) + xlim(c(-83.5,-78.5))+ylim(c(32,35.5))

scmap_base + geom_point(data=sc_ebird_all,aes(longitude,latitude,color=BCR_NAME),alpha=0.5)
scmap_base + geom_point(data=sc_ebird_all,aes(longitude,latitude,color=usfws_code),alpha=0.5)
scmap_base + geom_point(data=sc_ebird_all,aes(longitude,latitude,color=iba_code),alpha=0.5)

top_sighted <- sc_ebird_all %>% filter(common_name %in% list(species_count[1:5,'common_name'][[1]])[[1]])
scmap_base + geom_point(data=top_sighted,aes(longitude,latitude,color=common_name),alpha=0.5)

cardinals <- top_sighted %>% filter(common_name == 'Northern Cardinal')
scmap_base + geom_point(data=cardinals,aes(longitude,latitude,color=observation_date))

scmap_base + geom_point(data=sc_ebird_all %>% filter(common_name == 'Dark-eyed Junco'),aes(longitude,latitude,color=observation_date))
scmap_base + geom_point(data=sc_ebird_all %>% filter(common_name == 'Canada Goose'),aes(longitude,latitude,color=observation_date))

