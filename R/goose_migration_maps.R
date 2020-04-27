library(tidyverse)
library(auk)
library(ggmap)
library(lubridate)

# Load data Canada Goose Sightings, Jan 2019 - Jan 2020; filtered to US & CAN
ebd_path <- '/Users/chad/Documents/avian/data/ebd_cangoo_201901_202001_relMar-2020'
ebd_file <- paste(ebd_path,"ebd_canadagoose_uscanada_2019.txt",sep='/')
goose_uscan <- ebd_file %>% read_ebd()

# -- Data exploration

# obs by date
date_obs <- goose_uscan %>% group_by(observation_date) %>% summarise(obs = n())
state_obs <- goose_uscan %>% group_by(state) %>% summarise(obs = n()) %>%


ggplot(date_obs,aes(observation_date,obs))+geom_line()


uscanada_map <- get_map('Nebraska',source='google',maptype='terrain',zoom=4,color='bw')

base_map <- ggmap(uscanada_map) + ylim(c(25,57))

goose_sample <- goose_uscan %>% sample_n(10000)

goose_sample$date_int <- as.integer(goose_sample$observation_date - as.Date('2019-01-01'))
base_map + geom_point(data=goose_sample,aes(longitude,latitude,color=date_int),alpha=0.1)+
  scale_colour_gradient2(low = 'blue', mid='red', high='blue')

base_map + geom_point(data=goose_sample,aes(longitude,latitude),alpha=0.3,color='red')+  
  facet_wrap(~month(observation_date, label=TRUE),nrow = 3)
  
