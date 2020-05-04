library(tidyverse)
library(auk)
library(ggmap)
library(lubridate)
theme_set(theme_minimal())

# Load data Canada Goose Sightings, Jan 2019 - Jan 2020; filtered to US & CAN
ebd_path <- '/Users/chad/Documents/avian/data/ebd_cangoo_201901_202001_relMar-2020'
ebd_file <- paste(ebd_path,"ebd_canadagoose_uscanada_2019.txt",sep='/')
goose_uscan <- ebd_file %>% read_ebd()



goose_uscan$month <- month(goose_uscan$observation_date, label=TRUE)
goose_uscan$dayofyr <- yday(goose_uscan$observation_date)
goose_uscan$latitude_bin <- cut(goose_uscan$latitude,c(min(goose_uscan$latitude)-1,35,40,45,50,55,max(goose_uscan$latitude)+1))
goose_uscan$latitude_bin <- factor(goose_uscan$latitude_bin)

goose_sample <- goose_uscan %>% sample_n(10000)

uscanada_map <- get_map('Nebraska',source='google',maptype='terrain',zoom=4,color='bw')
base_map <- ggmap(uscanada_map) + ylim(c(25,57))

northerly_map <- get_map('North Dakota',source='google',maptype='terrain',zoom=3,color='bw')


# -- Data exploration

# obs by date

date_obs <- goose_uscan %>% group_by(observation_date) %>% summarise(obs = n())

month_latitude_obs <- goose_uscan %>% group_by(month,latitude_bin) %>% summarize(obs = n())

base_map + geom_point(data=goose_sample,aes(longitude,latitude,color=latitude_bin),alpha=0.3)


ggplot(month_latitude_obs,aes(month,obs,color=factor(latitude_bin)))+geom_point()

date_latitude_obs <- goose_uscan %>% group_by(observation_date,latitude_bin) %>% summarize(obs = n())
ggplot(date_latitude_obs,aes(observation_date,obs))+geom_point(aes(color=latitude_bin))+
  geom_smooth(span=0.3,color='black')+
  facet_wrap(~fct_rev(latitude_bin),scales = 'free_y',ncol = 1)




ggplot(goose_sample,aes(observation_date,latitude))+geom_point(alpha=0.3,aes(color=dayofyr))+
  scale_color_gradientn(colours = rainbow(7))+
  geom_smooth(color='black',lty=2,size=0.5,se=FALSE)

month_median_lat <- goose_uscan %>% group_by(month) %>% summarise(median_lat = median(latitude))
ggplot(goose_uscan,aes(latitude))+geom_histogram(alpha=0.5,bins=50,aes(fill=month))+
  facet_wrap(~month,nrow = 3)+
  geom_vline(data=month_median_lat,aes(xintercept = median_lat),lty=2,size=0.2)

state_obs <- goose_uscan %>% group_by(state,month) %>% summarise(obs = n()) %>% arrange(desc(obs))


  
ggplot(date_obs,aes(observation_date,obs))+geom_line()





base_map + geom_point(data=goose_uscan,aes(longitude,latitude,color=dayofyr),alpha=0.5)+
  scale_colour_gradient2(low = 'blue', mid='red', high='blue')

base_map + geom_point(data=goose_sample,aes(longitude,latitude),alpha=0.3,color='red')+  
  facet_wrap(~month(observation_date, label=TRUE),nrow = 3)
  



