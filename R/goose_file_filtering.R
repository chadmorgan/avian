library(tidyverse)
library(auk)
library(ggmap)
library(lubridate)

# Canada Goose Sightings, Jan 2019 - Jan 2020
ebd_path <- '/Users/chad/Documents/avian/data/ebd_cangoo_201901_202001_relMar-2020'
setwd(ebd_path)

# Filter to Canada & United States
goose_ebird_file <- paste(ebd_path,'ebd_cangoo_201901_202001_relMar-2020.txt',sep='/')
f_out <- paste(ebd_path,"ebd_canadagoose_uscanada_2019.txt",sep='/')

country_filters <- goose_ebird_file %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_country(c("Canada","United States")) %>% 
  # 3. run filtering
  auk_filter(file = f_out) 


goose_uscan <- f_out %>% read_ebd()
