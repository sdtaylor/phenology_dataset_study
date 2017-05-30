#For all NPN sites, extract the 2009-2016 PRISM mean temperature
library(sp)
library(prism)
library(tidyverse)
library(stringr)
library(lubridate)

options(prism.path = "~/data/prism_daily")

#Download all the data
#get_prism_dailys(type = 'tmean', minDate = '2008-07-01', maxDate = '2016-07-01', keepZip = FALSE)

#NPN Site coordinates
site_info = read_csv('~/data/phenology/npn_core/ancillary_site_data.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct() %>%
  mutate(dataset='npn')

#other dataset coordinates
site_info = read_csv('./non_npn_site_info.csv') %>%
  bind_rows(site_info) %>%
  select(-note)

#site_info = site_info[1:50,]

site_info_spatial = SpatialPointsDataFrame(cbind(site_info$Longitude, site_info$Latitude), data=as.data.frame(site_info), 
                                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0'))


prism_stacked = prism_stack(ls_prism_data())

##########################################################
#Extract raw temp data
###########################################################

extracted = as.data.frame(raster::extract(prism_stacked, site_info_spatial)) %>%
  bind_cols(site_info)

extracted = extracted %>%
  tidyr::gather(filename, temp, -Site_ID, -dataset) %>%
  dplyr::mutate(date=stringr::word(filename, 5, 5, sep='_')) %>%
  dplyr::select(-filename)

##########################################################
#Convert to format used in models
###########################################################

temperature_data = extracted %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  mutate(year = year(date), doy = yday(date))

#Limit temp data to fall and mid summer
temperature_data = temperature_data %>%
  filter(doy <=180 | doy >= 240)

#Assign fall temp to the next years growing season.
#Set jan 1 as doy 0, anything before that as negative doy's
temperature_data = temperature_data %>%
  mutate(year = ifelse(doy>=240, year+1, year)) %>%
  mutate(base_date = as_date(paste0(year,'-01-01'))) %>%
  mutate(doy = date - base_date) %>%
  select(-date, -base_date)

#All observations only go till 2016
temperature_data = temperature_data %>%
  filter(year<=2016)

##########################################################
#One temperature data file per dataset
###########################################################

datasets = unique(site_info$dataset)

for(this_dataset in datasets){
  temp_filename = paste0('./cleaned_data/',this_dataset,'_temp.csv')
  temp_subset = temperature_data %>%
    filter(dataset==this_dataset) %>%
    select(-dataset)
  write_csv(temp_subset, temp_filename)
}
