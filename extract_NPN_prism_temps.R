#For all NPN sites, extract the 2009-2016 PRISM mean temperature
library(sp)
library(prism)
library(tidyverse)
library(stringr)

options(prism.path = "~/data/prism_daily")

#Download all the data
#get_prism_dailys(type = 'tmean', minDate = '2008-07-01', maxDate = '2016-07-01', keepZip = FALSE)

#This is the original observation file but only keeping the top 20 species by total count
site_info = read_csv('~/data/phenology/observations_top_20.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct()

site_info_spatial = SpatialPointsDataFrame(cbind(site_info$Longitude, site_info$Latitude), data=as.data.frame(site_info), 
                                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0'))


prism_stacked = prism_stack(ls_prism_data())

extracted = as.data.frame(raster::extract(prism_stacked, site_info_spatial))

extracted$Site_ID = site_info$Site_ID

extracted = extracted %>%
  tidyr::gather(filename, value, -Site_ID) %>%
  dplyr::mutate(date=stringr::word(filename, 5, 5, sep='_'), var=stringr::word(filename, 2, 2, sep='_')) %>%
  dplyr::select(-filename)

write_csv(extracted, '~/data/phenology/site_PRISM_values.csv')
