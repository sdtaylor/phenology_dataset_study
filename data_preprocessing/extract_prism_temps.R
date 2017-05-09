#For all NPN sites, extract the 2009-2016 PRISM mean temperature
library(sp)
library(prism)
library(tidyverse)
library(stringr)

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

extracted = as.data.frame(raster::extract(prism_stacked, site_info_spatial)) %>%
  bind_cols(site_info)

extracted = extracted %>%
  tidyr::gather(filename, value, -Site_ID, -dataset) %>%
  dplyr::mutate(date=stringr::word(filename, 5, 5, sep='_'), var=stringr::word(filename, 2, 2, sep='_')) %>%
  dplyr::select(-filename)

write_csv(extracted, '~/data/phenology/site_PRISM_values.csv')
