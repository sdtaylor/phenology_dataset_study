#For all NPN sites, extract the 2009-2016 PRISM mean temperature
library(sp)
library(prism)
library(tidyverse)
library(stringr)
library(lubridate)

###############################################################

process_extracted_prism_data = function(extracted){
  extracted = extracted %>%
    tidyr::gather(filename, temp, -Site_ID, -dataset) %>%
    dplyr::mutate(date=stringr::word(filename, 5, 5, sep='_')) %>%
    dplyr::select(-filename)
  
  #Convert to format used in models

  temperature_data = extracted %>%
    mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
    mutate(year = year(date), doy = yday(date))
  
  #Limit temp data to fall and mid summer
  #temperature_data = temperature_data %>%
  #  filter(doy <=180 | doy >= 240)
  
  #Assign fall (begining in Oct.) temp to the next years growing season.
  #Also set Jan 1 as doy 0, anything before that as negative doy's
  temperature_data = temperature_data %>%
    mutate(year = ifelse(doy>=300, year+1, year)) %>%
    mutate(base_date = as_date(paste0(year,'-01-01'))) %>%
    mutate(doy = date - base_date) %>%
    select(-date, -base_date, -dataset)
  
  #Cuttoff to 2 decimals to save space in the csv files
  temperature_data$temp = round(temperature_data$temp, 2)
  
  return(temperature_data)
}

####################################################################
options(prism.path = "~/data/prism_daily")

#Download all the data
#get_prism_dailys(type = 'tmean', minDate = '1981-01-01', maxDate = '2005-12-31', keepZip = FALSE)

#NPN Site coordinates
site_info = read_csv('raw_data/npn/ancillary_site_data.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct() %>%
  mutate(dataset='npn', start_year=2009, end_year=2016)

#other dataset coordinates
site_info = read_csv('./non_npn_site_info.csv') %>%
  bind_rows(site_info) %>%
  select(-note)


datasets = unique(site_info$dataset)

#Load and extract prism data seperately for each dataset because they each have unique
#ranges between 1981-2016, and loading daily rasters for the entire timeperiod takes forever.

sites_spatial = list()
for(this_dataset in datasets){
  dataset_info = site_info %>%
    filter(dataset == this_dataset)
  
  this_dataset_spatial = dataset_info %>%
    SpatialPointsDataFrame(cbind(.$Longitude, .$Latitude), data=., 
                           proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
  
  #Also get temp data from the year prior to the first observation
  start_year = min(dataset_info$start_year)-1
  end_year   = max(dataset_info$end_year)
  year_range = start_year:end_year
  
  #Only load prism data from years which are needed for this dataset
  prism_files = ls_prism_data(absPath = TRUE) %>%
    mutate(date=stringr::word(files, 5, 5, sep='_'), year=as.numeric(substr(date,1,4))) %>%
    filter(year %in% year_range) %>%
    select(abs_path)
  
  prism_stacked = raster::stack(prism_files$abs_path, quick=TRUE)

  temp_data = as.data.frame(raster::extract(prism_stacked, this_dataset_spatial)) %>%
    bind_cols(dataset_info) %>%
    process_extracted_prism_data() 
  
  temp_filename = paste0('./cleaned_data/',this_dataset,'_temp.csv')
  
  write_csv(temp_data, temp_filename)
  
}
