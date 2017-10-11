library(tidyverse)
library(sp)
library(geosphere)

#############################################################
#Get the number of NPN observers for each species observation
data_dir = '~/data/phenology/npn_core/'

#The raw npn data
all_observations = read_csv(paste0(data_dir,'status_intensity_observation_data.csv')) %>%
  select(Site_ID, individual_id = Individual_ID, observer_id = ObservedBy_Person_ID, Phenophase_ID, 
         Observation_Date, status = Phenophase_Status, intensity_id = Intensity_Category_ID,
         intensity = Intensity_Value, Genus, Species) %>%
  mutate(species= tolower(paste(Genus,Species,sep=' ')), 
         year   = lubridate::year(Observation_Date),
         doy    = lubridate::yday(Observation_Date))

#########################################################################
unique_observers_for_each_species = all_observations %>%
  group_by(species) %>%
  summarize(num_observers = n_distinct(observer_id)) %>%
  ungroup()

#########################################################################
#Get the total area observed for each species (~range size, but not quite)

#Get the area of a species distrubtion by drawing a convex hull over points
#75% sure this is calculating as expected. 
get_area=function(lat,lon){
  if(length(lat)<3){return(NA)}
  coords = matrix(c(lon,lat), ncol=2)
  hull=grDevices::chull(coords)
  hull=c(hull, hull[1])
  hull_coord=coords[hull,]
  
  hull_polygon=SpatialPolygons(list(Polygons(list(Polygon(hull_coord)), ID=1)))
  
  #convert area to square km
  return(geosphere::areaPolygon(hull_polygon)/(1000^2))
}

site_info = read_csv(paste0(data_dir,'ancillary_site_data.csv')) %>%
  select(lon=Longitude, lat=Latitude, Site_ID)

unique_sites_for_each_species = all_observations %>%
  select(species, Site_ID) %>%
  distinct() %>%
  left_join(site_info, by='Site_ID')

area_sampled_for_each_species = unique_sites_for_each_species %>%
  group_by(species) %>%
  do(area_sampled = get_area(lat=.$lat, lon=.$lon)) %>%
  ungroup()
area_sampled_for_each_species$area_sampled = unlist(area_sampled_for_each_species$area_sampled)
###################################################################
# Get the distance between all sampling points and the long term study sites

# For sites with multiple plots get the center (average) location
non_npn_site_info = read_csv('non_npn_site_info.csv') %>%
  group_by(dataset) %>%
  summarise(lon = mean(Longitude), lat=mean(Latitude)) %>%
  select(lon, lat, dataset)

#This returns a distance matrix with non_npn_site as columns and npn sites as rows
distances=geosphere::distm(site_info[,c('lon','lat')], non_npn_site_info[,c('lon','lat')])
distances=as.data.frame(distances)
colnames(distances) = non_npn_site_info$dataset
distances$Site_ID=site_info$Site_ID

distances = distances %>%
  gather(dataset, distance, -Site_ID)

# To km
distances$distance = distances$distance / 1000

# For each species, get the mean distance of all it's sampling points
# to the long term sites
average_distance_to_long_term_sites = all_observations %>%
  left_join(distances, by='Site_ID') %>%
  group_by(species, dataset) %>%
  summarise(mean_distance = mean(distance)) %>%
  ungroup() 
#################################################################


species_sampling_data = area_sampled_for_each_species %>%
  left_join(unique_observers_for_each_species, by='species') 

write_csv(species_sampling_data, 'cleaned_data/npn_species_sampling_data.csv')
write_csv(average_distance_to_long_term_sites, 'cleaned_data/npn_mean_distance_to_long_term_sites.csv')
