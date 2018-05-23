library(tidyverse)
library(viridis)
library(ggrepel)

############################################
# LTS Sites
lts_sites = read_csv('non_npn_site_info.csv') %>%
  group_by(dataset) %>%
  top_n(1, Site_ID) %>%
  ungroup() %>%
  filter(dataset!='coweeta')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')
lts_sites$dataset = factor(lts_sites$dataset, levels = datasets, labels = pretty_dataset_names)

########################################
# NPN sites
npn_sites_used = read_csv('cleaned_data/npn_observations.csv') %>%
  pull(Site_ID) %>%
  unique()

npn_sites = read_csv('~/data/phenology/npn_core/ancillary_site_data.csv') %>%
  select(Site_ID, Latitude, Longitude) %>%
  distinct() %>%
  filter(Site_ID %in% npn_sites_used, Latitude<50) %>%
  mutate(dataset='npn')
###########################################
basemap = map_data('state')
elevation = raster::getData(name='alt', country='usa', level=1)[[1]] %>%
  raster::aggregate(fact=10) %>%
  raster::rasterToPoints() 
elevation = data.frame(elevation)
colnames(elevation) = c("lat", "lon", "elevation")

site_map = ggplot() + 
  geom_raster(data = elevation, aes(x=lat, y=lon, fill=elevation)) +
  scale_fill_gradient(low='white', high='grey50') + 
  geom_polygon(data = basemap, aes(x=long, y = lat, group = group), fill=NA, color='grey20', size=0.5) + 
  geom_point(data=npn_sites, aes(x=Longitude, y=Latitude), color='black', size=2) + 
  geom_point(data=lts_sites, aes(x=Longitude, y=Latitude), color='black', shape=16, size=5) +
  geom_point(data=lts_sites, aes(x=Longitude, y=Latitude), color='white', shape=16, size=3) +
  geom_label_repel(data=filter(lts_sites, dataset %in% c('Hubbard Brook','Harvard Forest')), aes(x=Longitude, y=Latitude, label=dataset), force=10, 
                   label.size = 0.8, segment.size = 1, nudge_y = 2.8, nudge_x = -5) + 
  geom_label_repel(data=filter(lts_sites, !dataset %in% c('Hubbard Brook','Harvard Forest')), aes(x=Longitude, y=Latitude, label=dataset), force=10, 
                   label.size = 0.8, segment.size = 1, nudge_y = -1, nudge_x = 3) + 
  coord_fixed(1.3) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(0,0,0,0),'mm'))
print(site_map)
# Note, I saved this manually using the export tool to get ride of uneeded whitespace in the figure.
ggsave('manuscript/figure_site_map.png', plot=site_map, height=20, width=50, units = 'cm')


