library(tidyverse)
library(lubridate)

data_dir = '~/data/phenology/jornada'

observations = read_csv(file.path(data_dir,'JornadaStudy_287_npp_perennial_plant_phenology_0.csv'), skip = 45)

plants = read_csv(file.path(data_dir,'plant_list.csv')) %>%
  select(Genus, Species, Code) %>%
  distinct()

#Metadata from the raw data file
# COMMENT  - Comment section [*see history log refers to file: JornadaStudy_287_npp_perennial_plant_phenology.his]
#PercentDORMANT = percent of all phenophase observations that are Dormant (= Dormant count divided by count sum of all phenophases)
#PercentNON-REP = percent of all phenophase observations that are Non-reproductive (= Dormant count divided by count sum of all phenophases)
#PercentBUD = percent of all phenophase observations that are Bud (= Bud count divided by count sum of all phenophases)
#PercentFLOWER = percent of all phenophase observations that are Flower (= Dormant count divided by count sum of all phenophases)
#PercentFRUIT = percent of all phenophase observations that are Fruit (= Dormant count divided by count sum of all phenophases)


o = observations %>%
  left_join(plants, by=c('SPP'='Code'))
