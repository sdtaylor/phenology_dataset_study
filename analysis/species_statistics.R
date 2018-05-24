library(tidyverse)
config = yaml::yaml.load_file('config.yaml')

all_species = read_csv(config$model_parameter_file) %>%
  filter(!parameter_name %in% c('run_time','num_iterations')) %>%
  select(dataset, species) %>%
  distinct()

#Pull out phenophase
all_species = all_species %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

all_species$phenophase = as.numeric(all_species$phenophase)

#Keep only species/phenophases that are present in NPN dataset
npn_species = all_species %>%
  filter(dataset == 'npn') %>%
  select(species, phenophase) %>%
  distinct()

all_species = all_species %>% 
  filter(paste(species,phenophase) %in% paste(npn_species$species,npn_species$phenophase) )

# This is the final numbers put into the Table 1
lts_sample_sizes = all_species %>%
  filter(dataset!='npn') %>%
  select(dataset, species, phenophase) %>%
  distinct() %>% 
  group_by(dataset, phenophase) %>%
  summarize(n_species = n_distinct(species))
print(paste0('total unique species: ',length(unique(all_species$species))))
print(paste0('total species/phenophase comparisons: ',sum(lts_sample_sizes$n_species)))

# Suppliment table of species names
library(kableExtra)
library(knitr)

phenophase_types = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
phenophase,phenophase_type
371,Budburst
480,Budburst
488,Budburst
496,Budburst
501,Flowers')

suppliment_species_table = all_species %>%
  filter(dataset!='npn') %>%
  left_join(phenophase_types) %>%
  mutate(present='x') %>%
  spread(dataset, present,fill='')

kable(suppliment_species_table, 'latex')
