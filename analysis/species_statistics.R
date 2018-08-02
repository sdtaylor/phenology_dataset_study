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

########################################
# Suppliment table of species names and sample sizes
library(kableExtra)
library(knitr)

# get sample sizes from the predictions file
predictions = read_csv(config$predictions_file) %>%
  select(data_type, observation_source, species, observation_id) %>%
  distinct()
#Keep only species that are present in NPN dataset
npn_species = predictions %>%
  filter(observation_source == 'npn') %>%
  select(species) %>%
  distinct()
sample_sizes = predictions %>%
  filter(species %in% npn_species$species) %>%
  mutate(phenophase = as.numeric(stringr::word(species,2,2, ' - ')),
         species = stringr::word(species,1,1,' - ')) %>%
  group_by(data_type, observation_source, species, phenophase) %>%
  tally()

phenophase_types = tribble(
  ~phenophase,~phenophase_type,
  371,'Budburst',
  480,'Budburst',
  488,'Budburst',
  496,'Budburst',
  501,'Flowers'
)

suppliment_species_table = sample_sizes  %>%
  spread(data_type, n) %>%
  left_join(phenophase_types) %>%
  mutate(display_text= paste0(train,' (',test,')')) %>%
  select(-train,-test) %>%
  spread(observation_source, display_text,fill='-')

kable(suppliment_species_table, 'latex')
