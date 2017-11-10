library(tidyverse)

# This goes thru all datasets and, for each species/phenophase, randomly
# selects a percentage of observations to be held out of model building
# it creates a new column in the observations file called 'data_type', set
# to either 'train' or 'test'


config = yaml::yaml.load_file('config.yaml')
dataset_configs = config$dataset_configs
test_percentage = config$percent_test_data
set.seed(1)

# Mark 20% of each species/phenophase as hold out for testing
apply_train_test_split = function(df, test_percent){
  total_obs = nrow(df)
  obs_ids = 1:total_obs
  number_of_test_obs = round(total_obs * test_percent)
  obs_in_test = sample(obs_ids, size = number_of_test_obs)

  df$data_type = 'train'
  df$data_type[obs_in_test] = 'test'
  
  return(df)
  }

final_counts = data.frame()
for(dataset in 1:length(dataset_configs)){
  dataset_observation_file = dataset_configs[[dataset]]$observations_data_file
  test_year_begin = dataset_configs[[dataset]]$test_year_begin
  test_year_end   = dataset_configs[[dataset]]$test_year_end
  dataset_name    = dataset_configs[[dataset]]$dataset_name
  
  observations = read_csv(dataset_observation_file) %>%
    group_by(species, Phenophase_ID) %>%
    do(apply_train_test_split(., test_percent = test_percentage)) %>%
    ungroup()
  
  final_counts = observations %>%
    group_by(species, Phenophase_ID, data_type) %>%
    tally() %>%
    mutate(dataset = dataset_name) %>%
    bind_rows(final_counts)
  
  final_counts = observations %>%
    group_by(species, Phenophase_ID) %>%
    tally() %>%
    mutate(dataset = dataset_name, data_type='total') %>%
    bind_rows(final_counts)
  
  write_csv(observations, dataset_observation_file)
}

###################################
# For informative purposes only.
####################################
final_counts = final_counts %>%
  spread(data_type, n)

# Only keep spp represented in at least 2 datasets
final_counts = final_counts %>%
  group_by(species, Phenophase_ID) %>%
  filter(n_distinct(dataset) >= 2) %>%
  ungroup()


