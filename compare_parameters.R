library(tidyverse)
#library(broom)

npn_results = read_csv('./results/npn_results.csv')

#Some NPN species did not have adqequate sample size, filter
#harvar species to ones that did
harvard_results = read_csv('./results/harvard_results.csv') %>%
  filter(species %in% unique(npn_results$species)) %>%
  select(-t1_slope)

all_results = harvard_results %>%
  bind_rows(npn_results)

all_results = all_results %>%
  gather(parameter, value, -boostrap_num, -dataset, -species) 

#all_results = all_results %>%
#  filter(species %in% c('acer rubrum', 'vaccinium corymbosum'))

ggplot(all_results, aes(species, value))+
  geom_boxplot(position = 'dodge', outlier.color = NA, aes(fill=dataset)) +
  facet_wrap(~parameter, scales = 'free', nrow=5)



