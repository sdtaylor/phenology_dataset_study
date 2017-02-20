library(tidyverse)
#library(broom)

npn_results = read_csv('./results/npn_results.csv') 

#Some NPN species did not have adqequate sample size, filter
#harvar species to ones that did
harvard_results = read_csv('./results/harvard_results.csv') %>%
  filter(species %in% unique(npn_results$species)) %>%
  select(-t1_slope)

all_results = harvard_results %>%
  bind_rows(npn_results) %>%
  rename(T1=t1_int)

all_results = all_results %>%
  gather(Parameter, value, -boostrap_num, -dataset, -species) 

#all_results = all_results %>%
#  filter(species %in% c('acer rubrum', 'vaccinium corymbosum'))

new_names=c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides', 'A. rubrum', 'B. papyrifera', 'A. saccharum', 'P. serotina')
old_names = c('quercus rubra','quercus alba','fagus grandifolia','populus tremuloides','acer rubrum','betula papyrifera','acer saccharum','prunus serotina')

all_results$species = factor(all_results$species, levels=old_names, labels=new_names)
all_results$dataset = factor(all_results$dataset, levels=c('npn','harvard'), labels=c('NPN','Harvard'))
ggplot(all_results, aes(species, value))+
  #geom_boxplot(position = 'dodge', outlier.color = NA, aes(fill=dataset)) +
  geom_violin(position = 'dodge', aes(fill=dataset)) +
  scale_fill_manual(values=c('#0072B2','#E69F00')) +
  facet_wrap(~Parameter, scales = 'free', nrow=5, labeller = label_both) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(size = 8, 
        angle = 0)) +
  labs(x = "Species", y = "Paramter Values",  fill = "Dataset") + 
  theme(legend.title = element_text(colour = NA), 
        legend.position = "bottom", 
        legend.direction = "horizontal")



