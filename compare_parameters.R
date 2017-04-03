library(tidyverse)
library(broom)

all_results = read_csv('./results/all_results.csv') %>%
  rename(T1=t1_int)

#Keep only species that are present in NPN dataset
npn_species = all_results %>%
  filter(dataset == 'npn') %>%
  select(species) %>%
  distinct()

all_results = all_results %>% 
  filter(species %in% npn_species$species)

all_results = all_results %>%
  gather(Parameter, value, -boostrap_num, -dataset, -species) 

parameter_means = all_results %>%
  group_by(species, Parameter, dataset) %>%
  summarise(param_mean = mean(value)) %>%
  spread(dataset, param_mean)

############################################################################
#Statistical test of parameters
#http://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions
stats_test=function(x,y){
  x_mean = mean(x)
  x_sd   = sd(x)
  y_mean = mean(y)
  y_sd   = sd(y)
  z = (x_mean-y_mean) / sqrt(y_sd^2 + x_sd^2)
  p_value = 2 * pnorm(abs(z), lower.tail = F)
}


p_values = all_results %>%
  filter(boostrap_num>0) %>%
  spread(dataset, value) %>%
  group_by(species, Parameter) %>%
  summarise(p_value = stats_test(npn, harvard))

#p_values$text = ifelse(p_values$p_value<0.05, '*', '')
#p_values$text = ifelse(p_values$p_value<0.001, '**', p_values$text)
p_values$text = round(p_values$p_value, 2)

###############################################


#all_results = all_results %>%
#  filter(species %in% c('acer rubrum', 'vaccinium corymbosum'))

new_names=c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides', 'A. rubrum', 'B. papyrifera', 'A. saccharum', 'P. serotina')
old_names = c('quercus rubra','quercus alba','fagus grandifolia','populus tremuloides','acer rubrum','betula papyrifera','acer saccharum','prunus serotina')

p_values$species = factor(p_values$species, levels=old_names, labels=new_names)

#Add in y placement for each variable
p_values = p_values %>%
  left_join(data.frame(Parameter=c('b','c','F','T1'),
                       y_placement=c(-21, -41, -5, -70)))

all_results$species = factor(all_results$species, levels=old_names, labels=new_names)
all_results$dataset = factor(all_results$dataset, levels=c('npn','harvard'), labels=c('NPN','Harvard'))
ggplot(all_results, aes(species, value))+
  geom_boxplot(position = 'dodge', outlier.color = NA, aes(fill=dataset)) +
  #geom_violin(position = 'dodge', aes(fill=dataset)) +
  scale_fill_manual(values=c('#0072B2','#E69F00')) +
  geom_text(data=p_values, aes(x=species, y=y_placement, label=text), size=4) +
  facet_wrap(~Parameter, scales = 'free', nrow=5, labeller = label_both) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(size = 8, 
        angle = 0)) +
  labs(x = "Species", y = "Paramter Values",  fill = "Dataset") + 
  theme(legend.title = element_text(colour = NA), 
        legend.position = "bottom", 
        legend.direction = "horizontal")



