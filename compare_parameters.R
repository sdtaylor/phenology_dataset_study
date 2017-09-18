library(tidyverse)
library(broom)

all_parameters = read_csv('./results/model_parameters.csv') %>%
  filter(parameter_name!='run_time') %>%
  filter(!species %in% c('heracleum maximum', 'achillea millefolium')) #can't do these till github #16 is fixed

#Pull out phenophase
all_parameters = all_parameters %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

all_parameters$phenophase = ifelse(all_parameters$dataset == 'jornada', 501, all_parameters$phenophase)
all_parameters$phenophase = as.numeric(all_parameters$phenophase)

#Keep only species that are present in NPN dataset
npn_species = all_parameters %>%
  filter(dataset == 'npn') %>%
  select(species) %>%
  distinct()

all_parameters = all_parameters %>% 
  filter(species %in% npn_species$species)


############################################################################
#The distribution of all parameters derived using bootstrapping
make_parameter_histograms = FALSE

save_histogram = function(r){
  histogram_data = all_parameters %>%
    filter(species==r$species, phenophase==r$phenophase, model==r$model, parameter_name==r$parameter_name, dataset==r$dataset)
  plot_name = paste0('parameter_histograms/parameter_',r$id,'.png')
  histogram = ggplot(histogram_data, aes(value)) +
    geom_histogram(bins=50) +
    facet_wrap(species~phenophase~model~parameter_name~dataset)
  ggsave(plot_name, plot=histogram, height=20, width=20, units = 'cm', limitsize = FALSE)
}

if(make_parameter_histograms){
  possible_histograms = all_parameters %>% 
    select(species,phenophase,model,parameter_name,dataset) %>%
    distinct() %>%
    mutate(id = 1:n()) %>%
    purrrlyr::by_row(save_histogram)
}
############################################################################
#Statistical test of parameters
#http://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions
stats_test=function(x_mean, x_sd ,y_mean, y_sd){
  #x_mean = mean(x)
  #x_sd   = sd(x)
  #y_mean = mean(y)
  #y_sd   = sd(y)
  z = (x_mean-y_mean) / sqrt(y_sd^2 + x_sd^2)
  p_value = 2 * pnorm(abs(z), lower.tail = F)
}

parameters_to_log_transform = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
model,parameter_name,transform 
uniforc,b,yes
uniforc,F,yes
unichill,a_c,yes
unichill,C,yes
unichill,b_f,yes
unichill,F,yes')

npn_parameters = all_parameters %>%
  filter(dataset=='npn') %>%
  left_join(parameters_to_log_transform, by=c('model','parameter_name')) %>%
  mutate(transform = ifelse(is.na(transform), 'no', transform)) %>%
  mutate(value = ifelse(transform=='yes', log(abs(value)), value)) %>%
  group_by(model, parameter_name, species, phenophase) %>%
  summarize(npn_mean=mean(value), npn_sd=sd(value)) %>%
  ungroup()

p_values = all_parameters %>%
  filter(dataset!='npn') %>%
  left_join(parameters_to_log_transform, by=c('model','parameter_name')) %>%
  mutate(transform = ifelse(is.na(transform), 'no', transform)) %>%
  mutate(value = ifelse(transform=='yes', log(abs(value)), value)) %>%
  group_by(dataset, model, parameter_name, species, phenophase) %>%
  summarize(parameter_mean=mean(value), parameter_sd=sd(value)) %>%
  ungroup() %>%
  left_join(npn_parameters, by=c('model', 'parameter_name','species', 'phenophase')) %>%
  mutate(p_value = stats_test(x_mean=parameter_mean, x_sd=parameter_sd, 
                              y_mean=npn_mean, y_sd=npn_sd)) %>%
  select(dataset,model,parameter_name,species,p_value)



###############################################################################
#scatter plots of npn vs long term datasets

leaf_phenophases = c(371, 496, 488)
flower_phenophases = c(501)

parameter_means = all_parameters %>%
  filter(phenophase %in% flower_phenophases) %>%
  group_by(species, parameter_name, dataset, model) %>%
  summarise(param_mean = mean(value)) %>%
  ungroup() 

npn_paramters = parameter_means %>%
  filter(dataset=='npn') %>%
  spread(dataset, param_mean)

parameter_means = parameter_means %>%
  filter(dataset!='npn') %>%
  left_join(npn_paramters, by=c('species','parameter_name','model'))

is_sig = parameter_means %>%
  left_join(p_values, by=c('dataset','species','parameter_name','model')) %>%
  filter(p_value<=0.05)

#Setup colors for 20+ species
#color_count = length(unique(parameter_means$species))
#getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
#set.seed(1)
#graph_palette = sample(getPalette(color_count))

unichill=ggplot(filter(parameter_means, model=='unichill'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=4) +
  geom_point(data=filter(is_sig, model=='unichill'), size=1.5, color='black', aes(shape=dataset)) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  #scale_color_manual(values = graph_palette) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Unichill Model", x='')
uniforc=ggplot(filter(parameter_means, model=='uniforc'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=4) +
  geom_point(data=filter(is_sig, model=='uniforc'), size=1.5, color='black', aes(shape=dataset)) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Uniforc Model", x='')
gdd=ggplot(filter(parameter_means, model=='gdd'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=4) +
  geom_point(data=filter(is_sig, model=='gdd'), size=1.5, color='black', aes(shape=dataset)) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "GDD Model", x='')
linear_temp=ggplot(filter(parameter_means, model=='linear_temp'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=4) +
  geom_point(data=filter(is_sig, model=='linear_temp'), size=1.5, color='black', aes(shape=dataset)) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Linear Model", x='')
naive=ggplot(filter(parameter_means, model=='naive'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=4) +
  geom_point(data=filter(is_sig, model=='naive'), size=1.5, color='black', aes(shape=dataset)) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Naive Model", x='')

legend = cowplot::get_legend(ggplot(filter(parameter_means, model=='uniforc'), aes(x=npn, y=param_mean, color=dataset, group=dataset))+
                               geom_point())

empty_space = ggplot(filter(parameter_means, model=='naive'), aes(x=npn, y=param_mean, color=species, group=species)) +
  #geom_point(size=4, aes(shape=dataset)) +
  #geom_abline(intercept=0, slope=1) +
  #facet_grid(~parameter_name, scales='free') + 
  theme_bw() 

empty_space = grid::textGrob('')
complex_layout = rbind(c(2,1,1,1,7,7,7,1),
                       c(3,3,1,1,7,7,7,1),
                       c(4,4,4,1,7,7,7,1),
                       c(5,5,5,5,1,1,1,1),
                       c(6,6,6,6,6,6,6,6))
#                                      1       2(1)     3(2)    4(3)  5(4)      6(8)         7
whole_plot=gridExtra::grid.arrange(empty_space,naive, linear_temp, gdd, uniforc, uniforc, legend, layout_matrix=complex_layout,
                        left = 'Long Term Dataset Derived Parameter Estimates',
                        bottom = 'NPN Derived Parameter Estimates')

ggsave('param_comparison_flower.png', plot=whole_plot, height=40, width=80, units = 'cm')

####################################################33
#Test for normality among parameters
normality_test = function(x){
  stats::shapiro.test(x)$p.value
}
is_normal = all_parameters %>%
  group_by(dataset, model, parameter_name, species) %>%
  summarize(normality_p_value =  normality_test(value))

is_normal = arrange(is_normal, normality_p_value)
for(i in 1:50){
  x = all_parameters %>% 
    filter(dataset==is_normal$dataset[i],model==is_normal$model[i], parameter_name==is_normal$parameter_name[i], 
           species==is_normal$species[i])
  plot_title = paste(is_normal$dataset[i], is_normal$model[i], is_normal$parameter_name[i], is_normal$species[i], sep='-')
  hist(x$value, breaks=100, main=plot_title)
  try(hist(log(abs(x$value)), main=paste0(plot_title,'-log')))
}


