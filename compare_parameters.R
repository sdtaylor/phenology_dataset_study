library(tidyverse)
library(broom)
library(cowplot)

all_parameters = read_csv('./results/model_parameters.csv') %>%
  filter(parameter_name!='run_time') 

#Pull out phenophase
all_parameters = all_parameters %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

all_parameters$phenophase = as.numeric(all_parameters$phenophase)

#Keep only species/phenophases that are present in NPN dataset
npn_species = all_parameters %>%
  filter(dataset == 'npn') %>%
  select(species, phenophase) %>%
  distinct()

all_parameters = all_parameters %>% 
  filter(species %in% npn_species$species, phenophase %in% npn_species$phenophase)

# This is the final numbers put into the Table 1
# lts_sample_sizes = all_parameters %>% 
#   filter(dataset!='npn') %>% 
#   select(dataset, species, phenophase) %>% 
#   distinct() %>% group_by(dataset, phenophase) %>% 
#   summarize(n_species = n_distinct(species))


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

#Comparison of parameters in npn vs other datasets
x = all_parameters %>%
  filter(dataset %in% c('hjandrews','npn'),model=='gdd_fixed')

ggplot(x, aes(x=value, group=dataset, fill=dataset)) +
  geom_histogram(bins=50, position = 'identity', alpha=0.7) +
  scale_fill_brewer(palette='Set2') +
  facet_wrap(parameter_name~model~species~phenophase, scales = 'free')

############################################################################
#Statistical test of parameters
npn_parameters = all_parameters %>%
  filter(dataset=='npn') %>%
  rename(npn_value = value) %>%
  select(-dataset)

# p_values = all_parameters %>%
#   filter(dataset!='npn') %>%
#   rename(dataset_value = value) %>%
#   left_join(npn_parameters, by=c('model','parameter_name','bootstrap_num','species','phenophase')) %>%
#   group_by(dataset, model, parameter_name, species, phenophase) %>%
#   #summarise(p_value = ks.test(.$dataset_value, .$npn_value, alternative='two.side', exact=TRUE)$p.value, n=n()) %>%
#   summarise(p_value = wilcox.test(.$dataset_value, .$npn_value, alternative = 'two.sided')$p.value) %>%
#   ungroup()



###############################################################################
#scatter plots of npn vs long term datasets

#Setup colors for 20+ species
#color_count = length(unique(parameter_means$species))
#getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
#set.seed(1)
#graph_palette = sample(getPalette(color_count))


budburst_phenophases = c(371, 496, 488)
flower_phenophases = c(501)

parameter_means = all_parameters %>%
  mutate(phenophase = ifelse(phenophase %in% budburst_phenophases, 'Budburst','Flower')) %>%
  group_by(species, parameter_name, dataset, model, phenophase) %>%
  summarise(param_mean = mean(value)) %>%
  ungroup()

npn_paramters = parameter_means %>%
  filter(dataset=='npn') %>%
  spread(dataset, param_mean)

parameter_means = parameter_means %>%
  filter(dataset!='npn') %>%
  left_join(npn_paramters, by=c('species','parameter_name','model', 'phenophase'))

common_plot_theme = theme(strip.text = element_text(size=10),
                          strip.background = element_rect(fill='grey95'),
                          axis.text = element_text(size=12),
                          axis.title.y = element_text(size=18))

point_size=4
point_shapes = c(17,13)
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7")

alternating=ggplot(filter(parameter_means, model=='alternating'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Alternating", x='') +
  common_plot_theme 
uniforc=ggplot(filter(parameter_means, model=='uniforc'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Uniforc", x='') + 
  common_plot_theme
gdd=ggplot(filter(parameter_means, model=='gdd'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "GDD", x='') + 
  common_plot_theme
gdd_fixed_a=ggplot(filter(parameter_means, model=='gdd_fixed'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Fixed GDD", x='') + 
  common_plot_theme
gdd_fixed_b=ggplot(filter(parameter_means, model=='gdd_fixed', dataset!='jornada'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Fixed GDD", x='') + 
  common_plot_theme
linear_temp=ggplot(filter(parameter_means, model=='linear_temp'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Linear", x='') + 
  common_plot_theme
naive=ggplot(filter(parameter_means, model=='naive'), aes(x=npn, y=param_mean, color=dataset, group=dataset)) +
  geom_point(size=point_size, aes(shape = phenophase)) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~parameter_name, scales='free', nrow=1) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Naive", x='') + 
  common_plot_theme

legend = cowplot::get_legend(ggplot(filter(parameter_means, model=='uniforc'), aes(x=npn, y=param_mean, color=dataset, group=dataset))+
                               geom_point(size=4, aes(shape = phenophase)) +
                               scale_shape_manual(values=point_shapes)  + 
                               scale_color_manual(values=color_pallete) +
                               theme(legend.text = element_text(size = 20), 
                                     legend.title = element_text(size = 25), 
                                     legend.key.size = unit(5, units = 'mm')) +
                               labs(colour = "LTS Dataset", 
                                    shape = "Phenophase"))

empty_space = grid::textGrob('')
complex_layout = rbind(c(2,1,7,7),
                       c(8,1,7,7),
                       c(3,3,1,1),
                       c(4,4,4,1),
                       c(6,6,6,1),
                       c(5,5,5,5))

#                                      1       2(1)     3(2)       4(3)  5(4)      6(3)         7        8       
whole_plot=gridExtra::grid.arrange(empty_space,naive, linear_temp, gdd, uniforc, alternating, legend, gdd_fixed_a, layout_matrix=complex_layout,
                        left = 'Long Term Dataset Derived Parameter Estimates',
                        bottom = 'NPN Derived Parameter Estimates')



ggsave('param_comparison.png', plot=whole_plot, height=40, width=80, units = 'cm')

#####################################################
#####################################################
#####################################################
# Normality tests for individual parameters
# some parameters are log transformed for this since they are
# bounded to either positive or negative

# parameters_to_log_transform = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
# model,parameter_name,transform 
# uniforc,b,yes
# uniforc,F,yes
# gdd,F,yes
# alternating,c,yes
# gdd_fixed,F,yes')
# 
# 
# #Test for normality among parameters
# normality_test = function(x){
#   stats::shapiro.test(x)$p.value
# }
# is_normal = all_parameters %>%
#   left_join(parameters_to_log_transform, by=c('model','parameter_name')) %>%
#   mutate(transform = ifelse(is.na(transform),'no', transform)) %>%
#   mutate(log_value = ifelse(transform=='yes', log1p(value), value)) %>%
#   group_by(dataset, model, parameter_name, species, phenophase) %>%
#   summarize(normality_p_value =  normality_test(value))
# 
# is_normal = arrange(is_normal, normality_p_value)
# for(i in 1:50){
#   x = all_parameters %>%
#     filter(dataset==is_normal$dataset[i],model==is_normal$model[i], parameter_name==is_normal$parameter_name[i],
#            species==is_normal$species[i])
#   plot_title = paste(is_normal$dataset[i], is_normal$model[i], is_normal$parameter_name[i], is_normal$species[i], sep='-')
#   hist(x$value, breaks=100, main=plot_title)
#   try(hist(log(abs(x$value)), main=paste0(plot_title,'-log')))
# }


