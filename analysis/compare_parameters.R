library(tidyverse)
library(cowplot)
config = yaml::yaml.load_file('config.yaml')

all_parameters = read_csv(config$model_parameter_file) %>%
  filter(!parameter_name %in% c('run_time','num_iterations')) 

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
  filter(paste(species,phenophase) %in% paste(npn_species$species,npn_species$phenophase) )

############################################################################
# Organize the two spatial models, MSB and M1
# These models add a correction to the Alternating and GDD models, respectivaely,
# to hopefully account for spatial variation. Since fitting these models to LTS
# sites doesn't make sense (as there is no spatial variation in them ) I'll
# compare them to LTS datasets fitted to the original Alternating and GDD models.

# Remove spatial models fit to LTS data
all_parameters = all_parameters %>%
  filter(!(dataset!='npn' & model %in% c('msb','m1')))

# Copy the LTS GDD and Alternating model to compare with the
# corrected NPN ones.
lts_models_parameters = all_parameters %>%
  filter(dataset!='npn', model %in% c('gdd','alternating'))
lts_models_parameters$model = with(lts_models_parameters, ifelse(model=='gdd','m1',
                                                                 ifelse(model=='alternating','msb','unk')))
if(any(lts_models_parameters$model=='unk')){stop('unknown model in lts subset')}  

all_parameters = all_parameters %>%
  bind_rows(lts_models_parameters)

# Remove the additional parameters which are only in the corrected models
all_parameters = all_parameters %>%
  filter(!(model=='msb' & parameter_name=='d')) %>%
  filter(!(model=='m1' & parameter_name=='k'))

rm(lts_models_parameters)
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
  filter(dataset %in% c('harvard','npn'),model=='uniforc',species=='populus tremuloides', phenophase==501)

ggplot(x, aes(x=value, group=dataset, fill=dataset)) +
  geom_histogram(bins=50, position = 'identity', alpha=0.7) +
  scale_fill_brewer(palette='Set2') +
  facet_wrap(parameter_name~model~species~phenophase, scales = 'free')

###########################################################################
# Mann Whitney and/or ks test for parameter distribution comparison.
# Are these results robust to a sample size smaller than 250 bootstraps?
# 
# all_parameters_subset = all_parameters %>%
#   filter(bootstrap_num %in% sample(1:250, size=20))
# 
# #Statistical test of parameters
# npn_parameters = all_parameters_subset %>%
#   filter(dataset=='npn') %>%
#   rename(npn_value = value) %>%
#   select(-dataset)
# 
# p_values = all_parameters_subset %>%
#   filter(dataset!='npn') %>%
#   rename(dataset_value = value) %>%
#   left_join(npn_parameters, by=c('model','parameter_name','bootstrap_num','species','phenophase')) %>%
#   group_by(dataset, model, parameter_name, species, phenophase) %>%
#   #summarise(p_value = ks.test(.$dataset_value, .$npn_value, alternative='two.side', exact=TRUE)$p.value, n=n()) %>%
#   summarise(p_value = wilcox.test(.$dataset_value, .$npn_value, alternative = 'two.sided')$p.value) %>%
#   ungroup()


###############################################################################
#scatter plots of npn vs long term datasets

budburst_phenophases = c(371, 496, 488, 480)
flower_phenophases = c(501)

parameter_means = all_parameters %>%
  mutate(phenophase = ifelse(phenophase %in% budburst_phenophases, 'Budburst','Flower')) %>%
  group_by(species, parameter_name, dataset, model, phenophase) %>%
  summarise(param_mean = mean(value)) %>%
  ungroup()

npn_parameters = parameter_means %>%
  filter(dataset=='npn') %>%
  spread(dataset, param_mean) %>%
  rename(npn_derived_parameter = npn)

parameter_means = parameter_means %>%
  filter(dataset!='npn') %>%
  rename(lts_derived_parameter = param_mean) %>%
  left_join(npn_parameters, by=c('species','parameter_name','model', 'phenophase'))

datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')
parameter_means$dataset = factor(parameter_means$dataset, levels = datasets, labels = pretty_dataset_names)


#################################################################################
# R^2 values

parameter_name_plotmath = tribble(
  ~model, ~parameter_name, ~parameter_symbol,
  'naive','mean_doy', 'widehat(DOY)',
  'gdd_fixed','F','F',
  'linear_temp','intercept','beta[1]',
  'linear_temp','slope','beta[2]',
  'gdd','F','F',
  'gdd','t1','t[1]',
  'gdd','T','T[base]',
  'm1','F','F',
  'm1','t1','t[1]',
  'm1','T','T[base]',
  'm1','k','k',
  'alternating','a','a',
  'alternating','b','b',
  'alternating','c','c',
  'msb','a','a',
  'msb','b','b',
  'msb','c','c',
  'msb','d','d',
  'uniforc','t1','t[1]',
  'uniforc','F','F',
  'uniforc','b','b',
  'uniforc','c','c'
)

r2_values = parameter_means %>%
  group_by(model, parameter_name) %>%
  summarise(r2= 1 - (sum((npn_derived_parameter - lts_derived_parameter)**2) / sum((npn_derived_parameter - mean(npn_derived_parameter))**2)) , n=n()) %>%
  ungroup() %>%
  mutate(r2_text=paste('R^2 == ',round(r2,2)))

# Put the r2 values in the parameter_name column so they are included in the
# labels of the plot
parameter_means = parameter_means %>%
  left_join(r2_values, by=c('model','parameter_name')) %>%
  left_join(parameter_name_plotmath, by = c('model','parameter_name')) %>%
  mutate(parameter_name2 = paste0('list(',parameter_symbol,',', r2_text,')'))

#################################################################################

common_plot_theme = theme(strip.text = element_text(size=9),
                          strip.background = element_rect(fill='grey95'),
                          axis.text = element_text(size=9),
                          axis.title = element_text(size=12))

point_size=3
point_shapes = c(17,13)
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7")

get_subplot = function(model_name, y_label){
  p=ggplot(filter(parameter_means, model==model_name), aes(x=npn_derived_parameter, y=lts_derived_parameter, color=dataset, group=dataset)) +
    geom_point(size=point_size, aes(shape = phenophase)) +
    scale_shape_manual(values=point_shapes) +
    scale_color_manual(values=color_pallete) +
    geom_abline(intercept=0, slope=1) +
    facet_wrap(~parameter_name2, scales='free', nrow=1, labeller = label_parsed) + 
    theme_bw() +
    theme(legend.position = "none") +
    labs(y = y_label, x='') +
    common_plot_theme 
  return(p)
}

alternating=get_subplot(model_name = 'alternating', y_label = "Alternating\n")
uniforc=get_subplot(model_name = 'uniforc', y_label = "Uniforc\n")
gdd=get_subplot(model_name = 'gdd', y_label = "GDD\n")
gdd_fixed=get_subplot(model_name = 'gdd_fixed', y_label = "Fixed GDD\n")
linear_temp=get_subplot('linear_temp', y_label = "Linear\n")
naive=get_subplot('naive', y_label = "Naive\n") 
m1=get_subplot('m1', y_label = "M1")
msb=get_subplot('msb', y_label = "MSB")

legend = cowplot::get_legend(ggplot(filter(parameter_means, model=='uniforc'), aes(x=npn_derived_parameter, y=lts_derived_parameter, color=dataset, group=dataset))+
                               geom_point(size=4, aes(shape = phenophase)) +
                               scale_shape_manual(values=point_shapes)  + 
                               scale_color_manual(values=color_pallete) +
                               theme(legend.text = element_text(size = 14), 
                                     legend.title = element_text(size = 18),
                                    
                                     legend.key.size = unit(5, units = 'mm')) +
                               labs(colour = "LTER Dataset", 
                                    shape = "Phenophase"))

empty_space = grid::textGrob('')
complex_layout = rbind(c(2,1,7,7),
                       c(8,1,7,7),
                       c(3,3,1,1),
                       c(4,4,4,1),
                       c(9,9,9,1),
                       c(6,6,6,1),
                       c(10,10,10,1),
                       c(5,5,5,5))

#                                      1       2(1)     3(2)       4(3)  5(4)      6(3)         7        8(1)    9(3) 10(3)
whole_plot=gridExtra::grid.arrange(empty_space,naive, linear_temp, gdd, uniforc, alternating, legend, gdd_fixed, m1, msb, layout_matrix=complex_layout,
                        left = 'LTER Derived Parameter Estimates',
                        bottom = 'NPN Derived Parameter Estimates')



ggsave(paste0(config$image_save_directory,'figure_param_comparison.png'), plot=whole_plot, height=33, width=20, units = 'cm')


