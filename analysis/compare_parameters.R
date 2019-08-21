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
###############################################################################
# Everything below if for making the fairly complicated Figure 2. 

# Once this image is rendered I do some slight editing in a photo editor to mask out all the 
# dummy variables and adjust the text and legend positioning. 
###############################################################################
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
  left_join(parameter_name_plotmath, by = c('model','parameter_name')) 

#################################################################################
# These dummy lines help create padding on the right side of Figure 2.
dummy_parameters = tribble(
~parameter_name,~facet_strip_text, ~model, ~lts_derived_parameter, ~npn_derived_parameter, ~r2_text,
'naive_dummy1','naive_dummy1', 'naive', 1,1,'naive_dummy1',
'naive_dummy2','naive_dummy2', 'naive', 1,1,'naive_dummy2',
'naive_dummy3','naive_dummy3', 'naive', 1,1,'naive_dummy3',

'fixed_gdd_dummy1','fixed_gdd_dummy1', 'gdd_fixed', 1,1,'fixed_gdd_dummy1',
'fixed_gdd_dummy2','fixed_gdd_dummy2', 'gdd_fixed', 1,1,'fixed_gdd_dummy2',
'fixed_gdd_dummy3','fixed_gdd_dummy3', 'gdd_fixed', 1,1,'fixed_gdd_dummy3',

'linear_dummy1','fixed_gdd_dummy1', 'linear_temp', 1,1,'linear_dummy1',
'linear_dummy2','fixed_gdd_dummy2', 'linear_temp', 1,1,'linear_dummy2',

'gdd_dummy1','gdd_dummy1', 'gdd', 1,1,'gdd_dummy1',

'm1_dummy1','m1_dummy1', 'm1', 1,1,'m1_dummy1',

'alternating_dummy1','alternating_dummy1', 'alternating', 1,1,'alternating_dummy1',

'msb_dummy1','msb_dummy1', 'msb', 1,1,'msb_dummy1'
)

# This ordering sets up the 32 subplots (20 model variables + 12 dummy variables for padding)
# in the correct grid. 
subplot_order = tribble(
  ~model, ~parameter_name, ~plot_order_number,
  'naive','mean_doy',1,
  'naive','naive_dummy1',2,
  'naive','naive_dummy2',3,
  'naive','naive_dummy3',4,
  
  'gdd_fixed','F',5,
  'gdd_fixed','fixed_gdd_dummy1',6,
  'gdd_fixed','fixed_gdd_dummy2',7,
  'gdd_fixed','fixed_gdd_dummy3',8,
  
  'linear_temp','intercept',9,
  'linear_temp','slope',10,
  'linear_temp','linear_dummy1',11,
  'linear_temp','linear_dummy2',12,
  
  'gdd','F',13,
  'gdd','t1',14,
  'gdd','T',15,
  'gdd','gdd_dummy1',16,
  
  'm1','F',17,
  'm1','t1',18,
  'm1','T',19,
  'm1','m1_dummy1',20,
  
  'alternating','a',21,
  'alternating','b',22,
  'alternating','c',23,
  'alternating','alternating_dummy1',24,
  
  'msb','a',25,
  'msb','b',26,
  'msb','c',27,
  'msb','msb_dummy1',28,
  
  'uniforc','b',29,
  'uniforc','c',30,
  'uniforc','F',31,
  'uniforc','t1',32
)

#################################################
#################################################
# Put it all together

parameters_with_dummy_vars = parameter_means %>%
  bind_rows(dummy_parameters) %>%
  left_join(subplot_order, by=c('model','parameter_name'))

parameters_with_dummy_vars$model = factor(parameters_with_dummy_vars$model, levels =  c("naive","gdd_fixed","linear_temp","gdd","m1","alternating","msb","uniforc"),
                          labels = c("naive","gdd_fixed","linear_temp","gdd","m1","alternating","msb","uniforc"),
                          ordered = TRUE)

parameters_with_dummy_vars$facet_strip_text = with(parameters_with_dummy_vars, paste0('list(',parameter_symbol,',', r2_text,')'))

# in the main ggplot call below, facet_wrap will use facet_strip_text to organize the different subplots.
# the order within the facet_strip_text factor (defined in this next line by the plot_order_number) will dictate how they are drawn
# starting at the top-left and going left-right,top-bottom.
parameters_with_dummy_vars$facet_strip_text = forcats::fct_reorder(parameters_with_dummy_vars$facet_strip_text, parameters_with_dummy_vars$plot_order_number)

y_axis_text = c('LTER Derived Parameter Estimates\n
                      Uniforc            MSB           Alternating              M1                  GDD               Linear            Fixed GDD           Naive')

whole_plot = 5
parameters_with_dummy_vars %>%
filter(model == 'gdd') %>%
  filter(parameter_name != 'gdd_dummy1') %>%
  ggplot(aes(x=npn_derived_parameter, y=lts_derived_parameter, color=dataset, group=dataset)) + 
  geom_point(size=8) +
  scale_shape_manual(values=c(17,13)) +
  scale_color_manual(values=c("grey42", "#E69F00", "#56B4E9", "#CC79A7")) +
  geom_abline(intercept=0, slope=1) +
  theme_bw(30) +
  facet_wrap(~parameter_name, scales='free') + 
  labs(x='USA-NPN GDDs',y='LTER GDDs',
       color = "LTER Dataset", shape = "Phenophase") 
  
  
    theme(strip.text = element_text(size=10),
        strip.background = element_rect(fill='grey95'),
        strip.switch.pad.wrap = unit(2, 'cm'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16)) 

ggsave(paste0(config$image_save_directory,'figure_2_param_comparison.png'), plot=whole_plot, height=28, width=25, units = 'cm', dpi=1000)

# Note, after rendering I deleted the dummy placeholder subplots inside a photo editing program and save as figure_param_comparison_final.png

