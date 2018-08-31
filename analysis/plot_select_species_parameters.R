library(tidyverse)
library(cowplot)
config = yaml::yaml.load_file('config.yaml')

###################################################
# This produced the supplementary figure comparing parameters
# of 4 species. Adjust the ones below to create different plots. 
#################################################

select_species = c('acer circinatum','populus tremuloides','acer rubrum','vaccinium parvifolium')
select_phenophase = c(371)
#models_of_interest = c('gdd_fixed','gdd','alternating')

all_parameters = read_csv(config$model_parameter_file) %>%
  filter(!parameter_name %in% c('run_time','num_iterations')) 

#Pull out phenophase
all_parameters = all_parameters %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

all_parameters$phenophase = as.numeric(all_parameters$phenophase)

#Make the threshold temperature name a bit more descriptive
all_parameters$parameter_name[all_parameters$parameter_name=='T'] = 'T_base'

select_species_params = all_parameters %>%
  filter(species %in% select_species, 
         phenophase %in% select_phenophase)

########################################################################
datasets = c('harvard','hubbard','npn','hjandrews','jornada')
pretty_dataset_names = c('Harvard Forest','Hubbard Brook','NPN','H.J. Andrews','Jornada')
select_species_params$dataset = factor(select_species_params$dataset, levels = datasets, labels = pretty_dataset_names)
############################################################################

common_plot_theme = theme(strip.text = element_text(size=10),
                          strip.background = element_rect(fill='grey95'),
                          axis.text = element_text(size=12),
                          axis.title.y = element_text(size=18))

point_size=4
point_shapes = c(17,13)
color_pallete=c("grey42", "#56B4E9", "#009E73")


single_model_plot = function(model_name, plot_title, num_column){
  plot_data = select_species_params %>%
    filter(model==model_name)
  summary_lines = plot_data %>%
    group_by(dataset, parameter_name, species) %>%
    summarise(mean_value = mean(value), median_value=median(value)) %>%
    ungroup() %>%
    gather(summary_metric, summary_value, mean_value, median_value)
  p=ggplot(plot_data, aes(x=value, fill=dataset)) +
    geom_density(position = position_identity(), alpha=0.7) +
    geom_vline(data=summary_lines, aes(xintercept=summary_value, color=dataset, linetype=summary_metric), size=1) +
    #scale_fill_manual(values=color_pallete) +
    #scale_color_manual(values=color_pallete) +
    scale_fill_brewer(palette = 'Dark2') +
    scale_color_brewer(palette = 'Dark2') +
    facet_wrap(species~parameter_name, scales='free', ncol=num_column) +
    theme(legend.key.size = unit(3, units='lines'),
          legend.key = element_rect(size=5))+
    labs(fill='', color='',linetype='', x='Parameter Distribution', y = plot_title)
  return(p)
}
library(patchwork)
alternating = single_model_plot('alternating', 'Alternating', num_column = 3)
gdd = single_model_plot('gdd', 'GDD', num_column = 3)
gdd_fixed = single_model_plot('gdd_fixed', 'Fixed GDD', num_column = 2)
naive = single_model_plot('naive','Naive', num_column = 2)
uniforc = single_model_plot('uniforc','Uniforc', num_column = 4)
linear = single_model_plot('linear_temp','Linear', num_column = 2)

plot(alternating)
plot(gdd)
plot(gdd_fixed)
plot(naive)
plot(uniforc)
plot(linear)

no_legend = theme(legend.position = 'none')

select_species_1 = 
  (naive + no_legend)+
  (gdd_fixed + no_legend) + 
  (linear) + 
  (gdd + no_legend) + 
  plot_layout(ncol=1, heights=c(1,1,2,3))

select_species_2 = 
  (alternating) + 
  (uniforc + no_legend) +
  plot_layout(ncol=1, heights=c(1,1))

ggsave('manuscript/supplement_select_species_param_comparison1.png', plot=select_species_1, height=40, width=30, units = 'cm')
ggsave('manuscript/supplement_select_species_param_comparison2.png', plot=select_species_2, height=40, width=30, units = 'cm')
