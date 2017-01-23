library(tidyverse)
library(broom)


harvard_observations = read_csv('./cleaned_data/harvard_observations.csv')
npn_observations = read_csv('./cleaned_data/NPN_observations.csv')



################################################################
get_param_estimates = function(df){
  #Do a lm estimate for the starting values before fitting the nls()
  #http://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
  
  a.0 = min(df$GDD)*0.5
  
  m.0 = lm(log(GDD-a.0) ~ NCD, data=df)
  start_list = list(b=exp(coef(m.0)[1]), c=coef(m.0)[2], a=a.0)
  m = nls(GDD ~ a + b*exp(c*NCD), data=df, start=start_list, control = nls.control(maxiter = 500))
  return(broom::tidy(m))
}
#############################################################


parameter_esimates=data.frame()
for(this_species in unique(harvard_observations$species)){
  this_species_data = harvard_observations %>%
    filter(species==this_species)
  
  params = try(get_param_estimates(this_species_data))
  if(class(params)=='try-error'){
    print(paste0('nls error: ',this_species))
  } else {
    params$species=this_species
    params$data_source='harvard'
    parameter_esimates = parameter_esimates %>%
      bind_rows(params)
  }
}

for(this_species in unique(npn_observations$species)){
  this_species_data = npn_observations %>%
    filter(species==this_species) 
    
  params = try(get_param_estimates(this_species_data))
  if(class(params)=='try-error'){
    print(paste0('nls error: ',this_species))
  } else {
    params$species=this_species
    params$data_source='npn'
    parameter_esimates = parameter_esimates %>%
      bind_rows(params)
  }
}




