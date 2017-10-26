

#################################################################
#Takes a data.frame in the format colnames(species, Site_ID, year, status, doy)
#where status is 1 or 0, and observations include *all* the observations from the dataset.
#
#Subsets this observations that were preceeded by at least one observation of status==0,
#and sets the doy as the midpoint between the first observed status==1 and the most recent
#observation.
# prior_obs_cutoff: Only use observations where the prior status=0 observation is < this amount
#                 if equal to -1 (the default) then don't enforce this. Only used in NPN data
process_phenology_observations = function(df, prior_obs_cutoff=-1){
  #Add an observation ID for each unique series
  df = df %>%
    arrange(doy) %>%
    group_by(species, Site_ID, year, individual_id, Phenophase_ID) %>%
    mutate(obs_num = 1:n()) %>%
    ungroup()
  
  #site,year,species where a status==0 was the first observation in a year
  phenophase_0 = df %>%
    group_by(species, Site_ID, year, individual_id, Phenophase_ID) %>%
    top_n(1, -doy) %>%
    ungroup() %>%
    filter(status==0) %>%
    select(species, Site_ID, year, individual_id, Phenophase_ID) %>%
    mutate(has_prior_obs='yes')
  
  #Keep only observations of status==1 that were preceded by an observation of the status==0
  df_subset = df %>%
    filter(status==1) %>%
    group_by(species, Site_ID, year, individual_id, Phenophase_ID) %>%
    top_n(1,-doy) %>%
    ungroup() %>%
    left_join(phenophase_0, by=c('species','Site_ID','year','individual_id','Phenophase_ID')) %>%
    filter(has_prior_obs=='yes') %>% 
    select(-status, -has_prior_obs)
  
  #Get the doy for the most recent observation, which should be status==0
  prior_observations = df_subset %>%
    mutate(obs_num = obs_num-1) %>%
    select(-doy) %>%
    left_join(df, by=c('species','Site_ID','year','obs_num', 'individual_id', 'Phenophase_ID')) %>%
    select(species, Site_ID, year, individual_id, doy_prior = doy, Phenophase_ID)
  
  df_subset = df_subset %>%
    left_join(prior_observations, by=c('species','Site_ID','year','individual_id','Phenophase_ID')) %>%
    mutate(doy_difference = doy-doy_prior)
  
  #Sanity check. No negative numbers, which would happen if doy_prior was larger than doy
  if(any(df_subset$doy_difference < 0, na.rm=T)){stop('doy_prior larger than doy')}
  
  if(prior_obs_cutoff>0){
    df_subset = df_subset %>%
      filter(doy_difference < prior_obs_cutoff)
  }
  
  #Final calc and select columns used in the python modeling code
  df_subset = df_subset %>%
    filter(!is.na(doy_difference)) %>%
    mutate(doy = round(doy_difference/2 + doy_prior)) %>%
    select(species, Site_ID,year,doy, Phenophase_ID)
  
  return(df_subset)
}


##########################################################
#Keep a list of all the unique species and their phenophases for
#each dataset. This will be used to subset the (very large) NPN
#dataset to only species being compared

append_species_file = function(species_df){
  non_npn_species_file = './cleaned_data/non_npn_species_list.csv'
  if(file.exists(non_npn_species_file)){
    read_csv(non_npn_species_file) %>%
      bind_rows(species_df) %>%
      distinct() %>%
      arrange() %>%
      write_csv(non_npn_species_file)
  } else {
    write_csv(species_df, non_npn_species_file)
  }
}