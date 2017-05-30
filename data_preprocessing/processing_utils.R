

#################################################################
#Takes a data.frame in the format colnames(species, Site_ID, year, status, doy)
#where status is 1 or 0, and observations include *all* the observations from the dataset.
#
#Subsets this observations that were preceeded by at least one observation of status==0,
#and sets the doy as the midpoint between the first observed status==1 and the most recent
#observation.
process_phenology_observations = function(df){
  #Add an observation ID for each unique series
  df = df %>%
    arrange(doy) %>%
    group_by(species, Site_ID, year, individual_id) %>%
    mutate(obs_num = 1:n()) %>%
    ungroup()
  
  #site,year,species where a budbreak==0 was the first observation in a year
  phenophase_0 = df %>%
    group_by(species, Site_ID, year, individual_id) %>%
    top_n(1, -doy) %>%
    ungroup() %>%
    filter(status==0) %>%
    select(species, Site_ID, year, individual_id) %>%
    mutate(keep='yes')
  
  #Keep only observations that were preceded by an observation of the phenophase==0
  df_subset = df %>%
    filter(status==1) %>%
    group_by(species,Site_ID,year, individual_id) %>%
    top_n(1,-doy) %>%
    ungroup() %>%
    left_join(phenophase_0, by=c('species','Site_ID','year','individual_id')) %>%
    filter(keep=='yes') %>% 
    select(-status, -keep)
  
  #Get the doy for the most recent observation
  prior_observations = df_subset %>%
    mutate(obs_num = obs_num-1) %>%
    select(-doy) %>%
    left_join(df, by=c('species','Site_ID','year','obs_num', 'individual_id')) %>%
    select(species, Site_ID, year, individual_id, doy_prior = doy)
  
  #Infer the doy of the phelogical event as the midpoint between the doy it was first
  #observed and the most prior observation
  df_subset = df_subset %>%
    left_join(prior_observations, by=c('species','Site_ID','year','individual_id')) %>%
    mutate(doy_difference = doy-doy_prior)
  
  #Sanity check. No negative numbers, which would happen if doy_prior was larger than doy
  if(any(df_subset$doy_difference < 0, na.rm=T)){stop('doy_prior larger than doy')}
  
  #Final calc and select columns used in the python modeling code
  df_subset = df_subset %>%
    filter(!is.na(doy_difference)) %>%
    mutate(doy = round(doy_difference/2 + doy_prior)) %>%
    select(species, Site_ID,year,doy)
  
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