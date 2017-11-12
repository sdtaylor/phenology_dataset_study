import pandas as pd
import numpy as np
from models import *
import yaml


#Get the doy estimates for budburst of the NPN dataset using paramters from harvard and vice versa

with open('config.yaml', 'r') as f:
    config = yaml.load(f)

all_model_parameters=pd.read_csv(config['model_parameter_file'])

results=[]

for dataset in config['dataset_configs']:
    observation_data = pd.read_csv(dataset['observations_data_file'])
    temperature_data = pd.read_csv(dataset['temp_data_file'])
    dataset_obs_name = dataset['dataset_name']

    #Combine phenophase + species since each has their own models. 
    observation_data.species = observation_data.species.map(str) + ' - ' + observation_data.Phenophase_ID.map(str)

    #The model framework differentiates by site to accomidate the NPN dataset.
    #For datasets that are a single site, add in a column for it
    if dataset['add_site_dummy_var']:
        observation_data['Site_ID']=1
        temperature_data['Site_ID']=1

    for species in observation_data.species.unique():
    #for species in ['acer rubrum','populus tremuloides', 'betula papyrifera']:
        sp_observations = observation_data[observation_data.species==species].copy()
        #Temperature data only at sites where this species occures
        sp_temp_data = temperature_data[temperature_data.Site_ID.isin(sp_observations.Site_ID.unique())].copy()

        for model_name in config['models_to_use']:
            model_estimator = phenology_model(temp_data=sp_temp_data, plant_data=sp_observations, model_name=model_name)

            for parameter_source in all_model_parameters.dataset.unique():
                #Parameters for a specific model, species, and dataset source
                parameter_values = all_model_parameters.query('model == @model_name & dataset == @parameter_source & species == @species')

                #Datasets do not have all species in common
                if parameter_values.empty:
                    continue

                #Spread the parameters out to columns. Number of columns is different for each model
                parameter_values = parameter_values.pivot_table(index=['bootstrap_num'], columns='parameter_name', values='value').reset_index()

                #sanity check, bootstrap numbers should be exact
                assert parameter_values.shape[0] == (config['num_bootstrap']), 'Bootstrap number incorrect in predictions'

                for bootstrap_iteration in parameter_values.to_dict('records'):
                    bootstrap_num=bootstrap_iteration.pop('bootstrap_num')
                    doy_estimated = model_estimator.get_doy_estimates(**bootstrap_iteration)
                    doy_observed  = model_estimator.doy_observations
                    year_observed = model_estimator.year_observations
                    site_observed = model_estimator.site_observations
                    doy_data_type = model_estimator.doy_data_type

                    print(dataset['dataset_name']+', '+species+', '+parameter_source+', '+str(bootstrap_num))
                    for i in range(doy_observed.shape[0]):
                        results.append({'bootstrap_num':bootstrap_num,
                                        'species'      :species,
                                        'model_name':   model_name,
                                        'parameter_source':parameter_source,
                                        'observation_source': dataset_obs_name,
                                        'doy_observed':       doy_observed[i],
                                        'observation_id':     i,
                                        'year_observed':       year_observed[i],
                                        'site_observed':       site_observed[i],
                                        'doy_estimated':      doy_estimated[i],
                                        'data_type':          doy_data_type[i]})

# list of dictionaries to pandas df
results = pd.DataFrame(results)

results.to_csv(config['predictions_file_large'], index=False)

# Summarize the 250 bootstrapped models to a single prediction

grouping_columns = ['data_type', 'model_name', 'observation_id', \
                 'observation_source','parameter_source', \
                 'site_observed', 'species', 'year_observed']

# Sanity check again for the number of bootstraps being correct
sanity_check = results.groupby(grouping_columns)['doy_estimated'].count().reset_index()
assert np.all(sanity_check.doy_estimated.values == config['num_bootstrap']), 'Bootstrap number incorrect in summarization'

# Drop predictions where the event was not actually predicted to occure
# see https://github.com/sdtaylor/phenology_dataset_study/issues/27
results = results[results.doy_estimated < 1000]

results_combined = results.groupby(grouping_columns)['doy_estimated','doy_observed'].mean().reset_index()

results_combined.to_csv(config['predictions_file'], index=False)
