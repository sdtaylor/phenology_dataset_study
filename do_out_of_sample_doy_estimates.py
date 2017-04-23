import pandas as pd
import numpy as np
from models import *
import yaml


#Get the doy estimates for budburst of the NPN dataset using paramters from harvard and vice versa

with open('config.yaml', 'r') as f:
    config = yaml.load(f)

all_model_parameters=pd.read_csv('model_parameters.csv')

results=[]

for dataset in config['dataset_configs']:
    observation_data = pd.read_csv(dataset['observations_data_file'])
    temperature_data = pd.read_csv(dataset['temp_data_file'])
    dataset_obs_name = dataset['dataset_name']

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
                parameter_values = all_model_parameters.query('model_name == @model_name & dataset == @parameter_source & species    == @species')

                #Datasets do not have all species in common
                if parameter_values.empty:
                    continue

                #Some dupes of bootsrap_num=0 here. Possibly related to the below issue.
                #TODO: figure that out
                parameter_values = parameter_values[parameter_values.bootstrap_num>0]

                #Each line will be parameters from a single bootsrapped run
                parameter_values = parameter_values.pivot(index='bootstrap_num', columns='parameter', values='value').reset_index()

                #sanity check, bootstrap numbers are sometimes low. possibly due to the optimizer erroring out?
                #TODO: figure that out
                print(parameter_values.shape)
                print(parameter_values.head())
                assert parameter_values.shape[0] >= (config['num_bootstrap']-10), 'number of bootstraps too low'

                for bootstrap_iteration in parameter_values.to_dict('records'):
                    bootstrap_num=bootstrap_iteration.pop('bootstrap_num')
                    doy_estimated = model_estimator.get_all_estimates(**bootstrap_iteration)
                    doy_observed  = model_estimator.plant_doy
                    year_observed = model_estimator.plant_year
                    site_observed = model_estimator.plant_site

                    print(dataset['dataset_name']+', '+species+', '+parameter_source+', '+str(bootstrap_num))
                    for i in range(doy_observed.shape[0]):
                        results.append({'bootstrap_num':bootstrap_num,
                                        'species'      :species,
                                        'model_name':   model_name,
                                        'parameter_source':parameter_source,
                                        'observation_source': dataset_obs_name,
                                        'doy_observed':       doy_observed[i],
                                        'year_observed':       year_observed[i],
                                        'site_observed':       site_observed[i],
                                        'doy_estimated':      doy_estimated[i]})


results = pd.DataFrame(results)
results.to_csv('out_of_sample_doy_estimates.csv', index=False)
