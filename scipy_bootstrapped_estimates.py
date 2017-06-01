import pandas as pd
import numpy as np
from scipy import optimize
from mpi4py import MPI
import yaml
from models import *
import time


class data_store:
    def __init__(self, all_dataset_configs, num_bootstrap, models=['uniforc','gdd']):
        self.num_bootstrap=num_bootstrap
        self.all_dataset_configs=all_dataset_configs
        self.models=models
        self.calculate_total_jobs()
        self.load_next_dataset()


    #total jobs = sum(species in each dataset) * number of bootstraps * number of models
    #for progress information only
    def calculate_total_jobs(self):
        total_species=0
        for dataset_config in self.all_dataset_configs:
            observation_data = pd.read_csv(dataset_config['observations_data_file'])
            total_species+=observation_data.species.unique().shape[0]
        self.total_jobs = total_species * self.num_bootstrap * len(self.models)
        self.num_jobs_left = self.total_jobs

    def load_next_dataset(self):
        dataset_config = self.all_dataset_configs.pop()

        self.observation_data = pd.read_csv(dataset_config['observations_data_file'])
        self.temp_data = pd.read_csv(dataset_config['temp_data_file'])
        self.dataset_name=dataset_config['dataset_name']

        #The model framework differentiates by site to accomidate the NPN dataset.
        #For datasets that are a single site, add in a column for it
        if dataset_config['add_site_dummy_var']:
            self.observation_data['Site_ID']=1
            self.temp_data['Site_ID']=1

        self.current_dataset_job_list=[]
        for model in self.models:
            for species in self.observation_data.species.unique():
                for bootstrap_i in range(self.num_bootstrap):
                    self.current_dataset_job_list.append({'species' : species,
                                                          'bootstrap_i' : bootstrap_i,
                                                          'model': model})


    #Return a model to optimize and info about the model as a single tuple
    #ready to be send over MPI
    def get_next_job(self):
        if not self.jobs_available_in_current_dataset():
            print('no jobs left in dataset')
            return None

        job_info=self.current_dataset_job_list.pop()

        #A bootstrapped sample with replacment of this species observations
        data_sample = self.observation_data[self.observation_data.species==job_info['species']].sample(frac=1, replace=True).copy()
        #Temperature data at sites where this species occures
        sp_temp_data = self.temp_data[self.temp_data.Site_ID.isin(data_sample.Site_ID.unique())].copy()

        model=phenology_model(temp_data=sp_temp_data, plant_data=data_sample, model_name=job_info['model'])
        model_bounds = model.get_scipy_parameter_bounds()

        #all model components to be recieved the by the work process
        package =  (model, model_bounds, job_info['species'], job_info['bootstrap_i'], self.dataset_name)

        if (not self.jobs_available_in_current_dataset()) and self.datasets_available():
            self.load_next_dataset()

        self.num_jobs_left-=1

        return package

    def datasets_available(self):
        return len(self.all_dataset_configs)>0

    def jobs_available_in_current_dataset(self):
        return len(self.current_dataset_job_list)>0

    def jobs_available(self):
        return self.datasets_available() or self.jobs_available_in_current_dataset()

#Each job outputs parameter values which may be different depending on model.
#This puts everything into a common format so a data.frame can be made like:
#model, parameter_name, paramter_value, species, dataset, bootstrap_num
def cleanup_results(all_job_results):
    cleaned_results=[]
    for job_result in all_job_results:
        dataset = job_result.pop('dataset')
        species = job_result.pop('species')
        bootstrap_num = job_result.pop('bootstrap_num')
        model = job_result.pop('model')

        #Only parameter values are left in the dictionary
        for parameter_name, parameter_value in job_result.items():
            cleaned_results.append({'parameter_name':parameter_name, 'value':parameter_value,
                                   'dataset':dataset, 'species':species,
                                   'bootstrap_num':bootstrap_num, 'model':model})
    return cleaned_results

#######################################################

########################################################


def master():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    num_workers = MPI.COMM_WORLD.Get_size()

    work_tag=0
    stop_tag=1
    ###################################
    with open('config.yaml', 'r') as f:
        config = yaml.load(f)

    results_file = config['model_parameter_file']

    job_queue = data_store(all_dataset_configs = config['dataset_configs'],
                           num_bootstrap =       config['num_bootstrap'],
                           models =              config['models_to_use'])

    #Dole out the first round of jobs to all workers
    for i in range(1, num_workers):
        if job_queue.jobs_available():
            next_job = job_queue.get_next_job()
        else:
            break
        comm.send(obj=next_job, dest=i, tag=work_tag)

    #While there are new jobs to assign.
    #Collect results and assign new jobs as others are finished.
    results=[]
    while job_queue.jobs_available():
        next_job = job_queue.get_next_job()
        job_result = comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
        results.append(job_result)
        num_jobs_completed = len(results)
        print('Completed job '+str(num_jobs_completed)+' of '+str(job_queue.total_jobs))
        print('Bootstrap: '+str(job_result['bootstrap_num']) + ' - ' +
              str(job_result['run_time']) + ' hrs - ' +
              job_result['dataset'] + ' - ' +
              job_result['model']   + ' - ' + 
              job_result['species'])

        comm.send(obj=next_job, dest=status.Get_source(), tag=work_tag)

    #Collect last jobs
    for i in range(1, num_workers):
        job_result = comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG)
        results.append(job_result)
    #Shut down all workers
    for i in range(1, num_workers):
        comm.send(obj=None, dest=i, tag=stop_tag)

    results=pd.DataFrame(cleanup_results(results))
    results.to_csv(results_file, index=False)

def worker():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    while True:
        model_package = comm.recv(source=0, tag=MPI.ANY_TAG, status=status)
        if status.Get_tag() == 1: break

        model, bounds, species, bootstrap_i, dataset = model_package
        start_time=time.time()
        optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=False, maxiter=None, popsize=100, mutation=1.5, recombination=0.25)
        #quicker testing optimizer
        #optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=False, maxiter=5, popsize=10)
        #Save the optimizer run time in hours
        total_time=round((time.time() - start_time)/60/60, 2)

        return_data = model.translate_scipy_parameter_output(optimize_output['x'])
        #return_data['final_score']=optimize_output['fun']
        return_data['species']      = species
        return_data['bootstrap_num']= bootstrap_i
        return_data['dataset']      = dataset
        return_data['model']        = model.model_name
        return_data['run_time']     = total_time

        comm.send(obj=return_data, dest=0)


if __name__ == "__main__":
    rank = MPI.COMM_WORLD.Get_rank()
    name = MPI.Get_processor_name()
    size = MPI.COMM_WORLD.Get_size()

    if rank == 0:
        print('master '+str(rank)+' on '+str(name))
        master()
    else:
        print('worker '+str(rank)+' on '+str(name))
        worker()


#temp_data = pd.read_csv('./cleaned_data/npn_temp.csv')
#obs_data = pd.read_csv('./cleaned_data/NPN_observations.csv')
#sp_data = obs_data[obs_data.species=='acer rubrum'].copy()
#species_temp_data=temp_data[temp_data.Site_ID.isin(sp_data.Site_ID.unique())].copy()
