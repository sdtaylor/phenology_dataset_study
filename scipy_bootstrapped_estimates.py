import pandas as pd
import numpy as np
from scipy import optimize
from mpi4py import MPI
from models import *


class data_store:
    def __init__(self, all_dataset_configs, num_bootstrap):
        self.num_bootstrap=num_bootstrap
        self.all_dataset_configs=all_dataset_configs
        self.load_next_dataset()

    def load_next_dataset(self):
        dataset_config = self.all_dataset_configs.pop()

        self.observation_data = pd.read_csv(dataset_config['observations_data_file'])
        self.temp_data = pd.read_csv(dataset_config['temp_data_file'])
        self.dataset_name=dataset_config['dataset_name']
        self.include_t1_parameter=dataset_config['include_t1_parameter']

        #The model framework differentiates by site to accomidate the NPN dataset.
        #For datasets that are a single site, add in a column for it
        if dataset_config['add_site_dummy_var']:
            self.observation_data['Site_ID']=1
            self.temp_data['Site_ID']=1

        #Lower and upper bounds of model parameters. Also used by differential_evolution()
        #for determining the  number of parameters
        if self.include_t1_parameter:
            #             t1         b         c       F*     t1_slope
            self.bounds = [(-126,180), (-20,0), (-50,50), (0,100), (-20,20)]
        else:
            #             t1         b         c       F*
            self.bounds = [(-126,180), (-20,0), (-50,50), (0,100)]

        self.job_list=[]
        for species in self.observation_data.species.unique():
            for bootstrap_i in range(self.num_bootstrap):
                self.job_list.append({'species':species, 'bootstrap_i':bootstrap_i})


    def get_next_dataset(self):
        pass

    #Return a model to optimize and info about the model as a single tuple
    #ready to be send over MPI
    def get_next_job(self):
        if not self.jobs_available_in_current_dataset():
            print('no jobs left in dataset')
            return None

        job_info=self.job_list.pop()
        if not self.jobs_available_in_current_dataset() and self.datasets_available():
            self.load_next_dataset()

        #A bootsrapped sample with replacment of this species observations
        data_sample = self.observation_data[self.observation_data.species==job_info['species']].sample(frac=1, replace=True).copy()
        #Temperature data at sites where this species occures
        sp_temp_data = self.temp_data[self.temp_data.Site_ID.isin(data_sample.Site_ID.unique())].copy()

        model=uniforc_model(temp_data=sp_temp_data, plant_data=data_sample, t1_varies=self.include_t1_parameter)
        return (model, self.bounds, job_info['species'], job_info['bootstrap_i'], self.dataset_name)

    def datasets_available(self):
        return len(self.all_dataset_configs)>0

    def jobs_available_in_current_dataset(self):
        return len(self.job_list)>0

    def jobs_available(self):
        return self.datasets_available() or self.jobs_available_in_current_dataset()

    def jobs_left(self):
        return len(self.job_list)

#######################################################

########################################################


def master():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    num_workers = MPI.COMM_WORLD.Get_size()

    work_tag=0
    stop_tag=1
    ###################################
    configs=[]

    configs.append({})
    configs[0]['dataset_name']='npn'
    configs[0]['observations_data_file'] = './cleaned_data/NPN_observations.csv'
    configs[0]['temp_data_file'] = './cleaned_data/npn_temp.csv'
    configs[0]['include_t1_parameter']=False
    configs[0]['add_site_dummy_var']=False

    configs.append({})
    configs[1]['dataset_name']='harvard'
    configs[1]['observations_data_file'] = './cleaned_data/harvard_observations.csv'
    configs[1]['temp_data_file'] = './cleaned_data/harvard_temp.csv'
    configs[1]['include_t1_parameter']=False
    configs[1]['add_site_dummy_var']=True

    results_file='all_results.csv'

    job_queue = data_store(all_dataset_configs=configs, num_bootstrap=500)

    total_jobs=job_queue.jobs_left()
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
        comm.send(obj=next_job, dest=status.Get_source(), tag=work_tag)
        num_jobs_left=job_queue.jobs_left()
        print('Completed job '+str(total_jobs-num_jobs_left)+' of '+str(total_jobs))

    #Collect last jobs
    for i in range(1, num_workers):
        job_result = comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG)
        results.append(job_result)
    #Shut down all workers
    for i in range(1, num_workers):
        comm.send(obj=None, dest=i, tag=stop_tag)

    results=pd.DataFrame(results)
    results.to_csv(results_file, index=False)

def worker():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    while True:
        model_package = comm.recv(source=0, tag=MPI.ANY_TAG, status=status)
        if status.Get_tag() == 1: break

        model, bounds, species, bootstrap_i, dataset = model_package
        optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=False, maxiter=None)
        x=optimize_output['x']
        #Get estimates of optimzed model

        #With 5 paramters the t1 paramter is being estimated
        if x.shape[0]==5:
            t1_int, b, c, F, t1_slope = x[0], x[1], x[2], x[3], x[4]
            return_data={'t1_int':t1_int, 'b':b, 'c':c, 'F':F, 't1_slope':t1_slope,
                         'species':species, 'boostrap_num':bootstrap_i, 'dataset':dataset}
        else:
            t1_int, b, c, F = x[0], x[1], x[2], x[3]
            return_data={'t1_int':t1_int, 'b':b, 'c':c, 'F':F,
                         'species':species, 'boostrap_num':bootstrap_i, 'dataset':dataset}

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
