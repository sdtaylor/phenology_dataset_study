import pandas as pd
import numpy as np
from scipy import optimize
from mpi4py import MPI

########################################################################
#Evaluate a UniForc model of a single species
#uniforc model evaluator designed to be used in any optimize function.
#accepts a set of temperature data and observations of phenological event
#as the doy. Returns the RMSE error of the doy given the  4 UniForc parameters
class uniforc_model:
    def __init__(self, temp_data, plant_data, t1_varies=False):
        #Create a ['Site_ID','year'] x 'doy' matrix with daily temp as the value
        #This is done to calculate the GDD info for all sites/years at once
        temp_data = temp_data.pivot_table(index=['Site_ID','year'], columns='doy', values='temp').reset_index()

        #doy are the columns here from -127 to 180
        self.temp_temp =temp_data.drop(['Site_ID','year'], axis=1).values

        #Indexes to find the temp infor for each site and year
        self.temp_year=temp_data['year'].values
        self.temp_site=temp_data['Site_ID'].values
        self.temp_doy =temp_data.drop(['Site_ID','year'], axis=1).columns

        #Plant data
        self.plant_site=plant_data['Site_ID'].values
        self.plant_year=plant_data['year'].values
        self.plant_doy =plant_data['doy'].values
        self.num_replicates=plant_data.shape[0]

        self.t1_varies=t1_varies

    #Get a site/year x doy boolean array of the days meeting the F* requirement
    def calculate_doy_estimates(self, t1, b, c, F, t1_slope=False):
        all_site_temps = self.temp_temp.copy()

        all_site_temps = 1 / (1 + np.exp(b*(all_site_temps-c)))

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=np.zeros_like(all_site_temps)
        for doy in range(all_site_daily_gdd.shape[1]):
            all_site_daily_gdd[:,doy]=all_site_temps[:,0:doy+1].sum(1)

        #The predicted doy for each site/year. If none was predicted give a doy which
        #will return a very large error
        doy_estimates = np.zeros(all_site_daily_gdd.shape[0])
        for site_year in range(all_site_daily_gdd.shape[0]):
            this_estimate=self.temp_doy[all_site_daily_gdd[site_year]>=F]
            doy_estimates[site_year]=this_estimate[0] if this_estimate.shape[0]>0 else 1000

        return doy_estimates

    #RMSE of the estimated budburst doy of all sites
    def get_error(self, **kargs):
        doy_estimates=self.calculate_doy_estimates(**kargs)
        errors = []
        for row in range(self.num_replicates):
            estimated_doy = doy_estimates[(self.temp_site == self.plant_site[row]) & (self.temp_year == self.plant_year[row])]
            if len(estimated_doy)>1: print('>1 estimated doy')
            #print(estimated_doy)
            errors.append(estimated_doy[0] - self.plant_doy[row])

        errors = np.array(errors)
        return np.sqrt(np.mean(errors**2))

    #The estimated doy given a set of parameters. 
    def get_all_estimates(self, **kargs):
        pass

    #scipy optimize functions want a array of parameter values
    #use this to unpack it
    def scipy_error(self,x):
        if self.t1_varies:
            kargs={'t1':x[0], 'b':x[1], 'c':x[2], 'F':x[3],'t1_slope':x[4]}
        else:
            kargs={'t1':x[0], 'b':x[1], 'c':x[2], 'F':x[3]}
        return self.get_error(**kargs)


class data_store:
    def __init__(self, dataset_config, num_bootstrap, add_site_dummy_var=False):
        self.num_bootstrap=num_bootstrap

        self.observation_data = pd.read_csv(dataset_config['observations_data_file'])
        self.temp_data = pd.read_csv(dataset_config['temp_data_file'])
        self.dataset_name=dataset_config['dataset_name']
        self.include_t1_parameter=dataset_config['include_t1_parameter']

        #The model framework differentiates by site to accomidate the NPN dataset.
        #For datasets that are a single site, add in a column for it
        if add_site_dummy_var:
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

    def load_next_dataset(self):
        pass

    def get_next_dataset(self):
        pass

    #Return a model to optimize and info about the model as a single tuple
    #ready to be send over MPI
    def get_next_job(self):
        if not self.jobs_available:
            return 'no jobs left'

        job_info=self.job_list.pop()
        #A bootsrapped sample with replacment of this species observations
        data_sample = self.observation_data[self.observation_data.species==job_info['species']].sample(frac=1, replace=True).copy()
        #Temperature data at sites where this species occures
        sp_temp_data = self.temp_data[self.temp_data.Site_ID.isin(data_sample.Site_ID.unique())].copy()

        model=uniforc_model(temp_data=sp_temp_data, plant_data=data_sample, t1_varies=self.include_t1_parameter)
        return (model, self.bounds, job_info['species'], job_info['bootstrap_i'], self.dataset_name)

    def jobs_available(self):
        return len(self.job_list)>0
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
    config={}

    config['dataset_name']='npn'
    config['observations_data_file'] = './cleaned_data/NPN_observations.csv'
    config['temp_data_file'] = './cleaned_data/npn_temp.csv'
    config['include_t1_parameter']=False
    results_file='npn_results.csv'

    job_queue = data_store(dataset_config=config, num_bootstrap=500)
    #harvard_data['Site_ID']=1
    #harvard_temp['Site_ID']=1

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
        optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=False)
        x=optimize_output['x']

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
