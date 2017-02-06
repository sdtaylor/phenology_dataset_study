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
        self.temp_sites=temp_data['Site_ID'].values
        self.temp_year =temp_data['year'].values
        self.temp_doy  =temp_data['doy'].values
        self.temp_temp =temp_data['temp'].values

        self.plant_site=plant_data['Site_ID'].values
        self.plant_year=plant_data['year'].values
        self.plant_doy =plant_data['doy'].values
        self.num_replicates=plant_data.shape[0]

        self.t1_varies=t1_varies

    def site_doy_estimate(self, site_id, year, t1, b, c, F, t1_slope=None):
        subset_temp = self.temp_temp[(self.temp_sites==site_id) &
                                     (self.temp_year==year)].copy()
        subset_doy = self.temp_doy[(self.temp_sites==site_id) &
                                     (self.temp_year==year)].copy()

        #Daily forcing according to sigmoid function params
        subset_temp = 1 / (1 + np.exp(b*(subset_temp-c)))

        #If fitting NPN data, let t1 vary with respect to mean Jan-Feb temp
        #The t1 being estimated by the optimizer is thus the intercept
        #as opposed to the mean value
        if self.t1_varies:
            T_jan_feb = np.mean(subset_temp[(subset_doy>=0) & (subset_doy<=60)])
            t1 = t1 + t1_slope*T_jan_feb

        #Only accumulate after t1
        subset_temp[subset_doy<=t1]=0
        daily_gdd = np.array([np.sum(subset_temp[0:i+1]) for i in range(subset_temp.shape[0])])

        #First day where GDD>=F*
        if np.sum(daily_gdd>=F)==0:
            return 0
        else:
            return subset_doy[daily_gdd>=F][0]

    #RMSE of the estimated budburst doy of all sites
    def get_error(self, **kargs):
        errors = []
        for row in range(self.num_replicates):
            estimated_doy = self.site_doy_estimate(site_id=self.plant_site[row],
                                                   year=self.plant_year[row],
                                                   **kargs)
            #print(estimated_doy)
            errors.append(estimated_doy - self.plant_doy[row])

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


#A wrapper to get the results of a model so that they can
#be run asynchronously
def run_optimizer(model, bounds, species, bootstrap_num, dataset):
    optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=True)
    x=optimize_output['x']
    t1_int, b, c, F, t1_slope = x[0], x[1], x[2], x[3], x[4]
    #t1_int, b, c, F, t1_slope = 1,2,3,4,5

    return {'t1_int':t1_int, 'b':b, 'c':c, 'F':F, 't1_slope':t1_slope,
            'species':species, 'boostrap_num':bootstrap_iteration, 'dataset':dataset}

#######################################################

########################################################


def master():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    num_workers = MPI.COMM_WORLD.Get_size()

    work_tag=0
    stop_tag=1
    job_list=[]
    ###################################
    harvard_data = pd.read_csv('./cleaned_data/harvard_observations.csv')
    harvard_temp = pd.read_csv('./cleaned_data/harvard_temp.csv')
    harvard_data['Site_ID']=1
    harvard_temp['Site_ID']=1

    num_bootstrap=200
    #             t1         b         c       F*     t1_slope
    bounds = [(-126,180), (-20,0), (-50,50), (0,100), (-20,20)]

    results=[]
    for species in harvard_data.species.unique():
        print(species)
        sp_data = harvard_data[harvard_data.species==species]
        for bootstrap_i in range(num_bootstrap):
            data_sample = sp_data.sample(frac=1, replace=True).copy()
            model=uniforc_model(temp_data=harvard_temp, plant_data=data_sample, t1_varies=True)
            package = (model, bounds, species, bootstrap_i, 'harvard')
            job_list.append(package)

    total_jobs=len(job_list)
    #Dole out the first round of jobs to all workers
    for i in range(1, num_workers):
        if len(job_list)>0:
            next_job = job_list.pop() if len(job_list)>0 else 'done'
        else:
            break
        comm.send(obj=next_job, dest=i, tag=work_tag)

    #While there are new jobs to assign.
    #Collect results and assign new jobs as others are finished.
    while len(job_list)>0:
        next_job = job_list.pop() if len(job_list)>0 else 'done'
        job_result = comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
        results.append(job_result)
        comm.send(obj=next_job, dest=status.Get_source(), tag=work_tag)
        num_jobs_left=len(job_list)
        print('Completed job '+str(total_jobs-num_jobs_left)+' of '+str(total_jobs))

    #Collect last jobs
    for i in range(1, num_workers):
        job_result = comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG)
        results.append(job_result)
    for i in range(1, num_workers):
        comm.send(obj=None, dest=i, tag=stop_tag)

    results=pd.DataFrame(results)
    results.to_csv('results.csv', index=False)

def worker():
    comm = MPI.COMM_WORLD
    status = MPI.Status()
    while 1:
        model_package = comm.recv(source=0, tag=MPI.ANY_TAG, status=status)
        if status.Get_tag() == 1: break

        model, bounds, species, bootstrap_i, dataset = model_package
        optimize_output = optimize.differential_evolution(model.scipy_error,bounds=bounds, disp=False)
        x=optimize_output['x']
        t1_int, b, c, F, t1_slope = x[0], x[1], x[2], x[3], x[4]

        return_data={'t1_int':t1_int, 'b':b, 'c':c, 'F':F, 't1_slope':t1_slope,
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
