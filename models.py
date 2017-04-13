import pandas as pd
import numpy as np

########################################################################
#Evaluate a UniForc model of a single species
#uniforc model evaluator designed to be used in any optimize function.
#accepts a set of temperature data and observations of phenological event
#as the doy. Returns the RMSE error of the doy given the  4 UniForc parameters
class phenology_model:
    def __init__(self, temp_data, plant_data, model_name):
        #Create a ['Site_ID','year'] x 'doy' matrix with daily temp as the value
        #This is done to calculate the GDD info for all sites/years at once
        temp_data = temp_data.pivot_table(index=['Site_ID','year'], columns='doy', values='temp').reset_index()

        #doy are the columns here from -127 to 180
        self.temp_temp =temp_data.drop(['Site_ID','year'], axis=1).values

        #Indexes to find the temp infor for each site and year
        self.temp_year=temp_data['year'].values
        self.temp_site=temp_data['Site_ID'].values
        self.temp_doy =temp_data.drop(['Site_ID','year'], axis=1).columns.values.astype(np.int)
        self.num_doy  = self.temp_doy.shape[0]

        #Plant data
        self.plant_site=plant_data['Site_ID'].values
        self.plant_year=plant_data['year'].values
        self.plant_doy =plant_data['doy'].values
        self.num_replicates=plant_data.shape[0]

        self.model_name=model_name
        if self.model_name=='uniforc':
            self.model = self.uniforc
        elif self.model_name=='gdd':
            self.model = self.gdd
        else:
            print('unknown model type: ' + model_name)

    #Get a site/year x doy boolean array of the days meeting the F* requirement
    #This is for all site/year combinations, which needs to match up exactly
    #with all entires in plant_data
    def calculate_doy_estimates(self, **kwargs):
        return self.model(**kwargs)

    #simple gdd model. 
    #t1: day gdd accumulation begins
    #T: temperature cutoff for GDD
    #F: total gdd required
    def gdd(self, t1,T,F):
        all_site_temps = self.temp_temp.copy()

        #Temperature cutoff
        all_site_temps[all_site_temps<T]=0

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=np.zeros_like(all_site_temps)
        for doy in range(self.num_doy):
            all_site_daily_gdd[:,doy] = all_site_temps[:,0:doy+1].sum(1)

        #The predicted doy for each site/year. If none was predicted give a doy which
        #will return a very large error
        doy_estimates = np.zeros(all_site_daily_gdd.shape[0])
        for site_year in range(all_site_daily_gdd.shape[0]):
            this_estimate=self.temp_doy[all_site_daily_gdd[site_year]>=F]
            doy_estimates[site_year]=this_estimate[0] if this_estimate.shape[0]>0 else 1000

        return doy_estimates

    #uniforc model from Chuine 2000
    def uniforc(self, t1, b, c, F):
        if b >0:
            b = b*-1
        #print(t1,b,c,F)
        all_site_temps = self.temp_temp.copy()

        all_site_temps = 1 / (1 + np.exp(b*(all_site_temps-c)))

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=np.zeros_like(all_site_temps)
        #all_site_daily_gdd=theano.tensor.zeros_like(all_site_temps)
        for doy in range(self.num_doy):
            all_site_daily_gdd[:,doy] = all_site_temps[:,0:doy+1].sum(1)

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

    #Estimated doy for all entries in self.plant_data
    def get_all_estimates(self, **kargs):
        site_year_doy_estimates=self.calculate_doy_estimates(**kargs)
        doy_estimates=[]
        for row in range(self.num_replicates):
            estimated_doy = site_year_doy_estimates[(self.temp_site == self.plant_site[row]) & (self.temp_year == self.plant_year[row])]
            doy_estimates.append(estimated_doy[0])

        #print(doy_estimates)
        return np.array(doy_estimates)

    #scipy optimize functions want a array of parameter values
    #use this to unpack it
    def scipy_error(self,x):
        if self.model_name=='uniforc':
            kargs={'t1':x[0], 'b':x[1], 'c':x[2], 'F':x[3]}
        elif self.model_name=='gdd':
            kargs={'t1':x[0], 'T':x[1], 'F':x[2]}

        return self.get_error(**kargs)
