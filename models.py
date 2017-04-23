import pandas as pd
import numpy as np

########################################################################
#Evaluate a phenology forcing model of a single species
#model evaluator designed to be used in any optimize function.
#accepts a set of temperature data and observations of phenological event
#as the doy. 
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

        #A helper array for calculating GDD.
        #WIP 
        #self.doy_sum_matrix = np.ones((self.num_doy, self.num_doy)).astype(bool)
        #for i in range(self.num_doy):
        #    self.doy_sum_matrix[(i+1):self.num_doy,i] = False
        #self.doy_sum_matrix = np.tile(self.doy_sum_rules, (self.num_replicates, 1,1))

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

    #gdd has site/year observarionts by rows, and doy in columns.
    #returns the first doy which meets forcing requirement F for each observation
    #if it's not met then return a very large doy estimate to produce a large error
    def doy_estimator(self, gdd, doy_index, F):
        doy_estimates = np.zeros(gdd.shape[0])
        for site_year in range(gdd.shape[0]):
            this_estimate=doy_index[gdd[site_year]>=F]
            doy_estimates[site_year]=this_estimate[0] if this_estimate.shape[0]>0 else 1000
        return doy_estimates

    #temps has site/year observations by rows, and doy in columns.
    #return the accumulated forcing/gdd for each doy for each observation
    def gdd_calculator(self, temps, num_doy):
        gdd = np.zeros_like(temps)
        for doy in range(num_doy):
            gdd[:,doy] = temps[:,0:doy+1].sum(1)
        return gdd

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

        all_site_daily_gdd=self.gdd_calculator(all_site_temps, self.num_doy)

        return self.doy_estimator(all_site_daily_gdd, self.temp_doy, F)

    #uniforc model from Chuine 2000
    #t1: day forcing accumulation begins
    #b,c: daily temp fitting paramters
    #F: total forcing required
    def uniforc(self, t1, b, c, F):
        all_site_temps = self.temp_temp.copy()

        all_site_temps = 1 / (1 + np.exp(b*(all_site_temps-c)))

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=self.gdd_calculator(all_site_temps, self.num_doy)

        return self.doy_estimator(all_site_daily_gdd, self.temp_doy, F)

    #RMSE of the estimated budburst doy of all sites
    def get_error(self, **kargs):
        doy_estimates=self.calculate_doy_estimates(**kargs)
        errors = []
        for row in range(self.num_replicates):
            estimated_doy = doy_estimates[(self.temp_site == self.plant_site[row]) & (self.temp_year == self.plant_year[row])]
            if len(estimated_doy)>1: print('>1 estimated doy')
            errors.append(estimated_doy[0] - self.plant_doy[row])

        errors = np.array(errors)
        return np.sqrt(np.mean(errors**2))

    #Estimated doy for all entries in self.plant_data given parameters
    def get_all_estimates(self, **kargs):
        site_year_doy_estimates=self.calculate_doy_estimates(**kargs)
        doy_estimates=[]
        for row in range(self.num_replicates):
            estimated_doy = site_year_doy_estimates[(self.temp_site == self.plant_site[row]) & (self.temp_year == self.plant_year[row])]
            doy_estimates.append(estimated_doy[0])

        return np.array(doy_estimates)

    #upper and lower bounds used in scipy.optimize.differential_evolution
    def get_scipy_parameter_bounds(self):
        if self.model_name=='uniforc':
            #           t1         b         c       F*
            return [(-126,180), (-20,0), (-50,50), (0,100)]
        elif self.model_name=='gdd':
            #           t1         T         F*
            return [(-126,180), (-20,20), (0,500)]

    #Organize the optimized parameter output from scipy.optimize in a nice labeled dictionary
    def translate_scipy_parameter_output(self, x):
        o={}
        if self.model_name=='uniforc':
            o['t1'], o['b'], o['c'], o['F'] = x[0], x[1], x[2], x[3]
        elif self.model_name=='gdd':
            o['t1'], o['T'], o['F'] = x[0], x[1], x[2]

        return o

    #scipy optimize functions want a array of parameter values
    #use this to unpack it
    def scipy_error(self,x):
        if self.model_name=='uniforc':
            kargs={'t1':x[0], 'b':x[1], 'c':x[2], 'F':x[3]}
        elif self.model_name=='gdd':
            kargs={'t1':x[0], 'T':x[1], 'F':x[2]}

        return self.get_error(**kargs)
