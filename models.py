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

        #Get associated temperature data for each observation
        plant_data = plant_data.merge(temp_data, on=['Site_ID','year'], how='left')

        #doy are the columns here from -127 to 180
        self.temp_observations = plant_data.drop(['species','Site_ID','year','doy'], axis=1).values
        #actual day of year values where 1 = Jan 1
        self.temp_doy =temp_data.drop(['Site_ID','year'], axis=1).columns.values.astype(np.int)

        self.doy_observations = plant_data['doy'].values

        self.num_replicates=plant_data.shape[0]

        #Indexes to find the temp infor for each site and year
        #self.temp_year=temp_data['year'].values
        #self.temp_site=temp_data['Site_ID'].values
        #self.num_doy  = self.temp_doy.shape[0]


        #Plant data
        #self.plant_site=plant_data['Site_ID'].values
        #self.plant_year=plant_data['year'].values
        #self.plant_doy =plant_data['doy'].values

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
        elif self.model_name=='unichill':
            self.model = self.unichill
        else:
            print('unknown model type: ' + model_name)

    #Get a site/year x doy boolean array of the days meeting the F* requirement
    #This is for all site/year combinations, which needs to match up exactly
    #with all entires in plant_data
    def get_doy_estimates(self, **kwargs):
        return self.model(**kwargs)

    def doy_estimator(self, gdd, doy_index, F):
        #If F is not met for a particular row, ensure that a large doy
        #gets (1000) returned so it produces a large error
        gdd = np.column_stack((gdd, np.repeat(100000, gdd.shape[0])))
        doy_index = np.append(doy_index, 1000)

        #Repeating the full doy index for each row in gdd
        doy_index = np.tile(doy_index, (gdd.shape[0],1))

        #The doy for each row where F was met
        doy_indexes = np.argmax(gdd>=F, axis=1)

        return doy_index[np.arange(len(doy_index)), doy_indexes]

    #temps has site/year observations by rows, and doy in columns.
    #return the accumulated forcing/gdd for each doy for each observation
    def gdd_calculator(self, temps):
        return temps.cumsum(axis=1)

    #simple gdd model. 
    #t1: day gdd accumulation begins
    #T: temperature cutoff for GDD
    #F: total gdd required
    def gdd(self, t1,T,F):
        all_site_temps = self.temp_observations.copy()

        #Temperature cutoff
        all_site_temps[all_site_temps<T]=0

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=self.gdd_calculator(all_site_temps)

        return self.doy_estimator(all_site_daily_gdd, self.temp_doy, F)

    #uniforc model from Chuine 2000
    #t1: day forcing accumulation begins
    #b,c: daily temp fitting paramters
    #F: total forcing required
    def uniforc(self, t1, b, c, F):
        all_site_temps = self.temp_observations.copy()

        all_site_temps = 1 / (1 + np.exp(b*(all_site_temps-c)))

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy<t1]=0

        all_site_daily_gdd=self.gdd_calculator(all_site_temps)

        return self.doy_estimator(all_site_daily_gdd, self.temp_doy, F)

    #unichill model from Chuine 2000 (8 parameters total)
    #t0: day chilling accumulation begins
    #a_c, b_c, c_c: fitting paramters for chilling sigmoid function
    #C: total chilling units requied
    #b_c, c_c: fitting paramters for heating sigmoid function
    #F: total forcing required
    def unichill(self, t0, a_c, b_c, c_c, C, b_f, c_f, F):
        all_site_temps_chill = self.temp_observations.copy()
        all_site_temps_heat = self.temp_observations.copy()

        all_site_temps_chill = 1 / (1 + np.exp(a_c*((all_site_temps_chill - c_c)**2) + b_c*(all_site_temps_chill-c_c)))
        all_site_temps_heat = 1 / (1 + np.exp(b_f*(all_site_temps_heat-c_f)))

        #Only accumulate chilling after t0
        all_site_temps_chill[:,self.temp_doy<t0]=0

        all_site_accumulated_chill=self.gdd_calculator(all_site_temps_chill)

        #Heat forcing accumulation starts when the chilling requirement, C, has been met
        #Enforce this by setting everything prior to that date to 0
        F_begin = self.doy_estimator(all_site_accumulated_chill, self.temp_doy, C)
        for row in range(F_begin.shape[0]):
            all_site_temps_heat[row, self.temp_doy<F_begin[row]]=0

        all_site_accumulated_heat=self.gdd_calculator(all_site_temps_heat)

        return self.doy_estimator(all_site_accumulated_heat, self.temp_doy, F)

    #RMSE of the estimated budburst doy of all sites
    def get_error(self, **kargs):
        doy_estimates=self.get_doy_estimates(**kargs)
        return np.sqrt(np.mean((doy_estimates - self.doy_observations)**2))

    #upper and lower bounds used in scipy.optimize.differential_evolution
    #ranges of possible values taken from Roberts et al. 2015 and Chuine 2000 with some buffer added
    #Values are constrained to either + or - when biologically relevent, see manuscript text. 
    def get_scipy_parameter_bounds(self):
        if self.model_name=='uniforc':
            #           t1         b         c       F*
            return [(-126,180), (-20,0), (-100,100), (0,200)]
        elif self.model_name=='unichill':
            #           t0         a_c     b_c     c_c        C        b_f      c_f       F
            return [(-126,180), (0,10), (-50,50), (-50,50), (0,300), (-20,0), (-100,100), (0,200)]
        elif self.model_name=='gdd':
            #           t1         T         F*
            return [(-126,180), (-20,20), (0,500)]

    #Organize the optimized parameter output from scipy.optimize in a nice labeled dictionary
    def translate_scipy_parameter_output(self, x):
        o={}
        if self.model_name=='uniforc':
            o['t1'], o['b'], o['c'], o['F'] = x[0], x[1], x[2], x[3]
        elif self.model_name=='unichill':
            o['t0'], o['a_c'], o['b_c'], o['c_c'], o['C'], o['b_f'], o['c_f'], o['F'] = x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]
        elif self.model_name=='gdd':
            o['t1'], o['T'], o['F'] = x[0], x[1], x[2]

        return o

    #scipy optimize functions want a array of parameter values
    #use this to unpack it
    def scipy_error(self,x):
        if self.model_name=='uniforc':
            kargs={'t1':x[0], 'b':x[1], 'c':x[2], 'F':x[3]}
        elif self.model_name=='unichill':
            kargs={'t0':x[0], 'a_c':x[1], 'b_c':x[2], 'c_c':x[3], 'C':x[4], 'b_f':x[5], 'c_f':x[6], 'F':x[7]}
        elif self.model_name=='gdd':
            kargs={'t1':x[0], 'T':x[1], 'F':x[2]}

        return self.get_error(**kargs)
