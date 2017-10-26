import pandas as pd
import numpy as np

########################################################################
#Evaluate a phenology forcing model of a single species
#model evaluator designed to be used in any optimize function.
#accepts a set of temperature data and observations of phenological event
#as the doy. 
class phenology_model:
    def __init__(self, temp_data, plant_data, model_name, error_type='rmse'):
        #Create a ['Site_ID','year'] x 'doy' matrix with daily temp as the value
        #This is done to calculate the GDD info for all sites/years at once
        temp_data = temp_data.pivot_table(index=['Site_ID','year'], columns='doy', values='temp').reset_index()

        #This first day of temperature data causes NA issues because of leap years
        temp_data.drop(-67, axis=1, inplace=True)

        #Get associated temperature data for each observation
        plant_data = plant_data.merge(temp_data, on=['Site_ID','year'], how='left')
        #Some observations have NA temperature values, most likely cause their
        #coordinates end up over water.
        plant_data.dropna(axis=0, inplace=True)

        #doy are the columns here from -128 to 180
        self.temp_observations = plant_data.drop(['species','Site_ID','year','doy','Phenophase_ID'], axis=1).values

        #actual day of year values where 1 = Jan 1
        self.temp_doy =temp_data.drop(['Site_ID','year'], axis=1).columns.values.astype(np.int)

        self.doy_observations = plant_data['doy'].values
        self.year_observations = plant_data['year'].values
        self.site_observations = plant_data['Site_ID'].values

        self.num_replicates=plant_data.shape[0]

        self.error_type=error_type
        self.model_name=model_name
        if self.model_name=='uniforc':
            self.model = self.uniforc
        elif self.model_name=='gdd':
            self.model = self.gdd
        elif self.model_name=='gdd_fixed':
            self.model = self.gdd_fixed
        elif self.model_name=='unichill':
            self.model = self.unichill
        elif self.model_name=='alternating':
            self.model = self.alternating
        elif self.model_name=='naive':
            self.model = self.naive
        elif self.model_name=='linear_temp':
            self.model = self.linear_temp
            #Do some preprocessing for this one
            spring_days = np.logical_and(self.temp_doy>=1,self.temp_doy<=90)
            all_site_temps = self.temp_observations[:,spring_days].copy()
            self.mean_spring_temps = all_site_temps.mean(axis=1)
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

    # Alternating model, originally defined in Cannell & Smith 1983
    # Phenological event happens the first day that GDD (above 5C)
    # is greater than an exponential curve of number of chill days (below 5C)
    def alternating(self, a, b, c, threshold=5, **kwargs):
        # Number of days below threshold from Jan 1
        chill_days = ((self.temp_observations < threshold)*1).copy()
        chill_days[:,self.temp_doy < 0]=0
        chill_days = self.gdd_calculator(chill_days)

        # Accumulated growing degree days from Jan 1
        gdd = self.temp_observations.copy()
        gdd[gdd < threshold]=0
        gdd[:,self.temp_doy < 0]=0
        gdd = self.gdd_calculator(gdd)

        # Phenological event happens the first day gdd is > chill_day curve
        chill_day_curve = a + b * np.exp( c * chill_days)
        difference = gdd - chill_day_curve

        return self.doy_estimator(difference, self.temp_doy, F=0)

    #Linear temperature model
    #A linear regression between mean spring temperature(Jan 1 - March 31)
    #and the event doy. Mean spring temps don't change with parameters, so
    #it is calculated in __init__
    def linear_temp(self, intercept, slope, **kwargs):
        return self.mean_spring_temps*slope + intercept

    #Naive model
    #Uses only the mean doy from all observations
    def naive(self, mean_doy, **kwargs):
        return np.repeat(mean_doy, self.num_replicates)


    #simple gdd model with fixed t1 (set to Jan 1) and T (set to 0) 
    #F: total gdd required
    def gdd_fixed(self, F, **kwargs):
        all_site_temps = self.temp_observations.copy()

        #Temperature cutoff
        all_site_temps[all_site_temps < 0]=0

        #Only accumulate forcing after t1
        all_site_temps[:,self.temp_doy < 0]=0

        all_site_daily_gdd=self.gdd_calculator(all_site_temps)

        return self.doy_estimator(all_site_daily_gdd, self.temp_doy, F)

    #simple gdd model. 
    #t1: day gdd accumulation begins
    #T: temperature cutoff for GDD
    #F: total gdd required
    def gdd(self, t1, T, F, **kwargs):
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
    def uniforc(self, t1, b, c, F, **kwargs):
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
    def unichill(self, t0, a_c, b_c, c_c, C, b_f, c_f, F, **kwargs):
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

    #Total error of the estimates given the parameters
    #r2 is R^2 from the 1:1 line. It is not subtracted from 1 because the scipy
    #optimizer does minimization
    def get_error(self, **kargs):
        doy_estimates=self.get_doy_estimates(**kargs)
        if self.error_type=='rmse':
            error = np.sqrt(np.mean((doy_estimates - self.doy_observations)**2))
        elif self.error_type=='r2':
            error = np.sum((self.doy_observations - doy_estimates)**2) / np.sum((self.doy_observations - np.mean(self.doy_observations))**2)

        return error

    #upper and lower bounds used in scipy.optimize.differential_evolution
    #ranges of possible values taken from Roberts et al. 2015 and Chuine 2000 with some buffer added
    #Values are constrained to either + or - when biologically relevent, see manuscript text. 
    def get_scipy_parameter_bounds(self):
        if self.model_name=='uniforc':
            #           t1         b         c       F*
            return [(-67,298), (-20,0), (-100,100), (0,200)]
        elif self.model_name=='unichill':
            #           t0         a_c     b_c     c_c        C        b_f      c_f       F
            return [(-67,298), (0,20), (-100,100), (-50,50), (0,300), (-20,0), (-100,100), (0,200)]
        elif self.model_name=='gdd':
            #           t1         T         F*
            return [(-67,298), (-25,25), (0,1000)]
        elif self.model_name=='alternating':
            #           a         b         c
            return [(-5000,5000), (-5000,5000), (0,100)]
        elif self.model_name=='gdd_fixed':
            #          F*
            return [(0,2500)]
        elif self.model_name=='naive':
            #           mean_doy
            return [(-67,298)]
        elif self.model_name=='linear_temp':
            #           intercept     slope
            return [(-200,200), (-50,50)]

    #Organize the optimized parameter output from scipy.optimize in a nice labeled dictionary
    def translate_scipy_parameter_output(self, x):
        o={}
        if self.model_name=='uniforc':
            o['t1'], o['b'], o['c'], o['F'] = x[0], x[1], x[2], x[3]
        elif self.model_name=='unichill':
            o['t0'], o['a_c'], o['b_c'], o['c_c'], o['C'], o['b_f'], o['c_f'], o['F'] = x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]
        elif self.model_name=='gdd':
            o['t1'], o['T'], o['F'] = x[0], x[1], x[2]
        elif self.model_name=='alternating':
            o['a'], o['b'], o['c'] = x[0], x[1], x[2]
        elif self.model_name=='gdd_fixed':
            o['F'] = x[0]
        elif self.model_name=='naive':
            o['mean_doy'] = x[0]
        elif self.model_name=='linear_temp':
            o['intercept'], o['slope'] = x[0], x[1]

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
        elif self.model_name=='alternating':
            kargs={'a':x[0], 'b':x[1], 'c':x[2]}
        elif self.model_name=='gdd_fixed':
            kargs={'F':x[0]}
        elif self.model_name=='naive':
            kargs={'mean_doy':x[0]}
        elif self.model_name=='linear_temp':
            kargs={'intercept':x[0], 'slope':x[1]}

        return self.get_error(**kargs)
