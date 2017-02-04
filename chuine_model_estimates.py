import pandas as pd
import numpy as np
from scipy import optimize

class uniforc_model:
    def __init__(self, temp_data, plant_data, site_data=None):
        self.temp_sites=temp_data['Site_ID'].values
        self.temp_year =temp_data['year'].values
        self.temp_doy  =temp_data['doy'].values
        self.temp_temp =temp_data['temp'].values

        self.plant_site=plant_data['Site_ID'].values
        self.plant_year=plant_data['year'].values
        self.plant_doy =plant_data['doy'].values
        self.num_replicates=plant_data.shape[0]

        #self.site_site=site_data['Site_ID'].values
        #self.site_mean_temp=site_data['mean_temp'].values

    def site_doy_estimate(self, site_id, year, t1, b, c, F):
        subset_temp = self.temp_temp[(self.temp_sites==site_id) &
                                     (self.temp_year==year)].copy()
        subset_doy = self.temp_doy[(self.temp_sites==site_id) &
                                     (self.temp_year==year)].copy()

        #Daily forcing according to sigmoid function params
        subset_temp = 1 / (1 + np.exp(b*(subset_temp-c)))
        #Only accumulate after t1
        subset_temp[subset_doy<=t1]=0
        daily_gdd = np.array([np.sum(subset_temp[0:i+1]) for i in range(subset_temp.shape[0])])

        #First day where GDD>=F*
        if np.sum(daily_gdd>=F)==0:
            return 0
        else:
            return subset_doy[daily_gdd>=F][0]

    #RMSE of the estimated budburst doy of all sites
    def get_error(self, t1, b, c, F):
        errors = []
        for row in range(self.num_replicates):
            estimated_doy = self.site_doy_estimate(site_id=self.plant_site[row],
                                                   year=self.plant_year[row],
                                                   t1=t1, b=b, c=c, F=F)
            #print(estimated_doy)
            errors.append(estimated_doy - self.plant_doy[row])

        errors = np.array(errors)
        return np.sqrt(np.mean(errors**2))

    #scipy optimize functions want a array of parameter values
    #use this to unpack it
    def scipy_error(self,x):
        t1, b, c, F = x[0], x[1], x[2], x[3]
        return self.get_error(t1,b,c,F)

########################################################
maple = pd.read_csv('maple.csv')
harvard_temp = pd.read_csv('./cleaned_data/harvard_temp.csv')
maple['Site_ID']=1
harvard_temp['Site_ID']=1

maple_model=uniforc_model(temp_data=harvard_temp, plant_data=maple)


########################################################
starting_values = [0, -1., 10, 50]

ret = optimize.basinhopping(maple_model.scipy_error, x0=starting_values, niter=100, minimizer_kwargs={'method':'Nelder-Mead'})
#print(ret)
