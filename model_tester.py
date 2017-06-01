import pandas as pd
import matplotlib.pyplot as plt
from scipy import optimize
from models import phenology_model

#just a quick script to load a single species and test things with

harvard_data = pd.read_csv('./cleaned_data/harvard_observations.csv')
harvard_temperature = pd.read_csv('./cleaned_data/harvard_temp.csv')
harvard_temperature['Site_ID']=1
harvard_data['Site_ID']=1
maple_data = harvard_data[harvard_data.species=='acer rubrum']

unichill = phenology_model(temp_data = harvard_temperature.copy(), plant_data=maple_data.copy(), model_name='unichill')
uniforc = phenology_model(temp_data = harvard_temperature.copy(), plant_data=maple_data.copy(), model_name='uniforc')
gdd = phenology_model(temp_data = harvard_temperature.copy(), plant_data=maple_data.copy(), model_name='gdd')

for model in [unichill, uniforc, gdd]:
    bounds = model.get_scipy_parameter_bounds()
    print(model.model_name)
    for rep in range(10):
        optimize_output = optimize.differential_evolution(model.scipy_error, bounds=bounds, disp=False, popsize=100, mutation=1.5, recombination=0.25)
        parameters = model.translate_scipy_parameter_output(optimize_output['x'])
        print(parameters)



#for model in [unichill, uniforc, gdd]:
#    bounds = model.get_scipy_parameter_bounds()
#    optimize_output = optimize.differential_evolution(model.scipy_error, bounds=bounds, disp=False)
#    print('')
#    print(model.model_name)
#    print(optimize_output)
#    print('')

