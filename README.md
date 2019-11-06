This is the repository for the study:

Taylor, S.D., J.M. Meiners, K. Riemer, M.C. Orr, E.P White. 2018. Comparison of large-scale citizen science data and long-term study data for phenology modeling. Ecology [https://doi.org/10.1002/ecy.2568](https://doi.org/10.1002/ecy.2568).

### Contents

analysis/ - The scripts to run the analysis and make all the figures in the paper  
data_preprocessing/ - Scripts to clean and organize raw data files  
model_fitting/ - Model building and predictions (Note, these are all in python)  

raw_data/ - The data in the form downloaded from the data sources. 
cleaned_data/ - Data used in model building and analyses.  
results/ - Model parameters and predictions  

manuscript/ - Figures and manuscript in latex format  

config.yaml - various model and dataset configurations  

preprocess_data.sh - This will run all the scripts in the data_preprocessing folder and produce the contents of the cleaned_data folder  
analysis_and_figures.sh - This will run all the scripts in the analysis folder and produce all the images in the manuscript  
manuscript_map.R - This produces figure 1, the map of all sites  

Note the model_fitting routines take several days on a university cluster to run, so it's not recommended to run these. The math of all the models in described in model_fitting/models.py. The output from the fitting routines produce 3 files which are provided:  
results/model_parameters.csv.gz - The model parameters derived from all datasets and all bootstrap iterations  
results/predictions.csv.gz - Predictions from the models for all observations and using the mean value of bootstrap iterations  
results/predictions_large.csv.gz - Predictions from models for all observations and all bootstrap iterations. This file is ~300MB so is not available on the GitHub repo, but can be obtained from the zenodo repo.   

