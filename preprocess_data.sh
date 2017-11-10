#!/bin/bash

#Produce the respective cleaned_data/*_observations.csv files
#from everything in raw_data/
Rscript data_preprocessing/compile_harvard_data.R
Rscript data_preprocessing/compile_jornada.R
Rscript data_preprocessing/compile_hubbard_brook.R
Rscript data_preprocessing/compile_hjandrews_data.R
#NPN must be run last because it filters based on a
#species list made by the others
Rscript data_preprocessing/compile_npn_data.R

#Go thru the compiled data and create a new column
#for testing/training splits
Rscript data_preprocessing/mark_out_of_sample_data.R

#This step takes a few hours and 400mb of disk space
Rscript data_preprocessing/extract_prism_temps.R
