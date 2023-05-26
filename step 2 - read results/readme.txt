This folder contains three files:
1. read_all_jobs. This "sh" file initiates the cluster computer to run the file read_data
2. read_data. This file sets some parameters and then initiates the read_data_functions file
3. read_data_functions_file. This file contains all functions. They take as input the saved data from "step 1 - run algorithms" and have as output the metrics at each iteration of the MCMC chain. 

