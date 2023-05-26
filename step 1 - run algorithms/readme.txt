This folder contains the code to run all 5 algorithms on instances with 10 variables, 10,20 or 100 observations, and on the graph types cluster, random and scale-free. The folder contains 3 R files:

1. Send_all_jobs. This R file initiates the cluster computer to run all the file run_and_save.R for each instance. 
2. run_and_save. This file sets all hyperparameters (burn-in, MCMC iterations, initial graph of the Markov Chain, etc.) and runs the file run_experiments 50 times.
3. run_experiments. This file does three things:
- It creates a graph, precision matrix and data (using the function bdgraph.sim) 
- It solves this instance using all five of the algorithms
- It saves the solutions


