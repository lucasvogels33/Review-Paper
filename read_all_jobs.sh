#!/bin/bash
#SBATCH --job-name=read_all_jobs
#SBATCH --output=read_all_jobs_output
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=1-23:59:00
#SBATCH --partition=normal
#SBATCH --constraint=gold_6130

module load 2022
module load R/4.2.1-foss-2022a
Rscript --no-save --slave read_data.R