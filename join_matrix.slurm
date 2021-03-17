#!/bin/bash
# Created by the University of Melbourne job script generator for SLURM
# Sat Aug 15 2020 21:15:43 GMT+1000 (AEST)

# Partition for the job:
#SBATCH --partition=physical

# Multithreaded (SMP) job: must run on one node 
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="pk_covariates"

# The project ID which this job should run under:
#SBATCH --account="punim1449"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4

#require more memory:
#SBATCH --mem=131072


# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=0-4:0:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

date

# Load required modules
module load r/4.0.0
module load gdal/3.0.2-python-3.7.4


time Rscript code_ruarai/R/produce_final_summaries.R
