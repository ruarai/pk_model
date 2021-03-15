#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-00:5:00
#SBATCH --array=1-625
#SBATCH --cpus-per-task=4

#SBATCH --output=slurm_out/pk_brt.%A_%a.out
#SBATCH --error=slurm_out/pk_brt.%A_%a.error

#SBATCH --job-name="pk_brt"
#SBATCH --account="punim1449"

#SBATCH --mem=10240

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

date

# Load required modules
module load r/4.0.0

time Rscript code_ruarai/R/model_run.R ${SLURM_ARRAY_TASK_ID}
