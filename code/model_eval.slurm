#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-02:00:00
#SBATCH --array=1-100
#SBATCH --cpus-per-task=1

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --job-name="pk_eval"
#SBATCH --account="punim1449"

#SBATCH --mem=12G

date

if [ $# -eq 0 ]
  then
    echo "No model run name supplied."
    exit 1
fi

# Load required modules
module load foss/2020b
module load r/4.0.4
module load rgdal/1.5-23-r-4.0.4

date

time Rscript code/R/evaluation/eval_IML.R ${SLURM_ARRAY_TASK_ID} $1
