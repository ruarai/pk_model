#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-04:00:00
#SBATCH --array=1-100
#SBATCH --cpus-per-task=5

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --job-name="pk_brt"
#SBATCH --account="punim1449"

#SBATCH --mem=48G

date

if [ $# -eq 0 ]
  then
    echo "No model run name supplied."
    exit 1
fi

# Load required modules
module load r/4.0.0
module load gdal/3.0.2-python-3.7.4

date

time Rscript code/R/model_run.R ${SLURM_ARRAY_TASK_ID} $1 ${SLURM_CPUS_PER_TASK}

time Rscript code/R/produce_predictions.R ${SLURM_ARRAY_TASK_ID} $1 ${SLURM_CPUS_PER_TASK}
