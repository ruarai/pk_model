#!/bin/bash

# Partition for the job:
#SBATCH --partition=physical


#SBATCH --ntasks=1
#SBATCH --time=0-00:5:00
#SBATCH --array=1-40
#SBATCH --cpus-per-task=4

#SBATCH --job-name="pk_pred"
#SBATCH --account="punim1449"

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --mem=10240

date

# Load required modules
module load r/4.0.0
module load gdal/3.0.2-python-3.7.4

time Rscript code_ruarai/R/produce_predictions.R ${SLURM_ARRAY_TASK_ID}
