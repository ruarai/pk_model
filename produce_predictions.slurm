#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-00:20:00
#SBATCH --array=1-20
#SBATCH --cpus-per-task=4

#SBATCH --job-name="pk_pred"
#SBATCH --account="punim1449"

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --mem=10000

date

# Load required modules
module load r/4.0.0
module load gdal/3.0.2-python-3.7.4


date

time Rscript code_ruarai/R/produce_predictions.R ${SLURM_ARRAY_TASK_ID}
