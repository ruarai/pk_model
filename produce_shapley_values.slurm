#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-00:40:00
#SBATCH --array=1-1
#SBATCH --cpus-per-task=5

#SBATCH --job-name="pk_pred"
#SBATCH --account="punim1449"

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --mem=128G

date

if [ $# -eq 0 ]
  then
    echo "No model run name supplied."
    exit 1
fi

# Load required modules
module load r/4.0.0


date

time Rscript code_ruarai/R/produce_shapley_values.R ${SLURM_ARRAY_TASK_ID} $1 ${SLURM_CPUS_PER_TASK}
