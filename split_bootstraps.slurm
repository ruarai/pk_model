#!/bin/bash

# Partition for the job:
#SBATCH --partition=physical

#SBATCH --ntasks=1
#SBATCH --time=0-01:00:00
#SBATCH --cpus-per-task=2

#SBATCH --output=slurm_out/pk_split.%A.out
#SBATCH --error=slurm_out/pk_split.%A.error

#SBATCH --job-name="pk_split"
#SBATCH --account="punim1449"

#SBATCH --mem=65536

date

# Load required modules
module load r/4.0.0
module load gdal/3.0.2-python-3.7.4

time Rscript code_ruarai/R/split_bootstraps.R
