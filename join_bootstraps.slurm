#!/bin/bash

# Partition for the job:
#SBATCH --partition=snowy

#SBATCH --ntasks=1
#SBATCH --time=0-00:45:00
#SBATCH --cpus-per-task=2

#SBATCH --output=slurm_out/pk_join.%A.out
#SBATCH --error=slurm_out/pk_join.%A.error

#SBATCH --job-name="pk_join"
#SBATCH --account="punim1449"

#SBATCH --mem=65536

date

# Load required modules
module load r/4.0.0

time Rscript code_ruarai/R/join_bootstraps.R
