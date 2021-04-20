#!/bin/bash

#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=0-01:00:00
#SBATCH --cpus-per-task=8

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --job-name="pk_forestloss"
#SBATCH --account="punim1449"

#SBATCH --mem=256G

date

# Load required modules
module load gcc/8.3.0
module load gdal/3.0.2-python-3.7.4

date

gdalinfo --version

time python code_ruarai/Python/forestextent.py 1

