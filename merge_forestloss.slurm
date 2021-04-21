#!/bin/bash

#SBATCH --partition=physical


#SBATCH --ntasks=1
#SBATCH --time=0-02:00:00
#SBATCH --cpus-per-task=1

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --job-name="pk_forestloss"
#SBATCH --account="punim1449"

#SBATCH --mem=128G

date

# Load required modules
module load gcc/8.3.0
module load gdal/3.0.2-python-3.7.4

date

gdalinfo --version

if [ $# -eq 0 ]
  then
    echo "No inputs supplied."
    exit 1
fi

time python code_ruarai/Python/forestloss_merge.py $1 $2
