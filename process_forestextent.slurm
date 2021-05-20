#!/bin/bash

#SBATCH --partition=snowy


#SBATCH --ntasks=1
#SBATCH --time=4-00:00:00
#SBATCH --cpus-per-task=4

#SBATCH --output=slurm_out/%x.%A_%a.out
#SBATCH --error=slurm_out/%x.%A_%a.error

#SBATCH --job-name="pk_f_ext"
#SBATCH --account="punim1449"

#SBATCH --mem=128G

date

# Load required modules
module load gcc/8.3.0
module load gdal/3.0.2-python-3.7.4

date

gdalinfo --version

for i in {1..54}
do
    time python code/Python/forestextent.py $i ${SLURM_CPUS_PER_TASK}
done

