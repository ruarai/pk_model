#!/bin/bash

#SBATCH --partition=snowy

#SBATCH --nodes=1

#SBATCH --job-name="pk_join"

#SBATCH --account="punim1449"

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2

#SBATCH --mem=24G

#SBATCH --time=0-1:00:00


date

if [ $# -eq 0 ]
  then
    echo "No model run name supplied."
    exit 1
fi

module load r/4.0.0
module load gdal/3.0.2-python-3.7.4


time Rscript code_ruarai/R/join_predictions.R $1
