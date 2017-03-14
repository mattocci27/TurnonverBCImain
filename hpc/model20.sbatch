#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=6000mb
#SBATCH --time=100:00:00
date;hostname;pwd

module load gcc/5.2.0 R

R --vanilla --slave --args ${MODEL} plot20m < spatial_model.R

date
