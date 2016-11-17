#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=nmds_20_hpc.out
#SBATCH --error=nmds_20_hpc.err
#SBATCH --cpus-per-task=1   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=20000mb
#SBATCH --time=60:00:00
date;hostname;pwd

module load gcc/5.2.0 R/3.2.2

Rscript nmds_20_hpc.R > nmds_20_hpc.log

date
