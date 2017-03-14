#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=1   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=4000mb
#SBATCH --time=10:00:00
date;hostname;pwd

module load gcc/5.2.0 R

R --vanilla --slave --args ${SIZE}  < glm_cv.R > ${SIZE}.log

date
