#!/bin/bash

#
#PBS -r n

##Job settings
#PBS -N temp1
#PBS -j oe
#PBS -m aye
#PBS -M tz05@me.com

##Job configuration

##Job resources
#PBS -l nodes=1:ppn=20
#PBS -l walltime=60:00:00
#PBS -l pmem=10000mb

module load R
module load intel/2013.sp1.3.174
module load openmpi/1.6.5
module load Rmpi/3.2.0

R --vanilla  < /home/taozhang/Sources/temp/temp1.R
