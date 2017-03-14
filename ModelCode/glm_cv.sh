###
### shell script to run glm_cv.R
###

for SIZE in plot20m plot100m
do

    #
    echo "${SIZE}"
    export SIZE
    #
    sbatch --job-name=glmcv${SIZE}\
    --output=glmcv_${SIZE}.out\
    --error=glmcv_${SIZE}.err\
    --job-name=glmcv_${SIZE}\
    moge.sbatch
    #
  sleep 1 # pause to be kind to the scheduler
done
