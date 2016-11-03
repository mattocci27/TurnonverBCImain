for MODEL in model1 model2 model3 model4
  do
  for PLOT in plot20m plot100m
  do
  echo "${MODEL}"
  echo "${PLOT}"
  export MODEL
  export PLOT
  #
  sbatch --job-name=${MODEL}_${PLOT}\
  --output=${MODEL}_${PLOT}.out\
  --error=${MODEL}_${PLOT}.err\
  moge2.sbatch
  #
  sleep 10 # pause to be kind to the scheduler
  done
  sleep 10
done



for MODEL in model1 model2 model3 model4
  do
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=${MODEL}_plot20m\
  --output=${MODEL}_plot20m.out\
  --error=${MODEL}_plot20m.err\
  model.sbatch
  #
  sleep 10 # pause to be kind to the scheduler
done

for MODEL in model1 model2 model3 model4
  do
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=${MODEL}_plot20m_simple\
  --output=${MODEL}_plot20m_simple.out\
  --error=${MODEL}_plot20m_simple.err\
  model20_simple.sbatch
  #
  sleep 10 # pause to be kind to the scheduler
done


for MODEL in model1 model2 model3 model4
  do
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=${MODEL}_plot100m\
  --output=${MODEL}_plot100m.out\
  --error=${MODEL}_plot100m.err\
  moge.sbatch
  #
  sleep 10 # pause to be kind to the scheduler
done



for SIZE in plot20m plot100m
  do
  echo "${SIZE}"
  export SIZE
  #
  sbatch --job-name=${SIZE}\
  --output=${SIZE}.out\
  --error=${SIZE}.err\
  moge.sbatch
  #
  sleep 2 # pause to be kind to the scheduler
done
