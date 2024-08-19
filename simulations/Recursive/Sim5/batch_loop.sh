#!/bin/bash

# Loop over a set of values
for i in {1..100}
do
  # Submit a batch job for each iteration
  sbatch --job-name=iter${i}_sim5 Sim5_batch.sh $i
done