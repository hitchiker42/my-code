#!/bin/bash
./kDv
touch parallel.time
:>parallel.time
for i in `seq 1 8`;do
  OMP_NUM_THREADS=$i
  echo "OMP_NUM_THREADS = $OMP_NUM_THREADS">>parallel.time
  time ./kDv >>parallel.time
done
