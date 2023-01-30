#!/usr/bin/bash


log_suffix=".log"
no_prob_suffix="-no_prob"

for example in *.pp
do
		echo "Running $example..."
		/usr/bin/time -p symprog "$example" > "${example%.*}"$log_suffix 2>&1
		echo "Finished running $example..."

		echo "Running $example with no probabilities..."
		/usr/bin/time -p symprog -n "$example" > "${example%.*}"$no_prob_suffix$log_suffix 2>&1
		echo "Finished running $example with no probabilities..."
done
