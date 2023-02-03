#!/usr/bin/bash


log_suffix=".log"
prob_suffix="-prob"

for example in *.pp
do
		echo "Running $example..."
		/usr/bin/time -p symProb "$example" > "${example%.*}"$log_suffix 2>&1
		echo "Finished running $example..."

		echo "Running $example with probabilities..."
		/usr/bin/time -p symProb -p "$example" > "${example%.*}"$prob_suffix$log_suffix 2>&1
		echo "Finished running $example with probabilities..."
done
