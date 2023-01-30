#!/usr/bin/bash

log_suffix=".log"

for example in *.psi
do
		echo "Running $example..."
		/usr/bin/time -p psi "$example" > "${example}"$log_suffix 2>&1
		echo "Finished running $example..."
done
