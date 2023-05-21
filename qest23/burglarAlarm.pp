fn main() {
	 earthquake ~ bern(0.0001);
	 burglary ~ bern(0.001);
	 alarm := earthquake == 1 || burglary == 1;
	 phoneWorking := 0;
	 if (earthquake == 1) {
	 		phoneWorking ~ bern(0.7);
   } else {
	 	 	phoneWorking ~ bern(0.99);
	 }

	 maryWakes := 0;
	 if (alarm) {
	 		if (earthquake == 1) {
				 maryWakes ~ bern(0.8);
      } else {
			 	 maryWakes ~ bern(0.6);
      }
   } else {
     maryWakes ~ bern(0.2);
   }

   called := (maryWakes == 1) && (phoneWorking == 1);
	 observe(called);
}
