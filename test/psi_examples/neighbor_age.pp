fn main() {
	 isGirl1 ~ bern(0.5);
	 age1 ~ uniform(1,30);
	 isGirl2 ~ bern(0.5);
	 age2 ~ uniform(1,30);
	 if (age1 > age2) {
	   observe(isGirl1 == 1);
	 } else {
     observe(isGirl2 == 1);
   }
	 bothAreGirls := (isGirl1 == 1) && (isGirl2 == 1);
}
