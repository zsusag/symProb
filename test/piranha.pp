fn main() {
	 a ~ bern(1/2);
	 b := true;
	 cond ~ bern(1/2);
	 r := false;
	 if (cond == 1) {
     r := a == 1;
   } else {
     r := b;
	 }
	 observe(r);
}
