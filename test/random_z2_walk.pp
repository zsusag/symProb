fn main() {
  u := 0;
	v := 0;
	x ~ bern(1/2);
	y ~ bern(1/2);
	u := u + (x-y);
	v := v + (x + y - 1);
	while (u != 0 || v != 0) {
		x ~ bern(1/2);
		y ~ bern(1/2);
		u := u + (x-y);
		v := v + (x + y - 1);
  }
}
