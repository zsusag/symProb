fn main(x:Real) {
	while (x != 0) {
		u ~ bern(1/2);
		x := x+2*(u-1/2);
  }
}
