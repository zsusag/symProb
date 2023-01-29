fn main(){
	x := 0;
	throws := 0;
	i := 0;
	while (i < 20) {
		if (x != 6) {
			x ~ rnd();
      x := x * 6;

			observe(( x > 1 && x <= 2) || (x > 3 && x <= 4) && (x > 5) );
			throws := throws + 1;
		} else {
		  x := x;
    }
		i := i + 1;
	}
}
