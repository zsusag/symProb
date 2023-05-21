fn main(){
	x := 0;
	throws := 0;
	i := 0;
	while (i < 20) {
		if (!(5 <= x && x < 6)) {
			x ~ uniform(0,6);

			observe(( 1 <= x && x < 2) || (3 <= x && x < 4) || (5 <= x && x < 6) );
			throws := throws + 1;
		} else {
		  x := x;
    }
		i := i + 1;
	}
}
