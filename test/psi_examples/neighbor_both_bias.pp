fn main(){
    bias := 4 / 5;
    isGirl1 ~ bern(1 / 2);
    isGirl2 ~ bern(1 / 2);
		cond ~ bern(bias);
		isGirlAnnounce := 0;
    if (cond == 1) {
			 tmp ~ bern(1/2);
			 if (tmp == 1) {
			 		isGirlAnnounce := isGirl1;
			 } else {	
          isGirlAnnounce := isGirl2;
			 }
    } else {
			if ((isGirl1 == 1) || (isGirl2 == 1)) {
			  isGirlAnnounce := 1;
			} else {
			  isGirlAnnounce := 0;
			}
    }
    observe(isGirlAnnounce == 1);
    bothAreGirls := (isGirl1 == 1) && (isGirl2 == 1);
}
