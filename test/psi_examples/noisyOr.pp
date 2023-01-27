fn main(){
    n0 ~ bern(1/2);
    n4 ~ bern(1/2);
		n1 :=0;
		n21 := 0;
		n4 := 0;
		n22 := 0;
		n33 := 0;
		n31 := 0;
		n32 := 0;
    if (n0 == 1) {
        n1 ~ bern(4/5);
        n21 ~ bern(4/5);
    } else {
        n1 ~ bern(1/10);
        n21 ~ bern(1/10);
    }
    if (n4 == 1) {
        n22 ~ bern(4/5);
        n33 ~ bern(4/5);
    } else {
        n22 ~ bern(1/10);
        n33 ~ bern(1/10);
    }
    n2 := (n21 == 1) || (n22 == 1); 
    if (n1 == 1) {
        n31 ~ bern(4/5);
    } else {
        n31 ~ bern(1/10);
    }
    if (n2) {
        n32 ~ bern(4/5);
    } else {
        n32 ~ bern(1/10);
    }
    n3 := (n31 == 1) || (n32 == 1) || (n33 == 1);
}
