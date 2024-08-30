fn main() {
  gender ~ bern(51 / 100);
	height := 0;
	if (gender == 1) {
    height ~ normal(175,72);
	} else {
    height ~ normal(161,50);
	}
	observe(height >= 180);
}
