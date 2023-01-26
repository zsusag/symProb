fn main() {
  firstCoin ~ bern(1/2);
	secondCoin ~ bern(1/2);
	bothHeads := (firstCoin != 0) && secondCoin != 0;
	observe(!bothHeads);
}
