fn main() {
   gunFound := true;
   aliceDunnit ~ bern(0.3);
   withGun := 0;
	 if (aliceDunnit == 1) {
 	   withGun ~ bern(0.03);
	 } else {
	   withGun ~ bern(0.8);
	}
  observe((withGun == 1) && gunFound);
  posterior := aliceDunnit;
}
