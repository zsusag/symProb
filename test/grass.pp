fn main(){ 
    cloudy ~ bern(0.5);

		rain := 0;
		sprinkler := 0;
    if (cloudy == 1) {
        rain ~ bern(0.8);
        sprinkler ~ bern(0.1);
    } else {
        rain ~ bern(0.2);
        sprinkler ~ bern(0.5);
    }

    temp1 ~ bern(0.7);
    wetRoof := (temp1 == 1) && (rain == 1);
    temp2 ~ bern(0.9);
    temp3 ~ bern(0.9);
    wetGrass := ((temp2 == 1) && (rain == 1)) || ((temp3 == 1) && (sprinkler == 1));

    observe(wetGrass);
}
