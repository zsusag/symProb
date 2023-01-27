fn main(){
	a ~ rnd();
	b ~ rnd();
	observe(a<=b);
	c ~ rnd();
	d ~ rnd();
	observe(c<=d);
	e ~ rnd();
	f ~ rnd();
	observe(e<=f);
	g ~ rnd();
	h ~ rnd();
	observe(g<=h);
}
