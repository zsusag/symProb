fn main(){ 
    skillA ~ normal(100,10);
    skillB ~ normal(100,10);
    skillC ~ normal(100,10);

  
    perfA1 ~ normal(skillA,15);
    perfB1 ~ normal(skillB,15);
    observe(perfA1 > perfB1);

  
    perfB2 ~ normal(skillB,15);
    perfC2 ~ normal(skillC,15);
    observe(perfB2 > perfC2);

  
    perfA3 ~ normal(skillA,15);
    perfC3 ~ normal(skillC,15);
    observe(perfA3 > perfC3);
} 
