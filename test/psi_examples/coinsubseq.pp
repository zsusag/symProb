fn main(){
	 z := 0;
	 k :=	 1;

	pos := 0;
	
	for i in [0..10){
		cur := flip(1/2);
		if pos==0{
			if cur==z{
				pos += 1;
			}
		}else if pos==1{
			if cur==k{
				pos += 1;
			}else{
				pos = 1;
			}
		}else if pos==2{
			if cur==k{
				pos += 1;
			}else{
				pos = 1;
			}
		}else if pos==3{
			if cur==z{
				return 1;
			}else{
				pos = 0;
			}
		}
	}
	return 0;
}
