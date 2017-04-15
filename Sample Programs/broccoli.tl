main(){
    chc (0, 0.6, 0); //a shade of dark-ish green
    broccoli(50, 2);
}

broccoli(x, y){
    if (x >= y){ //A nice standard if statement
	square(x);
	fd x;
	lt 45;

	broccoli (x/sqrt(2), y); //here we have recursion and the sqrt function
	
	pu;
	rt 90;
	fd (x/sqrt(2));
	pd;

	broccoli (x/sqrt(2), y);

	pu;
	lt 180;
	fd (x/sqrt(2));
	lt 180;
	lt 45;
	pd;
	lt 180;
	fd x;
	lt 180;
    }
}

square(size){
    for (i, 0, 3){ //note that for loops evaluate again when the counter is equal to the upper bound
	fd size;
	rt 90;
    } 
}





    
	
