main(){
    gasket(7, 1024);
}

gasket(levels, size){
    for (i, 0, levels){
	gasketP(size, size/2^i);
    }
}

//this shows how for loops and recursion can be used within if statements
gasketP(size, min){
    if (size >= min){
	for(j, 0, 2){
	    gasketP(size/2, min);
	    fd size/2;
	    rt 120;
	}
    }    
}

	    
