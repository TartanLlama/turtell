//Another example of the power of recursion in our language, and also the uses of changing pen colour
main(){
    chc (0.4, 0.1, 0);
    tree(100);
}

tree(size){
    if (size >= 12){
	fd size;
	rt 25; tree(size*0.8);
	lt 25; tree(size*0.7);
	lt 25; tree(size*0.8);
	rt 25;
	rt 180;
	if (size < 30){
	    chc (0.2, 0.7, 0.1);
	}
	if (size >= 30){
	    chc (0.7, 0.2, 0.1);
	}
	fd size;
	rt 180;
    }
}
