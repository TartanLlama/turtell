main(){
    square(-100, 100, 100, 100, 100, -100, -100, -100, 0);
}

square(x1, y1, x2, y2, x3, y3, x4, y4, iteration){
    pu;
    mv (x1, y1);
    pd;
    mv (x2, y2);
    mv (x3, y3);
    mv (x4, y4);
    mv (x1, y1);

    if (iteration < 20){
	//recursion!
	square(x1+10, y1, x2, y2-10, x3-10, y3, x4, y4+10, iteration + 1);
    }
}
