//this was a very early program, doesn't really show anything particularly interesting off, but it looks pretty
triangle(x1, y1, x2, y2, x3, y3){
    pu;
    mv (x1, y1);
    pd;
    mv (x2, y2);
    mv (x3, y3);
    mv (x1, y1);
    if (x1 > 0){
	triangle(x1-5, y1-5, x2-5, y2-5, x3-5, y3-5);
    }
    
}

main(){
    triangle(200, 50, 300, 200, 100, 200);
}


