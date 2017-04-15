main(){
    a = 3;
    b = 3.0;

    print "This is a: "+a;
    print "This is b: "+b;
    
    print "This is a+b: "+(a+b);
    print "This is toString(a)+b: "+(toString(a)+b);

    print "This is the floor of b "+floor(b);
    print "This is 2.123151 rounded "+round(2.123151);

    print "This is one string covering multiple lines!\nIsn't it pretty!\n\n\n\n\n\nThis is still that same string. Lots of \\ns going on here.";

    print "The length of 'cake' is: " + length("cake");
    print "The third letter of 'cake' is: " + "cake"[2];
    print "The first two letters of 'cake' are: " + "cake"[0:2];
    
    print "This is an arbitrary number: " + rand 2;
}
