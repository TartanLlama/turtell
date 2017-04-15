
/*
Test program, which builds on the sample code provided.

This serves as a test of various language features. If it works properly, it
should draw two squares, one large and blue, the other small and red.
*/

// Draws a line of length arg_1, then turns right by arg_2:
test(arg_1, // The length of the line
     arg_2) // The angle to turn
{
    // Makes use of different synonyms for turtle commands:
    forward (arg_1 / 2);
    fd (arg_1 / 2);
    left arg_2;
    lt arg_2;
    right // going right by the following amount:
    2*arg_2;
    rt arg_2;
}

// Square function creates a square, sides of length x:
square(x)
{
    // Calls test 4 times
    test(x, 90);
    test(x, 90); // Empty comment on the line below:
    test(x, 90); //
    test(x, 90);
}

main() // This is a test of a comment in a random place.
{
    changeColour 0, 0, 255;
    square(5 +      // 5
           (5 + 5)
            * 10);
    // 5 + (5 + 5) * 10 = 105
    
    penUp; // This is a test of end of line comments... Fingers crossed!
    move (-100, -100);
    changeColour red;
    penDown;
    square(20);
}


// End of file comment without a newline after it