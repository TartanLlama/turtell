/*
Circle in Turtell
Sean O'Kelly, 2010-12-06
*/

main()
{
    drawCircles(300);
}

drawCircles(radius)
{
    size_dif = radius / 8;
    
    changeColour red;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour orange;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour yellow;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour green;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour cyan;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour blue;
    circle(0, 0, radius);
    radius = radius - size_dif;
    
    changeColour violet;
    circle(0, 0, radius);
    radius = radius - size_dif;
}

circle(x, y, radius)
{
    pi = 3.14159;
    circ = 2 * radius * pi;
    angle_change = 1;
    num_increments = (360 / angle_change);
    increment = circ / num_increments;
    
    penUp;
    move (x - radius, y);
    penDown;
    
    for(i, 0, num_increments)
    {
        forward increment;
        right angle_change;
    }
}