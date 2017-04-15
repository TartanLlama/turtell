/*
Circles in Turtell
Sean O'Kelly, 2010-12-06
*/

main()
{
    circle(300, 24, 4);
}

circle(radius, iterations, recursions)
{
    if(recursions > 0)
    {
        divider = "\n==========\n";
        
        // Choose a colour:
        if(recursions == 4) {
            changeColour red;
        } else if(recursions == 3) {
            changeColour green;
        } else if(recursions == 2) {
            changeColour blue;
        } else if(recursions == 1) {
            changeColour white;
        }
        
        // circumference:
        circ = 2 * radius * pi;
        
        angle_change = 360 / iterations;
        dist = circ / iterations;
        
        angle_counter = 60;
        // For loop draws a circle:
        for(i, 0, iterations - 1)
        {
            /*
            Debug print statements which have outlived their usefulness:
            These only serve to slow the program down.
            
            print divider;
            print "Recursions: " + recursions;
            print "Current angle: " + angle_counter;
            */
            
            // if angle counter has reached a certain amount, draw a mini
            // circle within this one:
            if(angle_counter >= 60)
            {
                // Be sure to take one from recursions, so we don't have
                // infinite recursion:
                circle((radius / 3), iterations, (recursions - 1));
                angle_counter = 0;
            }
            
            // Draw this segment of the circle:
            forward dist;
            right angle_change;
            angle_counter = angle_counter + angle_change;
        }
        
        // Change colour back to what it was before the current recursion.
        if(recursions == 4) {
            changeColour white;
        } else if(recursions == 3) {
            changeColour red;
        } else if(recursions == 2) {
            changeColour green;
        } else if(recursions == 1) {
            changeColour blue;
        }
    }
}