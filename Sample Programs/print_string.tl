main(){
    string = "Hello World abcdefghijklmnopqrstuvwxyz";
    size = 20;

    processString(string, size);
}

processString(string, size){
    rt 90;
    for (i, 0, length(string)-1){
	processChar(string[i], size);
    }    
}

processChar(char, size){
    pu;
    fd size/2;
    pd;

    if (char == "A" || char == "a"){
	pythagoras_diff = sqrt((size^2)*2);
	lt 60; fd pythagoras_diff*0.75; rt 120; fd pythagoras_diff/2; rt 120; fd pythagoras_diff/2; rt 180; fd pythagoras_diff/2; rt 60; fd pythagoras_diff*0.25; lt 60;
    }

    else if (char == "B" || char == "b"){
	lt 90; fd size; rt 90; for (i , 0, 179){ fd (pi*size)/(360*2); rt 1; } rt 180; for (i , 0, 179){ fd (pi*size)/(360*2); rt 1; } rt 180; pu; fd size/2; pd;
	    
    }

    else if (char == "C" || char == "c"){
	pu; fd size/2; pd; rt 180; for (i , 0, 179){ fd (pi*size)/(360); rt 1; } rt 90; pu; fd size; lt 90; pd;
    }

    else if (char == "D" || char == "d"){
	lt 90; fd size; rt 90; for (i , 0, 179){ fd (pi*size)/(360); rt 1; } rt 180; pu; fd size/2; pd;
    }

    else if (char == "E" || char == "e"){
	fd size/2; lt 180; fd size/2; rt 90; fd size/2; rt 90; fd size/2; lt 180; fd size/2; rt 90; fd size/2; rt 90; fd size/2; pu; rt 90; fd size; lt 90; pd;
 
    }
	
    else if (char == "F" || char == "f"){
	lt 90; fd size/2; rt 90; fd size/2; lt 180; fd size/2; rt 90; fd size/2; rt 90; fd size/2; pu; rt 90; fd size; lt 90; pd;
    }

    else if (char == "G" || char == "g"){
	pu; fd size/2; pd; rt 180; for (i , 0, 179){ fd (pi*size)/(360); rt 1; } rt 90; pu; fd size; rt 180; pd; fd size/4; lt 90; fd size/8; lt 180; fd size/8; fd size/8; lt 180; fd size/8; lt 90; fd size/4; lt 90;
    }

    else if (char == "H" || char == "h"){
	lt 90; fd size; rt 180; fd size/2; lt 90; fd size/2; lt 90; fd size/2; rt 180; fd size; lt 90;
    }

    else if (char == "I" || char == "i"){
	fd size/4; lt 90; fd size; lt 90; fd size/4; rt 180; fd size/2; rt 180; fd size/4; lt 90; fd size; lt 90; fd size/4;
    }

    else if (char == "J" || char == "j"){
	for (i , 0, 89){ fd (pi*size)/(360); lt 1; } fd (size/2); lt 90; fd size/4; rt 180; fd size/2; rt 180; fd size/4; lt 90; pu; fd size; lt 90;
    }

    else if (char == "K" || char == "k"){
	pythagoras_diff = sqrt(((size/2)^2)*2);
	lt 90; fd size; rt 180; fd size/2; lt 135; fd pythagoras_diff; rt 180; fd pythagoras_diff; lt 90; fd pythagoras_diff; lt 45;     }

    else if (char == "L" || char == "l"){
	fd size/2; rt 180; fd size/2; rt 90; fd size; rt 180; fd size; lt 90; fd size/4;
    }

    else if (char == "M" || char == "m"){
	pythagoras_diff = sqrt(((size/2)^2)*2);
	lt 90; fd size; rt 135; fd pythagoras_diff; lt 90; fd pythagoras_diff; rt 135; fd size; lt 90;
    }
    
    else if (char == "N" || char == "n"){
	pythagoras_diff = sqrt(((size)^2)*2);
	lt 90; fd size; rt 135; fd pythagoras_diff; lt 135; fd size; lt 180; fd size; lt 90;
    }

    else if (char == "O" || char == "o"){
	 pu; fd size/2; pd; for (i , 0, 359){ fd (pi*size)/(360); lt 1; } pu; fd size/2;
    }

    else if (char == "P" || char == "p"){
	lt 90; fd (size*3)/4;  for (i , 0, 359){ fd (pi*size/2)/(360); rt 1; } lt 180; fd (size*3)/4; lt 90; pu; fd size/2; pd;
    }

    else if (char == "Q" || char == "q"){
	pu; fd size/2; pd; for (i , 0, 359){ fd (pi*size)/(360); lt 1; } for (i , 0, 44){ fd (pi*size)/(360); lt 1; }lt 90; fd size/4; rt 180; fd size/2; rt 180; fd size/4; lt 90; for (i , 0, 44){ fd (pi*size)/(360); rt 1; } rt 180; pu; fd size/2; pd;
    }

    else if (char == "R" || char == "r"){
	pythagoras_diff = sqrt(((size/2)^2)*2);
	lt 90; fd (size*3)/4; for (i , 0, 359){ fd (pi*size/2)/(360); rt 1; } rt 180; fd size/4; lt 45; fd pythagoras_diff; lt 45;
    }

    else if (char == "S" || char == "s"){
	fd size/2; for (i , 0, 179){ fd (pi*size)/(360*2); lt 1; } fd size/4; for (i , 0, 179){ fd (pi*size)/(360*2); rt 1; } fd size/2; rt 90; pu; fd size; lt 90; pd;
    }

    else if (char == "T" || char == "t"){
	pu; fd size/2; lt 90; pd; fd size; lt 90; fd size/2; lt 180; fd size; pu; rt 90; fd size; lt 90; pd;
    }

    else if (char == "U" || char == "u"){
	pu; fd size/4; pd; rt 180; for (i , 0, 89){ fd (pi*size)/(360*2); rt 1; } fd (size*3)/4; lt 180; pu; fd size; lt 90; fd size/4; pd; for (i , 0, 89){ fd (pi*size)/(360*2); lt 1; } fd (size*3)/4; lt 180; pu; fd size; lt 90; pd;
    }

    else if (char == "V" || char == "v"){
	pythagoras_diff = sqrt((size^2)+(size/3)^2);
	pu; fd size/2; pd; lt 112.5; fd pythagoras_diff; rt 180; fd pythagoras_diff; lt 135; fd pythagoras_diff; pu; rt 157.5; fd size; lt 90; pd;
    }

    else if (char == "W" || char == "w"){
	pythagoras_diff = sqrt((size^2)+(size/3)^2);
	pu; fd size/2; pd; lt 112.5; fd pythagoras_diff; rt 180; fd pythagoras_diff; lt 135; fd pythagoras_diff; rt 135; fd pythagoras_diff; lt 135; fd pythagoras_diff; pu; rt 157.5; fd size; lt 90; pd;
	
    }

    else if (char == "X" || char == "x"){
	pythagoras_diff = sqrt((size^2)*2);
	
	lt 45; fd pythagoras_diff; pu; rt 135; fd size; rt 135; pd; fd pythagoras_diff; pu; rt 135; fd size; rt 90; fd size; lt 90; pd;
    }

    else if (char == "Y" || char == "y"){
	pythagoras_diff = sqrt(((size/2)^2)*2);

	pu; fd size/2; lt 90; pd; fd size/2; lt 45; fd pythagoras_diff; rt 180; fd pythagoras_diff; lt 90; fd pythagoras_diff; rt 135; pu; fd size; lt 90; pd;
    }

    else if (char == "Z" || char == "z"){
	pythagoras_diff = sqrt((size^2)*2);

	lt 45; fd pythagoras_diff; lt 135; fd size; lt 90; pu; fd size; lt 90; pd; fd size;
    }
    else if (char == " "){
	pu;
	fd size;
	pd;
    }  
}
