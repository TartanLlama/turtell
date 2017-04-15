/*
Test for expressions
*/

main()
{
    // Float:
    my_float = pi * 9 ^ 2 + 3.5 + 2 * (9 + 9); // 293.969...
    // Boolean:
    my_bool = true && (my_float == 5 || my_float >= 10); // true
    // String:
    my_string = "Hello, " + "world!" + "\n"; // "Hello, world!\n"
    // Integer:
    my_int = 3 * 5 / 2 + round my_float; // 301
    print my_float;
    print my_bool;
    print my_string;
    print my_int;
}