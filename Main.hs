{-
Module for parsing the Turtell language,
Sean O'Kelly.

Contains functions which will parse elements of a program, up to an entire
program, and down to minor elements such as numbers. Built on top of Parsing
module. All parsers in this module ignore whitespace.
-}

module Main where

import System
import IO
import Monad
import Graphics.Gloss

import Turtle
import Lang

import Random

import Parsing

main :: IO ()
main = do {
    args <- getArgs;
    -- Case statement to check what has been passed in via args:
    case args of {
        
        [] -> do {
            -- There are no args, so exit with an error message because
            -- filename arg is needed.
            putStrLn "Error: You must pass in a filename.";
        };
        
        (filename:_) -> do {
            -- The filename has been passed in via args, try to read/parse it.
            putStrLn ("Reading " ++ filename ++ "...");
            catch (run_program filename) failure;
        } where {
            -- run_program: parses program (doesn't actually run program yet)
            run_program :: String -> IO ();
            run_program filename = do {
                text <- readFile filename;
                let {
                    parsed_program = parse program text;
                };
                case parsed_program of {
                    -- Parsers return definitions and an empty string if they
                    -- work correctly, anything else indicates failure.
                    [(definitions, [])] -> do {
                        putStrLn "Parsed program succesfully.";
                        runTurtle (runMain definitions);
                        return ();
                    };
                    _ -> do {
                        putStrLn "Error: could not parse program.";
                    };
                };
            };
            -- failure: if file read fails.
            failure :: IOError -> IO ();
            failure io_error = do {
                putStrLn "Error: Could not open file.";
            };
        };
    };
}




-- Parser for text of entire program
program :: Parser Definitions
program = do {
    -- A program is made up of one or more functions
    defs <- many1 (do {
        comments;
        function;
    });
    
    do {
        eof_comment;
        return ();
    } ||| do {
        comments;
        return ();
    };
    return defs;
}


-- Parser for a single function
function :: Parser (String, Function)
function = do {
    -- Get the name of the function and the list of arguments it takes in:
    name <- commented identifier;
    arguments <- commented (bracketed (argument_list identifier));
    
    commented (symbol "{");
    
    -- Function body is the lines of code in between the curly braces:
    body <- commented program_lines;
    
    commented (symbol "}");
    
    -- return tuple of (function name, function object)
    return (name, (Fn arguments body));
}


-- Parser for a list of arguments in a function definition
-- e.g. "( arg1, arg2, arg3 )"
argument_list :: Parser a -> Parser [a]
argument_list element_parser = do {
    
    args <- commented (do {
        -- An identifier, followed by a list of zero or more identifiers
        -- preceeded by commas:
        arg <- commented element_parser;
        args <- many (do {
            commented (symbol ",");
            name <- commented element_parser;
            return name;
        });
        return (arg:args)
    } ||| do {
        -- if the list has nothing in it but whitespace:
        space;
        return [];
    });
    
    return args;
}

list_of_size :: Int -> Parser a -> Parser [a]
list_of_size x element = do {
    xs <- commented (argument_list element);
    if (length xs) == x then return xs else failure;
};

bracketed :: Parser a -> Parser a
bracketed element = do {
    commented (symbol "(");
    e <- commented element;
    commented (symbol ")");
    return e;
}


-- Parser for function bodies
program_lines :: Parser Program
program_lines = do {
    -- A function may include zero or more 'lines' of code
    commands <- many (commented program_line);
    return (Seq commands);
}

-- parser for a single 'line' or instruction of code.
program_line :: Parser Program
program_line = do {
    -- A line is a turtle command or a function call, followed by a semicolon.
    do {
        commented turtle_command;
    } ||| do {
        commented print_statement;
    } ||| do {
        commented assignment;
    } ||| do {
        commented conditional_statement;
    } ||| do {
        commented while_loop;
    } ||| do {
        commented for_loop;
    } ||| do {
        commented function_call;
    };
}


print_statement :: Parser Program
print_statement = do {
    (_, expr) <- spaced (string "print") expression;
    commented (symbol ";");
    return (Print expr);
}   


assignment :: Parser Program
assignment = do {
    var_name <- commented identifier;
    commented (symbol "=");
    expression <- commented expression;
    commented (symbol ";");
    return (Assign var_name expression);
}


conditional_statement :: Parser Program
conditional_statement = do {
    commented (symbol "if");
    condition <- commented (bracketed expression);
    commented (symbol "{");
    
    if_body <- commented program_lines;
    
    commented (symbol "}");
    
    do {
        comments;
        string "else";
        else_body <- do {
            commented (symbol "{");
            else_body <- commented program_lines;
            commented (symbol "}");
            return else_body;
        } ||| do {
            do {
                comments1;
                return ();
            } ||| do {
                space1;
                return ();
            };
            commented conditional_statement;
        };
        return (If condition [if_body, else_body]);
    } ||| do {
        return (If condition [if_body]);
    };
}


while_loop :: Parser Program
while_loop = do {
    commented (symbol "while");
    condition <- commented (bracketed expression);
    commented (symbol "{");
    
    while_body <- commented program_lines;
    
    commented (symbol "}");
    
    return (While condition while_body);
}


for_loop :: Parser Program
for_loop = do {
    commented (symbol "for");
    
    (var_name, [from, to]) <- bracketed (do {
        var_name <- commented identifier;
        commented (symbol ",");
        range <- list_of_size 2 expression;
        return (var_name, range);
    });
    commented (symbol "{");
    
    for_body <- commented program_lines;
    
    commented (symbol "}");
    
    return (For var_name from to for_body);
}


-- Parser for a turtle command, e.g. "forward 50"
turtle_command :: Parser Program
turtle_command = do {
    -- A turtle command is a command (fd etc.) followed by a number:
    command <- token (do {
        do {
            param <- instruction forward expression;
            return (Fd param);
        } ||| do {
            param <- instruction left expression;
            return (Lt param);
        } ||| do {
            param <- instruction right expression;
            return (Rt param);
        } ||| do {
            (symbol "penUp") ||| (symbol "pu");
            return Up;
        } ||| do {
            (symbol "penDown") ||| (symbol "pd");
            return Down;
        } ||| do {
            param <- instruction change_colour colour;
            return (ChColour param);
        } ||| do {
            param <- instruction move coordinates;
            return (Mv param);
        };
    });
    commented (symbol ";");
    return (T command);
} where {
    instruction command element = do {
        (a, b) <- spaced command element;
        return b;
    };
    forward = (string "forward") ||| (string "fd");
    left = (string "left") ||| (string "lt");
    right = (string "right") ||| (string "rt");
    change_colour = (string "changeColour") ||| (string "chc");
    move = (string "move") ||| (string "mv");
}

spaced :: Parser a -> Parser b -> Parser (a, b)
spaced elem_1 elem_2 = do {
    comments;
    a <- elem_1;
    do {
        comments1;
        return ();
    } ||| do {
        space1;
        return ();
    };
    b <- commented elem_2;
    return (a, b);
}

coordinates :: Parser (Expr, Expr)
coordinates = do {
    do {
        [x, y] <- commented (list_of_size 2 expression);
        return (x, y);
    } ||| do {
        commented (bracketed coordinates);
    };
}

colour :: Parser Colour
colour = do {
    do {
        col_name <- commented identifier;
        case lookup col_name colours of {
            Just colour -> return (C colour);
            Nothing     -> failure;
        };
    } ||| do {
        rgb;
    };
}

colours :: [(String, Color)];
colours = [
        ("black", black),
        ("white", white),
        ("red", red),
        ("green", green),
        ("blue", blue),
        ("yellow", yellow),
        ("cyan", cyan),
        ("magenta", magenta),
        ("rose", rose),
        ("violet", violet),
        ("azure", azure),
        ("aquamarine", aquamarine),
        ("chartreuse", chartreuse),
        ("orange", orange)
    ]

rgb :: Parser Colour
rgb = do {
    do {
        [r, g, b] <- commented (list_of_size 3 expression);
        return (RGBA r g b (Val (Single 255)));
    } ||| do {
        commented (bracketed rgb);
    };
}


function_call :: Parser Program
function_call = do {
    name <- commented identifier;
    args <- commented (bracketed (argument_list expression));
    commented (symbol ";");
    return (Call name args);
}




-- Comment - end of line comment
comment :: Parser String
comment = do {
    space;
    string "//";
    text <- many not_newline;
    char '\n';
    space;
    return text;
}

multiline_comment :: Parser String
multiline_comment = do {
    symbol "/*";
    text <- multiline_comment_tail;
    return text;
}

multiline_comment_tail :: Parser String
multiline_comment_tail = do {
    do {
        symbol "*/";
        return [];
    } ||| do {
        x <- item;
        xs <- multiline_comment_tail;
        return (x:xs);
    };
}

-- eof_comment supports comments without a newline at the end of the file
eof_comment :: Parser String
eof_comment = do {
    do {
        space;
        string "//";
        text <- many not_newline;
        return text;
    } ||| do {
        multiline_comment;
    }
}

-- comments - multiple comments
comments :: Parser [String]
comments = do {
    many ((token comment) ||| multiline_comment);
}

-- comments - multiple comments
comments1 :: Parser [String]
comments1 = do {
    many1 ((token comment) ||| multiline_comment);
}

commented :: Parser a -> Parser a
commented item = do {
    comments;
    a <- item;
    comments;
    return a;
}


--Expression: can be mathematical, boolean, string etc.
--type checking is done in Lang.hs
--expression_ln = nth level of precedence
--higher n = higher precedence.
expression :: Parser Expr
expression = expression_l1

expression_l1 :: Parser Expr
expression_l1 = do {
    l2 <- commented expression_l2;
    do {
        l1 <- operation "||" expression_l1;
        return (Or l2 l1);
    } ||| do {
        return l2;
    };
}

expression_l2 :: Parser Expr
expression_l2 = do {
    l3 <- commented expression_l3;
    do {
        l2 <- operation "&&" expression_l2;
        return (And l3 l2);
    } ||| do {
        return l3;
    };
}

expression_l3 :: Parser Expr
expression_l3 = do {
    l4 <- commented expression_l4;
    do {
        l3 <- operation "==" expression_l3;
        return (Equal l4 l3);
    } ||| do {
        l3 <- operation "!=" expression_l3;
        return (NotEqual l4 l3);
    } ||| do {
        return l4;
    };
}

expression_l4 :: Parser Expr
expression_l4 = do {
    l5 <- commented expression_l5;
    do {
        l4 <- operation "<" expression_l4;
        return (Less l5 l4);
    } ||| do {
        l4 <- operation "<=" expression_l4;
        return (LessEq l5 l4);
    } ||| do {
        l4 <- operation ">" expression_l4;
        return (Greater l5 l4);
    } ||| do {
        l4 <- operation ">=" expression_l4;
        return (GreatEq l5 l4);
    } ||| do {
        return l5;
    };
}

-- parsers for addition, subtraction, multiplication of division have been
-- specially built to associate to left. This was based on ideas (not code) from:
--    http://eli.thegreenplace.net/2009/03/14/some-problems-of-recursive-descent-parsers/
expression_l5 :: Parser Expr
expression_l5 = do {
    -- Have to do one of the higher level expression,
    -- then any chain of "+/- higher level expression"
    e <- commented expression_l6;
    do {
        es <- many1 (do {
            e <- operation "+" expression_l6;
            return (e, Add);
        } ||| do {
            e <- operation "-" expression_l6;
            return (e, Sub);
        });
        -- use expr_list function to turn this list of expressions and
        -- operators into an Expr which associates to left.
        return (expr_list e es);
    } ||| do {
        return e;
    };
}

-- This is built to associate to left in the same way as +/- parser above.
expression_l6 :: Parser Expr
expression_l6 = do {
    e <- commented expression_l7;
    do {
        es <- many1 (do {
            e <- operation "*" expression_l7;
            return (e, Mul);
        } ||| do {
            e <- operation "/" expression_l7;
            return (e, Div);
        } ||| do {
            e <- operation "%" expression_l7;
            return (e, Mod);
        });
        return (expr_list e es);
    } ||| do {
        return e;
    };
}

-- Various prefix operators/operators which are supposed to associate to right:
expression_l7 :: Parser Expr
expression_l7 = do {
    do {
        l7 <- operation "!" expression_l7;
        return (Not l7);
    } ||| do {
        l7 <- operation "sin" expression_l7;
        return (Sin l7);
    } ||| do {
        l7 <- operation "cos" expression_l7;
        return (Cos l7);
    } ||| do {
        l7 <- operation "tan" expression_l7;
        return (Tan l7);
    } ||| do {
        l7 <- operation "arcsin" expression_l7;
        return (ArcSin l7);
    } ||| do {
        l7 <- operation "arccos" expression_l7;
        return (ArcCos l7);
    } ||| do {
        l7 <- operation "arctan" expression_l7;
        return (ArcTan l7);
    } ||| do {
        l7 <- operation "sqrt" expression_l7;
        return (Sqrt l7);
    } ||| do {
        l7 <- operation "round" expression_l7;
        return (Round l7);
    } ||| do {
        l7 <- operation "floor" expression_l7;
        return (Floor l7);
    } ||| do {
        l7 <- operation "rand" expression_l7;
        return (Random l7);
    } ||| do {
        l7 <- operation "toString" expression_l7;
        return (ToString l7);
    } ||| do {
        l7 <- operation "length" expression_l7;
        return (StringLength l7);
    } ||| do {
        -- String index access/string slicing
        l8 <- commented expression_l8;
        do {
            commented (symbol "[");
            e1 <- commented expression;
            string_operation <- do {
                e2 <- operation ":" expression;
                return (StringRange l8 e1 e2);
            } ||| do {
                return (StringElem l8 e1);
            };
            commented (symbol "]");
            return string_operation;
        };
    } ||| do {
        l8 <- commented expression_l8;
        do {
            l7 <- operation "^" expression_l7;
            return (Power l8 l7);
        } ||| do {
            return l8;
        };
    };
}

expression_l8 :: Parser Expr
expression_l8 = do {
    do {
        -- Allow chains of +/- before expressions, because why not?
        l8 <- operation "+" expression_l8;
        return l8;
    } ||| do {
        l8 <- operation "-" expression_l8;
        return (Sub (Val (Single 0)) l8);
    } ||| do {
        commented (symbol "pi");
        return Pi;
    } ||| do {
        commented (symbol "true");
        return (Val (Boolean True));
    } ||| do {
        commented (symbol "false");
        return (Val (Boolean False));
    } ||| do {
        num <- commented number;
        return (Val num);
    } ||| do {
        -- A string literal can be an expression (type checking done in interp)
        text <- string_text;
        return (Val (Yarn text));
    } ||| do {
        -- A variable name
        name <- commented identifier;
        return (Var name);
    } ||| do {
        -- An expression in brackets
        commented (bracketed expression);
    };
}

-- This code is to turn a list of expressions (with their operator) into a
-- single expression, left associative.
expr_list :: Expr -> [(Expr, Expr -> Expr -> Expr)] -> Expr
expr_list first rest
    -- foldr could probably be used here, but is unnecessary
    | null rest = first
    | otherwise = expr_list ((snd (head rest)) first (fst (head rest))) (tail rest)

-- Helper parser: parses an operator then another parser
operation :: String -> Parser a -> Parser a
operation operator expr_type = do {
    commented (symbol operator);
    commented expr_type;
}

string_text :: Parser String
string_text = do {
    comments;
    char '"';
    text <- string_tail;
    let {
        formatted_text = (read ("\"" ++ text ++ "\"")) :: String;
    };
    comments;
    return formatted_text;
}

string_tail :: Parser String
string_tail = do {
    do {
        char '"';
        return [];
    } ||| do {
        string "\\\"";
        xs <- string_tail;
        return ("\\\"" ++ xs);
    } ||| do {
        x <- not_newline;
        xs <- string_tail;
        return (x:xs);
    };
}
    

-- Reads a floating point number - ignores space around number
number :: Parser Value
number = do {
    -- token so that it ignores whitespace:
    float_value <- token (do {
        num_string <- do {
            -- Float is made up of three components:
            -- positive or negative int, point, positive int
            whole <- int;
            char '.';
            fraction <- nat;
            -- Turn the components of the number into a string:
            return ((show whole) ++ "." ++ (show fraction));
        };
        -- Use read to turn the string (e.g. "3.14159") into a float.
        return (Decimal (read num_string));
    } ||| do {
        -- Also allow integers without decimal points (but convert to float)
        number <- integer;
        return (Single number);
    });
    return float_value;
}