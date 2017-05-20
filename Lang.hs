module Lang where

import Turtle
import Graphics.Gloss
import Debug.Trace
import System.Random
import Data.Char

-- Expression - parser doesn't care about type, so type checking needs to be done in interp.
data Expr = Var String

          | Val Value
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | ArcSin Expr
          | ArcCos Expr
          | ArcTan Expr
          | Sqrt Expr
          | Power Expr Expr
          | Pi
          | Round Expr
          | Floor Expr
          | Random Expr

          | Greater Expr Expr
          | Less Expr Expr
          | Equal Expr Expr
          | NotEqual Expr Expr
          | GreatEq Expr Expr
          | LessEq Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr

          | ToString Expr
          | StringElem Expr Expr
          | StringRange Expr Expr Expr
          | StringLength Expr

    deriving Show

data Value = Decimal Float
           | Single Int
           | Yarn String
           | Boolean Bool
             deriving Show

--Turtle command is a data structure which the turtle takes in
data TurtleCmd = Fd Expr
               | Rt Expr
               | Lt Expr
               | Up
               | Down
               | ChColour Colour
               | Mv (Expr, Expr)
  deriving Show

-- Either a Color object or expressions for R, G, B and A.
data Colour = C Color
            | RGBA Expr Expr Expr Expr
  deriving Show

-- Program data structure - can either be a list of programs,
-- a turtle command or a function call
data Program = T TurtleCmd
             | Assign String Expr
             | Call String [Expr]
             | Seq [Program]
             | Print Expr
             | For String Expr Expr Program
             | If Expr [Program] -- Two programs if there is an else clause
             | While Expr Program
               deriving Show

-- function data structure: a list of parameters and a definition
-- (the function body)
data Function = Fn { arguments :: [String],
                     definition :: Program }
   deriving Show

-- definitions: A list of tuples: (function name, function object)
type Definitions = [(String, Function)]

-- A set of variables - string of the name and float of the value
type Locals = [(String, Value)]


-- Make runMain create a list of functions, then call main. defs is test code.
runMain defs = interp defs [] (Call "main" [])

-- Evaluate a mathematical expression
eval :: Locals -> Expr -> Maybe Value
eval locs exp = case exp of
                  Sqrt e -> expCheck e (\x -> sqrt(x))
                  Sin e -> expCheck e (\x -> sin(x))
                  Cos e -> expCheck e (\x -> cos(x))
                  Tan e -> expCheck e (\x -> tan(x))
                  ArcSin e -> expCheck e (\x -> asin(x))
                  ArcCos e -> expCheck e (\x -> acos(x))
                  ArcTan e -> expCheck e (\x -> atan(x))
                  Add exp1 exp2 -> checkedAdd exp1 exp2
                  Sub exp1 exp2 -> doubleExpCheck exp1 exp2 (\x y -> x - y)
                  Mul exp1 exp2 -> doubleExpCheck exp1 exp2 (\x y -> x * y)
                  Div exp1 exp2 -> checkedDivide exp1 exp2
                  Power exp1 exp2 -> checkedPower exp1 exp2
                  Equal exp1 exp2 -> checkedEqual exp1 exp2
                  NotEqual exp1 exp2 -> checkedNotEq exp1 exp2
                  Greater exp1 exp2 -> checkedGreater exp1 exp2
                  GreatEq exp1 exp2 -> checkedGreatEq exp1 exp2
                  Less exp1 exp2 -> checkedLess exp1 exp2
                  LessEq exp1 exp2 -> checkedLessEq exp1 exp2
                  And exp1 exp2 -> checkedAndOr exp1 exp2 (\x y -> x && y)
                  Or exp1 exp2 -> checkedAndOr exp1 exp2 (\x y -> x || y)
                  Floor exp -> checkedFloor exp
                  Round exp -> checkedRound exp
                  ToString exp -> checkedToString exp
                  StringElem s i -> checkedStringElem s i
                  StringRange s i1 i2 -> checkedStringRange s i1 i2
                  StringLength s -> checkedStringLength s
                  Pi -> Just (Decimal pi)
                  Var s -> checkVar s
                  Val v -> Just v
                  Random e -> getRand e
                  Not e -> checkedNot e
                  _ -> Nothing
    where
      --start 080010347's code
      -- create an infinite list of PR floats
      randFloats :: Int -> [Float]
      randFloats seed = randoms (mkStdGen seed)

      -- returns a float from a list
      getFloat :: Int -> [Float] -> Float
      getFloat _ [] = 1.0
      getFloat index (x:xs) = getItem (x:xs) 0 index

      -- Recurse through list until current == n
      getItem :: [a] -> Int -> Int -> a
      getItem (x:xs) current index | current < index = getItem xs (current+1) index
	                                   | current == index = x

      -- returns a random Float
      rand :: Int -> Int -> Float
      rand seed index = getFloat index (randFloats seed)

      --end 080010347's code

      getRand e =
	  case (eval locs e) of
	    Just (Yarn str) -> Just (Decimal (rand (length str) (foldr (\x xs -> ord x + xs) 0 str)))
	    Just (Decimal f) -> Just (Decimal (rand (floor f) (length (show f))))
	    Just (Single i) -> Just (Decimal (rand i (length (show i))))
	    Just (Boolean b) -> if b then Just (Decimal (rand 0 0)) else Just (Decimal (rand 1 1))

      checkedStringLength s =
          case (eval locs s) of
            Just (Yarn str) -> Just (Single (length str))
            _ -> Nothing
      checkedStringRange s i1 i2 =
          case (eval locs s, eval locs i1, eval locs i2) of
            (Just (Yarn str), Just (Single int1), Just (Single int2)) -> Just (Yarn (fst (splitAt int2 (snd (splitAt int1 str)))))
            _ -> Nothing

      checkedStringElem s i =
          case (eval locs s, eval locs i) of
            (Just (Yarn str), Just (Single int)) -> safeElem str int 0
            _ -> Nothing

      safeElem "" i c = Nothing
      safeElem (x:xs) i c | c == i = Just (Yarn [x])
                          | otherwise = safeElem xs i (c+1)
      checkedToString e =
          case (eval locs e) of
            Just (Boolean b) -> Just (Yarn (show b))
            Just (Yarn y) -> Just (Yarn (y))
            Just (Single i) -> Just (Yarn (show i))
            Just (Decimal f) -> Just (Yarn (show f))
      checkedFloor e =
          case (eval locs e) of
            Just (Decimal f) -> Just (Single (floor f))
            Just (Single i) -> Just (Single i)
            _ -> Nothing
      checkedRound e =
          case (eval locs e) of
            Just (Decimal f) -> Just (Single (round f))
            Just (Single i) -> Just (Single i)
            _ -> Nothing

      checkVar s = case lookup s locs of
                   Just v -> Just v
                   Nothing -> Nothing
      expCheck e fun =
          case (eval locs e) of
            Just (Decimal f) -> Just (Decimal (fun f))
            Just (Single i) -> Just (Decimal (fun (fromIntegral i)))
            _ -> Nothing

      doubleExpCheck exp1 exp2 fun =
          case (eval locs exp1, eval locs exp2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Decimal (fun f1 f2))
            (Just (Decimal f), Just (Single i)) -> Just (Decimal (fun f (fromIntegral i)))
            (Just (Single i), Just (Decimal f)) -> Just (Decimal (fun (fromIntegral i) f))
            (Just (Single i1), Just (Single i2)) -> Just (Single (floor (fun (fromIntegral i1) (fromIntegral i2))))
            _ -> Nothing


      checkedAdd e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Yarn (s1 ++ s2))
            (Just (Yarn s), Just (Single a)) -> Just (Yarn (s ++ show a))
            (Just (Yarn s), Just (Decimal a)) -> Just (Yarn (s ++ show a))
            (Just (Yarn s), Just (Boolean a)) -> Just (Yarn (s ++ show a))
            (Just (Single a), Just (Yarn s)) -> Just (Yarn (show a ++ s))
            (Just (Decimal a), Just (Yarn s)) -> Just (Yarn (show a ++ s))
            (Just (Boolean a), Just (Yarn s)) -> Just (Yarn (show a ++ s))
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Decimal (f1 + f2))
            (Just (Single i), Just (Decimal f)) -> Just (Decimal (fromIntegral i + f))
            (Just (Decimal f), Just (Single i)) -> Just (Decimal (f + fromIntegral i))
            (Just (Single i1), Just (Single i2)) -> Just (Single (i1 + i2))
            _ -> Nothing

      checkedDivide e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Decimal (f1 / f2))
            (Just (Decimal f), Just (Single i)) -> Just (Decimal (f / fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Decimal (fromIntegral i / f))
            (Just (Single i1), Just (Single i2)) -> Just (Single (i1 `div` i2))
            _ -> Nothing

      checkedPower e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Decimal (f1 ** f2))
            (Just (Decimal f), Just (Single i)) -> Just (Decimal (f ^^ i))
            (Just (Single i), Just (Decimal f)) -> Just (Decimal (fromIntegral i ** f))
            (Just (Single i1), Just (Single i2)) -> Just (Single (i1 ^ i2))
            _ -> Nothing

      checkedEqual e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 == f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f == fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i == f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 == i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 == b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 == s2))
            _ -> Nothing

      checkedGreater e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 > f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f > fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i > f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 > i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 > b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 > s2))
            _ -> Nothing

      checkedGreatEq e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 >= f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f >= fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i >= f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 >= i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 >= b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 >= s2))
            _ -> Nothing

      checkedLess e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 < f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f < fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i < f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 < i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 < b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 < s2))
            _ -> Nothing

      checkedLessEq e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 <= f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f <= fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i <= f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 <= i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 <= b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 <= s2))
            _ -> Nothing

      checkedNotEq e1 e2 =
          case (eval locs e1, eval locs e2) of
            (Just (Decimal f1), Just (Decimal f2)) -> Just (Boolean (f1 /= f2))
            (Just (Decimal f), Just (Single i)) -> Just (Boolean (f /= fromIntegral i))
            (Just (Single i), Just (Decimal f)) -> Just (Boolean (fromIntegral i /= f))
            (Just (Single i1), Just (Single i2)) -> Just (Boolean (i1 /= i2))
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 /= b2))
            (Just (Yarn s1), Just (Yarn s2)) -> Just (Boolean (s1 /= s2))
            _ -> Nothing

      checkedAndOr e1 e2 fun =
          case (eval locs e1, eval locs e2) of
            (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (fun b1 b2))
            _ -> Nothing

      checkedNot e =
          case (eval locs e) of
            (Just (Boolean b)) -> Just (Boolean (not b))
            _ -> Nothing


-- We need more interp functions - this one handles one function call
-- also need to handle Seq [Program] which is a list of function calls and
-- turtle commands, and T TurtleCmd which is a single turtle command.
interp :: Definitions -> Locals -> Program -> Maybe Command
interp defs locs (Call f args)
    = case lookup f defs of
        Just (Fn pargs prog) -> safeZip pargs prog
        Nothing   -> Nothing
      where
        safeZip pargs prog =
            case safeMap args of
              Just l -> interp defs (zip pargs l) prog
              Nothing -> Nothing

        --I would have done unspeakable things for a while loop here
        safeMap [] = Just []
        safeMap (x:xs) =
            case (eval locs x) of
              Just f -> safeMap' f xs
              Nothing -> Nothing
        safeMap' f xs =
            case safeMap xs of
              Just l -> Just ([f] ++ l)
              Nothing -> Nothing

interp defs locs (Print exp) =
    case (eval locs exp) of
      Just (Yarn s) -> if trace s True then Just nothing else Just nothing
      Just (Boolean b) -> if trace (show b) True then Just nothing else Just nothing
      Just (Decimal f) -> if trace (show f) True then Just nothing else Just nothing
      Just (Single i) -> if trace (show i) True then Just nothing else Just nothing
      _ -> Nothing

interp defs locs (T (ChColour col))
    = case col of
        C colour -> Just (chColour colour)
        RGBA e0 e1 e2 e3 ->
             executeColourCh e0 e1 e2 e3
                 where
                     canonicalise (Just (Decimal e)) = e
                     canonicalise (Just (Single e))  = fromIntegral e

                     executeColourCh a b c d =
                         Just $ chColour
                              $ makeColor
                              (canonicalise $ ev a)
                              (canonicalise $ ev b)
                              (canonicalise $ ev c)
                              (canonicalise $ ev d)
                     ev exp = eval locs exp

interp defs locs (T cmd)
    = case cmd of
        Mv xs -> chkMove xs
        Rt arg -> chkArg arg (\x -> rt x)
        Fd arg -> chkArg arg (\x -> fd x)
        Lt arg -> chkArg arg (\x -> rt (-x))
        Up -> Just penUp
        Down -> Just penDown
      where
        chkArg arg fun =
            case (eval locs arg) of
              Just (Decimal f) -> Just (fun f)
              Just (Single i) -> Just (fun (fromIntegral i))
              _ -> Nothing
        chkMove (a1, a2)=
            case (eval locs a1, eval locs a2) of
              (Just (Decimal f1), Just (Decimal f2)) -> Just (mv (f1, f2))
              (Just (Decimal f), Just (Single i)) -> Just (mv (f, fromIntegral i))
              (Just (Single i), Just (Decimal f)) -> Just (mv (fromIntegral i, f))
              (Just (Single i1), Just (Single i2)) -> Just (mv (fromIntegral i1, fromIntegral i2))
              _ -> Nothing


interp defs locs (Seq (While e p:xs)) =
    case (eval locs e) of
      Just (Boolean b) -> if b then stringCmds p else singleInterp p
      _ -> Nothing
    where
      stringCmds (Seq progs) = interp defs locs (Seq (progs++[While e p]++xs))
      singleInterp (Seq progs) = interp defs locs (Seq (progs++xs))



interp defs locs (Seq (For v i1 i2 p:xs)) =
    case (eval locs i1, eval locs i2) of
      (Just (Single int1), Just (Single int2)) -> checkCont int1 int2
      _ -> Nothing
    where
      new_locs counter = Just (filter (\x -> fst x /= v) locs ++ [(v, counter)])

      checkLocs inc int1 =
          case (new_locs int1) of
            Just ls -> checkEval inc ls
            Nothing -> Nothing

      checkEval inc ls =
          case (eval ls i1) of
            Just (Single i) -> checkInterp inc ls i p
            _ -> Nothing

      checkInterp inc ls i (Seq progs) =
          interp defs ls (Seq (progs ++ [For v (Val (Single (i+inc))) i2 p]++xs))

      correctEquals int1 =
          case new_locs int1 of
            Just ls -> correctEquals' p ls
            Nothing -> Nothing

      correctEquals' (Seq pr) ls = interp defs ls (Seq (pr++xs))

      checkCont int1 int2
          | int1 > int2 = checkLocs (-1) (Single int1)
          | int1 == int2 = correctEquals (Single int1)
          | int1 < int2 = checkLocs 1 (Single int1)


interp defs locs (Seq (If e [p1, p2]:xs))
    = case (eval locs e) of
        Just (Boolean b) -> if b then passUp p1 else passUp p2
        _ -> Nothing
      where
        passUp (Seq pr) = interp defs locs (Seq (pr ++ xs))
        passUp another_if = interp defs locs (Seq ([another_if] ++ xs))


interp defs locs (Seq (If e [p]:xs))
    = case (eval locs e) of
        Just (Boolean b) -> if b then passUp p else interp defs locs (Seq xs)
        _ -> Nothing
      where
        passUp (Seq pr) = interp defs locs (Seq (pr ++ xs))


interp defs locs (Seq []) = Just nothing
interp defs locs (Seq (Assign v f:xs)) =
    case (eval locs f) of
      Just val -> interp defs (filter (\x -> fst x /= v) locs ++ [(v, val)]) (Seq xs)
      Nothing -> Nothing
interp defs locs (Seq (x:xs)) =
    case (interp defs locs x, interp defs locs (Seq xs)) of
      (Just c1, Just c2) -> Just (c1 +> c2)
      _ -> Nothing
