{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Data.Data
import Data.Char
import Data.List
import Debug.Trace


data Token = LPAR String | RPAR String | QUOTE String | TRUE String | FALSE String | NULL String | SYM String | NUM Int | LIST [Token] | DATA_LIST [Token] | FUNCTION ([Token]->[Token]) | UNKNOWN String | FOLLOW_LIST [Token] | DOTTED_LIST [Token] | FOLLOW_DOTTED_LIST [Token]

instance Show Token where
        show (LPAR str) = str
        show (RPAR str) = str
        show (TRUE str) = str
        show (FALSE str) = str
        show (NULL str) = "'()"
        show (SYM str) = str
        show (NUM str) = show str
        show (LIST (x:xs)) = "(" ++ show x ++ " " ++ show (FOLLOW_LIST xs) ++ ")"
        show (DATA_LIST (x:xs)) =  "(" ++ show x ++ " " ++ show (FOLLOW_LIST xs) ++ ")"
        show (DOTTED_LIST (x:xs)) =  "(" ++ show (x) ++ show (FOLLOW_DOTTED_LIST xs) ++ ")"
        show (FOLLOW_DOTTED_LIST (x:xs)) = " . " ++ show x ++ show (FOLLOW_DOTTED_LIST xs)
        show (FOLLOW_LIST (x:xs)) = show x ++ " " ++ show (FOLLOW_LIST xs)
        show (FOLLOW_LIST lst) = ""
        show (FOLLOW_DOTTED_LIST lst) = ""
        show (UNKNOWN str) = str
        show (QUOTE str) = str

addSymInList :: String -> String -> [String]
addSymInList [] toAdd = [toAdd]
addSymInList (x:xs) toAdd
        | isAlpha x || isDigit x || isValid x  = addSymInList xs (toAdd ++ [x])
        | otherwise = toAdd : myParser (x:xs)

myParser :: String -> [String]
myParser "" = []
myParser (x:xs)
        | x == '(' = "(" : myParser xs
        | x == ')' = ")" : myParser xs
        | x == '\'' = "'" : myParser xs
        | isAlpha x || isDigit x || isValid x = addSymInList (x:xs) ""
        | otherwise =  myParser xs

symbol :: [String]
symbol = ["cons", "quote", "car", "cdr", "list", "lambda", "define", "let", "eq?", "atom?", "cond", "+", "-", "div", "*"]

defined :: [(String, Token)]
defined = []

myIsNumber :: String -> Bool
myIsNumber str = all isDigit str

validChar :: String
validChar = "#\'!?+-*/"

isValid :: Char -> Bool
isValid char = elem char validChar

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace [] _ _ = []
replace str find repl =
    if take (length find) str == find
        then repl ++ (replace (drop (length find) str) find repl)
        else [head str] ++ (replace (tail str) find repl)


parser :: [Token] -> [Token]
parser [] = []
parser (QUOTE "'":LPAR "(":xs) = do let tuple = (convertList xs [])
                                    let first = fst tuple
                                    let second = snd tuple
                                    DATA_LIST first : parser second
parser (LPAR "(":xs) = do let tuple = (convertList xs [])
                          let first = fst tuple
                          let second = snd tuple
                          LIST first : parser second
parser (RPAR ")":xs) = parser xs
parser (QUOTE "'":x:xs) = (QUOTE (show x)) : parser xs
parser (x:xs) = x : parser xs

convertList :: [Token] -> [Token] -> ([Token], [Token])
convertList [] [] = ([], [])
convertList [] lst = (lst, [])
convertList (QUOTE "'":x:xs) [] = (convertList xs [QUOTE (show x)])
convertList (x:xs) [] = (convertList xs [x])
convertList (RPAR ")":xs) acc = (acc, xs)
convertList (QUOTE "'":LPAR "(": xs) acc = do let tuple = (convertList xs [])
                                              let first = fst tuple
                                              let second = snd tuple
                                              (acc ++ [DATA_LIST first] ++ fst (convertList second []), [])
convertList (LPAR "(":xs) acc = do let tuple = (convertList xs [])
                                   let first = fst tuple
                                   let second = snd tuple
                                   (acc ++ [LIST first] ++ parser second, [])
convertList (QUOTE "'":x:xs) acc = (convertList xs (acc ++ [QUOTE (show x)]))
convertList (x:xs) acc = (convertList xs (acc ++ [x]))

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize ("(":xs) = LPAR "(" : (tokenize xs)
tokenize (")":xs) = RPAR ")" : (tokenize xs)
tokenize ("#t":xs) = TRUE "#t" : (tokenize xs)
tokenize ("#f":xs) = FALSE "#f" : (tokenize xs)
tokenize ("nil":xs) = NULL "nil" : (tokenize xs)
tokenize ("'":xs) = QUOTE "'" : (tokenize xs)
tokenize (x:xs)
        | elem x symbol = (SYM x) : (tokenize xs)
        | myIsNumber x = (NUM ((read x) :: Int)) : (tokenize xs)
        | otherwise = (UNKNOWN x) : (tokenize xs)

epureString :: String -> String
epureString "" = ""
epureString str = replace str "'()" "nil"

env :: [(Token, ([Token] -> [Token]))]
env = [(SYM "+", add),
       (SYM "-", sub),
       (SYM "*", multiply),
       (SYM "div", division),
       (SYM "quote", quote),
       (SYM "cons", cons),
       (SYM "car", car),
       (SYM "cdr", cdr),
       (SYM "eq?", eq),
       (SYM "cond", cond)]

car :: [Token] -> [Token]
car (LIST x:xs) = car((eval x)) ++ xs
car (DATA_LIST x:xs) = x!!0 : xs
car (DOTTED_LIST x:xs) = x!!0 : xs

cdr :: [Token] -> [Token]
cdr (LIST x:xs) = cdr((eval x)) ++ xs
cdr (DATA_LIST x:xs) = tail x!!0 : xs
cdr (DOTTED_LIST x:xs) = tail x!!0 : xs

add :: [Token] -> [Token]
add [] = [NUM 0]
add (NUM x: LIST y:xs) = NUM (x + ((read (show ((eval y)!!0))) :: Int)) : xs
add (NUM x:NUM y:xs) = [NUM (x + y + ((read (show ((add xs)!!0))) :: Int))]
add (NUM x:xs) = [NUM (x + ((read (show ((add xs)!!0))) :: Int))]
add (LIST x: NUM y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) + y) : xs
add (LIST x: LIST y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) + ((read (show ((eval y)!!0))) :: Int)) : xs

sub :: [Token] -> [Token]
sub [] = [NUM 0]
sub (NUM x: LIST y:xs) = NUM (x - ((read (show ((eval y)!!0))) :: Int)) : xs
sub (NUM x:NUM y:xs) = [NUM (x - y - ((read (show ((sub xs)!!0))) :: Int))]
sub (NUM x:xs) = [NUM (x - ((read (show ((sub xs)!!0))) :: Int))]
sub (LIST x: NUM y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) - y) : xs
sub (LIST x: LIST y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) - ((read (show ((eval y)!!0))) :: Int)) : xs

multiply :: [Token] -> [Token]
multiply [] = [NUM 1]
multiply (NUM x: LIST y:xs) = NUM (x * ((read (show ((eval y)!!0))) :: Int)) : xs
multiply (NUM x:NUM y:xs) = [NUM (x * y * ((read (show ((multiply xs)!!0))) :: Int))]
multiply (NUM x:xs) = [NUM (x * ((read (show ((multiply xs)!!0))) :: Int))]
multiply (LIST x: NUM y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) * y) : xs
multiply (LIST x: LIST y:xs) = NUM (((read (show ((eval x)!!0))) :: Int) * ((read (show ((eval y)!!0))) :: Int)) : xs

division :: [Token] -> [Token]
division [] = [NUM 1]
division (NUM x: LIST y:xs) = NUM (div x ((read (show ((eval y)!!0))) :: Int)) : xs
division (NUM x:NUM y:xs) = [NUM (div (div x y) ((read (show ((division xs)!!0))) :: Int))]
division (NUM x:xs) = [NUM (div x ((read (show ((division xs)!!0))) :: Int))]
division (LIST x: NUM y:xs) = NUM (div ((read (show ((eval x)!!0))) :: Int) y) : xs
division (LIST x: LIST y:xs) = NUM (div ((read (show ((eval x)!!0))) :: Int) ((read (show ((eval y)!!0))) :: Int)) : xs

quote :: [Token] -> [Token]
quote (LIST x:xs) = DATA_LIST x : xs
quote (SYM x:xs) = QUOTE x : xs
quote (UNKNOWN x:xs) = QUOTE x : xs
quote (x:xs) = x : xs

cons :: [Token] -> [Token]
cons (LIST x:y:xs) = (DOTTED_LIST ((eval x) ++ [y])) : xs
cons (x:LIST y:xs) = (DOTTED_LIST (x : eval(y))) : xs
cons (x:y:xs) = (DOTTED_LIST (x : [y])) : xs

eq :: [Token] -> [Token]
eq [] = []
eq (LIST x: LIST y:xs) = if show ((eval x)!!0)  == show ((eval y)!!0) then TRUE "#t" : xs else FALSE "#f" : xs
eq (LIST x:y:xs) = if show ((eval x)!!0)  == (show y) then TRUE "#t" : xs else FALSE "#f" : xs
eq (x: LIST y:xs) = if (show x) == show ((eval y)!!0) then TRUE "#t" : xs else FALSE "#f" : xs
eq (x:y:xs) = if (show x) == (show y) then TRUE "#t" : xs else FALSE "#f" : xs

cond :: [Token] -> [Token]
cond [] = []
cond (LIST x:y:xs) = if (show ((eval (tail x))!!0)) == "#t" then [y] else (cond xs)
cond lst = trace ("in cond = " ++ show lst) (lst)

empty :: [Token] -> [Token]
empty [] = []

findEnv :: Token -> [(Token, ([Token] -> [Token]))] -> ([Token] -> [Token])
findEnv a [] = trace("error on " ++ show a) (empty)
findEnv x (y:ys) = if show (x) == show (fst (y)) then snd (y) else findEnv x ys

eval :: [Token] -> [Token]
eval [] = []
eval (LIST x:xs) = eval(x) ++ eval(xs)
eval (NUM x:xs) = [NUM x] ++ eval(xs)
eval (TRUE x:xs) = [TRUE x] ++ eval(xs)
eval (FALSE x:xs) = [FALSE x] ++ eval(xs)
eval (DATA_LIST x:xs) = DATA_LIST x : eval(xs)
eval (QUOTE x:xs) = [QUOTE x] ++ eval(xs)
eval (NULL x:xs) = [NULL x] ++ eval(xs)
eval (DOTTED_LIST x:xs) = DOTTED_LIST x : eval(xs)
eval (x:xs) = eval(((findEnv x env) xs))

epureEval :: [Token] -> Token
epureEval(x:xs) = x
{- case readMaybe x :: Maybe Int of
        Just n -> Int n
        Nothing -> error
-}