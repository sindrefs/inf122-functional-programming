-- Sindre Sørensen
module Oblig1 where
import Data.Char
import Data.List
data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String deriving (Show)

--Parsing different expected inputs
parseExpr :: String -> (Ast, String)
parseExpr (' ':xs) = parseExpr xs
parseExpr ('-' :xs) = let (ast, rest) = parseExpr xs
                          in (Min ast, rest)
parseExpr ('+':xs) = let (ast1, rest1) = parseExpr xs
                         (ast2, rest2) = parseExpr rest1
                         in (Sum ast1 ast2, rest2)
parseExpr ('*':xs) = let (ast1, rest1) = parseExpr xs
                         (ast2, rest2) = parseExpr rest1
                         in (Mul ast1 ast2, rest2)
parseExpr ('i':'f':xs) = let (ast1, rest1) = parseExpr xs
                             (ast2, rest2) = parseExpr rest1
                             (ast3, rest3) = parseExpr rest2
                             in (If ast1 ast2 ast3, rest3)
parseExpr ('l':'e':'t':xs) = let (string, rest1) = parseExpr xs
                                 ('=':rest1ny) = removeWs rest1
                                 (ast1, rest2) = parseExpr rest1ny
                                 ('i':'n':rest2ny) = removeWs rest2
                                 (ast2, rest3) = parseExpr rest2ny
                                 in (Let (parseLetter string) ast1 ast2, rest3)
parseExpr (x:xs) = if (isUpper x) then (Var [x], xs) else (Nr (read (takeWhile isDigit (x:xs)) :: Int), dropWhile isDigit xs)

--Remove whitespaces before chars
removeWs xs = dropWhile (==' ') xs

--Converts from Var String to plain String
parseLetter (Var string) = string

--Converts from input String to Ast
parse :: String -> Ast
parse xs = fst (parseExpr xs)

--Makes the calculations according to Ast
calc f sum mul min zero (Nr n ) (axs, bxs) = f (n)
calc f sum mul min zero (Sum a b) (axs, bxs) = sum (calc f sum mul min zero a (axs, bxs)) (calc f sum mul min zero b (axs, bxs))
calc f sum mul min zero (Mul a b) (axs, bxs) = mul (calc f sum mul min zero a (axs, bxs)) (calc f sum mul min zero b (axs, bxs))
calc f sum mul min zero (Min a) (axs, bxs) = (min (calc f sum mul min zero a (axs, bxs)))
calc f sum mul min zero (If a b c) (axs, bxs) = if (zero (calc f sum mul min zero a (axs, bxs)))
                                                  then (calc f sum mul min zero b (axs, bxs))
                                                  else (calc f sum mul min zero c (axs, bxs))
calc f sum mul min zero (Var a) (axs, bxs) = if elem a axs
                                                then let n = getMaybe (elemIndex a axs) in bxs !! n
                                                else error $ "Undeclared value for constant: " ++ a --If there is an udeclared variable it will be discovered, and printed here
calc f sum mul min zero (Let string ast1 ast2) (axs, bxs) = let value = calc f sum mul min zero ast1 (axs, bxs)
                                                            in calc f sum mul min zero ast2 ((string:axs), (value:bxs))
--Coverts (mainly) from Maybe Int to Int, but alo general types
getMaybe :: Maybe a -> a
getMaybe Nothing = error "Can't convert"
getMaybe (Just a) = a

--Function that does nothing, equvivalent with ie. \l->,l / id
nothing a = a
isZero a = if (a == 0) then True else False

--Evaluate input to Int
evi :: String -> Int
evi str = calc (nothing) (+) (*) (negate) (isZero) (parse str) ([], [])

--Evaluate input to Bool
evb :: String -> Bool
evb str = calc (odd) (||) (&&) (not) (nothing)  (parse str) ([], [])
