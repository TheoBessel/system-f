{-# LANGUAGE LambdaCase #-}

module Lang.Parser where

import Control.Applicative

-- Define Parser type
newtype Parser a = P (String -> [(a, String)])

{-
    Parsing and evaluation functions
-}
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

eval :: Show a => Parser a -> String -> a
eval s p = case parse s p of
    [(x,[])] -> x
    [(_,out)] -> error ("UnusedInput :" ++ out)
    x -> error ("InvalidInput :" ++ show x)

{-
    Parser type
-}
-- Define Parser type as an instance of Functor
instance Functor Parser where
    fmap f p =
        P ( \s -> case parse p s of
            [] -> []
            [(val, out)] -> [(f val, out)]
            _ -> error "ParsingError"
        )

-- Define Parser type as an instance of Applicative
instance Applicative Parser where
    pure val = P (\s -> [(val, s)])
    pf <*> px =
        P ( \s -> case parse pf s of
            [] -> []
            [(fval, out)] -> parse (fmap fval px) out
            _ -> error "ParsingError"
        )

-- Define Parser type as an instance of Monad
instance Monad Parser where
    p >>= f =
        P ( \s -> case parse p s of
            [] -> []
            [(val, out)] -> parse (f val) out
            _ -> error "ParsingError"
        )

-- Define Parser type as an instance of Alternative
instance Alternative Parser where
    empty = P (const [])
    many x = some x <|> pure []
    some x = (:) <$> x <*> many x
    p1 <|> p2 =
        P ( \s -> case parse p1 s of
            [] -> parse p2 s
            x -> x
        )

{-
    Useful parsers
-}
-- Consumes the first element of a string
item :: Parser Char
item =
    P ( \case
        [] -> []
        (h : t) -> [(h, t)]
    )

-- Check if the first element satifies a predicate p
check :: (Char -> Bool) -> Parser Char
check p = item
    >>= \x -> if p x
        then return x
        else empty

-- Check if the elements of a string are equal to a given string
string :: String -> Parser String
string [] = return []
string (h:t) = check (==h)
    >> string t
    >> return (h:t)

-- Parse one element
one :: Parser Char -> Parser String
one p = p >>= \x -> return [x]

-- Parse parenthesis
delimiters :: String -> String -> Parser a -> Parser a
delimiters l r p = string l >> p >>= \x -> string r >> return x

parens :: Parser a -> Parser a
parens = delimiters "(" ")"
