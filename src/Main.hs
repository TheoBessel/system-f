module Main where

{-
    External imports
-}
import Data.Char
import Control.Applicative

{-
    Builtin imports
-}
import Lang.AST
import Lang.Parser

{-
    Lambda calculus
-}
identifier :: Parser String
identifier = check isLower
    >>= \c -> many (check isAlpha)
    >>= \s -> return (c:s)

simpleTerm :: Parser Term
simpleTerm = func <|> var <|> parens term

term :: Parser Term
term = app <|> simpleTerm <|> parens simpleTerm

var :: Parser Term
var = identifier >>= \i -> return $ Var i

func :: Parser Term
func = string "\\"
    >> identifier
    >>= \p -> string ":"
    >> typeTerm
    >>= \t -> string "."
    >> term >>= \b -> return Func {
        param = p,
        typ = t,
        body = b
    }

app :: Parser Term
app = simpleTerm
    >>= \f -> (
        string " "
        >> term
        >>= \a -> return App {
            fun=f,
            arg=a
        }
    ) <|> return f

{-
    Typed lambda calculus
-}
typeIdentifier :: Parser String
typeIdentifier = check isUpper
    >>= \c -> many (check isAlpha)
    >>= \s -> return (c:s)

typeVar :: Parser Type
typeVar = typeIdentifier >>= \i -> return $ TVar i

typeSimpleTerm :: Parser Type
typeSimpleTerm = typeVar <|> parens typeTerm

typeTerm :: Parser Type
typeTerm = typeFunc <|> typeSimpleTerm

typeFunc :: Parser Type
typeFunc = typeSimpleTerm
    >>= \p -> (
        string "->"
        >> typeTerm
        >>= \b -> return TFunc {
            tparam = p,
            tbody = b
        }
    ) <|> return p

{-
    Main function
-}
main :: IO ()
main = do
    printType "\n" $ eval typeTerm "Foo"
    printType "\n" $ eval typeTerm "Foo->(Bar->Baz)->Dummy->Bar"
    printTerm "\n" $ eval term "\\x:Foo->(Bar->Baz).x"
    printTerm "\n" $ eval term "\\f:T->T.\\y:T.x"
    --printTerm "\n" $ eval term "\\g:S->(T->T)->U.\\y:g y"
