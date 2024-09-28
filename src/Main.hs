module Main where

{-
    Builtin imports
-}
import Parsing.Parser
import Lang.AST
import Lang.Parser

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
