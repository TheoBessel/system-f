module Lang.AST where

{-
    Type definitions
-}
data Term =
    Var String
    | Func { param :: String, typ :: Type, body :: Term }
    | App { fun :: Term, arg :: Term }
    deriving (Show)

data Type =
    TVar String
    | TFunc { tparam :: Type, tbody :: Type }
    deriving (Show)

{-
    Pretty printers
-}
printType :: String -> Type -> IO ()
printType s (TVar x) = do putStr x; putStr s
printType s TFunc { tparam=p, tbody=b } = do
    printType "" p; putStr "->"; printType "" b;
    putStr s

printTerm :: String -> Term -> IO ()
printTerm s (Var x) = do putStr x; putStr s
printTerm s App { fun=f, arg=a } = do
    putStr "("; printTerm "" f; putStr " "; printTerm "" a; putStr ")"; putStr s
printTerm s Func { param=p, typ=t, body=b } = do
    putStr "\\"; putStr p; putStr ":"; printType "" t; putStr "."; printTerm "" b;
    putStr s
