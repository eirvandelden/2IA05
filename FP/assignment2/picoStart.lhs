Het is mogelijk tekst en code in een ``.lhs file'' aan te bieden aan het Haskell systeem.
Er zijn een paar regels waarmee rekening gehouden moet worden:

1. tekst en code moeten afgewisseld worden met tenminste een blanke regel.
2. code regels moeten beginnen met ``> '' en daarna gelden de gebruikelijke syntaxregels.

Als startdocument voor deelopdracht 2 zou deze file genomen kunnen worden. 


> module Pico where
> type Prog = ([Decl],[Stat])
> type Decl = (Name, Type)
> type Name = String

> data Type = N | S deriving (Show, Eq)

> -- waarden bij type N en S
> data Val  = N1 Integer | S1 String deriving (Show,Eq)

> data Stat = Assign Name Expr
>           | If Expr [Stat] [Stat]
>           | While Expr [Stat]
>           deriving (Show,Eq)

> data Expr = NCon Integer
>           | SCon String
>           | Var Name
>           | Bin Op Expr Expr
>           deriving (Show,Eq)

> data Op   = Plus | Min | Cat deriving (Show,Eq)

> type State = [(Name,Val)]

aan te vullen met opdrachten.



Haskell versie van het voorbeeld programma 

> pico1 = (dl,sl)
> dl = [("in",N), ("out", N), ("a", N), ("b", N), ("h", N)]
> sl = [ Assign "in" (NCon 11)
>      , Assign "a" (NCon 1)
>      , While (Var "in")
>          [ Assign "h" (Var "a")
>          , Assign "a" (Var "b")
>          , Assign "b" (Bin Plus (Var "h") (Var "b"))
>          , Assign "in" (Bin Min (Var "in") (NCon 1))
>          ]
>      , Assign "out" (Var "b")
>      ]
