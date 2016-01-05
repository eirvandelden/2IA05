module Pico where

import List
import Data.Set (Set)
import qualified Data.Set as Set

type Prog = ([Decl],[Stat])
type Decl = (Name, Type)
type Name = String

reservedWords = ["begin", "end", "declare", "natural", "string", "if", "then", "else", "fi", "while", "do", "od"]

data Type = N | S deriving (Show, Eq)

-- waarden bij type N en S
data Val  = N1 Integer | S1 String deriving (Show,Eq)

data Stat = Assign Name Expr
         | If Expr [Stat] [Stat]
         | While Expr [Stat]
        deriving (Show,Eq)

data Expr = NCon Integer
          | SCon String
          | Var Name
          | Bin Op Expr Expr
         deriving (Show,Eq)

data Op   = Plus | Min | Cat deriving (Show,Eq)

type State = [(Name,Val)]

-- Haskell versie voor het voorbeeldprogramma
pico1 = (dl,sl)
dl = [("in",N), ("out", N), ("a", N), ("b", N), ("h", N)]
sl = [ Assign "in" (NCon 11)
       , Assign "a" (NCon 1)
       , While (Var "in")
           [ Assign "h" (Var "a")
           , Assign "a" (Var "b")
           , Assign "b" (Bin Plus (Var "h") (Var "b"))
           , Assign "in" (Bin Min (Var "in") (NCon 1))
           ]
       , Assign "out" (Var "b")
       ]

----------------------------------------------
-- 
-- Section 1
--
----------------------------------------------
-- OPDRACHT 3
opdracht3 = (dlo3,slo3)
dlo3 = [("a",N), ("b", N), ("out", N)]
slo3 = [ Assign "a" (NCon 13)
     , Assign "b" (NCon 37)
     , While (Var "b")
        [ Assign "out" (Bin Plus (Var "out") (Var "a"))
        , Assign "b" (Bin Min (Var "b") (NCon 1))
        ]
     ]
     
-- OPDRACHT 5
pico2 = (dl2,sl2)
dl2 = [("a",N), ("b", N), ("out", N), ("tmp", N)]
sl2 = [ Assign "a" (NCon 5)
     , Assign "b" (Bin Min (Var "a") (NCon 1))
     , Assign "tmp" (Var "a")
     , Assign "out" (Var "a")
     , While (Bin Min (Var "a") (NCon 1))
        [ While (Bin Min (Var "b") (NCon 1))
            [ Assign "out" (Bin Plus (Var "out") (Var "tmp"))
            , Assign "b" (Bin Min (Var "b") (NCon 1))
            ]
            , Assign "tmp" (Var "out")
            , Assign "a" (Bin Min (Var "a") (NCon 1))
            , Assign "b" (Bin Min (Var "a") (NCon 1))
        ]
     ]

-- OPDRACHT 7

pico3 = (dl3,sl3)
dl3 = [("a",N), ("b", N), ("temp_a", N), ("temp_b", N), ("out", N), ("a_greater_than_b_or_equal", N)]
sl3 = [ Assign "a" (NCon 1311)
     , Assign "b" (NCon 23)
     , Assign "out" (NCon 0)
     , Assign "a_greater_than_b_or_equal" (NCon 1)
     , Assign "temp_a" (NCon 0)
     , Assign "temp_b" (NCon 0)
     , While (Bin Min (Var "b") (Var "temp_b"))
        [ Assign "temp_a" (Bin Plus (Var "temp_a") (NCon 1))
        , Assign "temp_b" (Bin Plus (Var "temp_b") (NCon 1))
        , If (Bin Min (Bin Plus (Var "a") (NCon 1)) (Var "temp_a"))
            [ Assign "a" (Var "a")]
            [ Assign "a_greater_than_b_or_equal" (NCon 0)]
        ]
			, While (Var "a_greater_than_b_or_equal")
			        [ Assign "a" (Bin Min (Var "a") (Var "b"))
			        , Assign "out" (Bin Plus (Var "out") (NCon 1))
			        , Assign "a_greater_than_b_or_equal" (NCon 1)
			        , Assign "temp_a" (NCon 0)
			        , Assign "temp_b" (NCon 0)
			        , While (Bin Min (Var "b") (Var "temp_b"))
			            [ Assign "temp_a" (Bin Plus (Var "temp_a") (NCon 1))
			            , Assign "temp_b" (Bin Plus (Var "temp_b") (NCon 1))
			            , If (Bin Min (Bin Plus (Var "a") (NCon 1)) (Var "temp_a"))
			                [ Assign "a" (Var "a")]
			                [ Assign "a_greater_than_b_or_equal" (NCon 0)]
			            ]
			        ]
     
     ]


----------------------------------------------
-- 
-- Section 2
--
----------------------------------------------


-- OPDRACHT 1
---
-- ongeaccumuleerde versie
---
    --Hoofdfunctie die een lijst retourneert met alle gebruikte variabelen
program_used_variables:: Prog->[Name]
program_used_variables (dl, sl) = used_variables sl

    --functie voor het opzoeken van gebruikte variabelen in een lijst van Statements
used_vars_statlist::[Stat] -> [Name]
used_vars_statlist [] = []
used_vars_statlist (x:xs) = used_vars_stat x ++ used_vars_statlist xs

    --Functie die de variabelen in Statements laat zien
used_vars_stat :: Stat -> [Name]
used_vars_stat (Assign n e) = [n] ++ used_vars_expr e
used_vars_stat (If e xs1 xs2) = used_vars_expr e ++ used_vars_statlist xs1 ++ used_vars_statlist xs2
used_vars_stat (While e xs) = used_vars_expr e ++ used_vars_statlist xs

    --Functie die de variabelen in expressies (indien mogelijk) retourneert
used_vars_expr:: Expr -> [Name]
used_vars_expr (NCon _) = []
used_vars_expr (SCon _) = []
used_vars_expr (Var n) = [n]
used_vars_expr (Bin _ e1 e2) = used_vars_expr e1 ++ used_vars_expr e2
    
    -- Functie die alle gebruikte variabelen in een statement lijst retourneert
used_variables:: [Stat] -> [Name]
used_variables = nub.used_vars_statlist

---
-- geaccumuleerde versie
---
    --Hoofdfunctie die een lijst retourneert met alle gebruikte variabelen
program_g_used_variables:: Prog -> [Name]
program_g_used_variables (dl, sl) = g_used_variables sl
    -- Functie die een lijst retourneert met alle gebruikte variabelen en alle duplicaten verwijdert.
    -- De input word gegeven door een lijst van statements.
g_used_variables:: [Stat] -> [Name]
g_used_variables stats = nub (g_used_vars_statlist [] stats)

    -- Functie die alle gebruikte variabelen in een statement lijst retourneert, mbv een accumulator
g_used_vars_statlist::[String] -> [Stat] -> [String]
g_used_vars_statlist acc [] = acc
g_used_vars_statlist acc (x:xs) = g_used_vars_statlist (acc ++ used_vars_stat x) xs


-- OPDRACHT 2
    -- Hoofdfunctie die een gegeven programma checkt op context conditie 1
context_conditie1::Prog->Bool
context_conditie1 ([],[]) = True
context_conditie1 (dl,sl) = check_type_statlist dl sl

    -- Functie die gegeven een declaratie lijst en statement lijst kijkt of de variabelen die gebruikt worden in beiden lijsten voorkomen, niet meer, niet minder. Er word gebruik gemaakt van de functie used_variables van Opdracht 2.1
context_conditie1_stats:: [Decl] -> [Stat] -> Bool
context_conditie1_stats dl sl = check_declared (fst (unzip dl)) sl && check_reserved (used_variables sl)
    -- Deze functie neemt de doorsnede van twee verzamelingen, die gemaakt worden uit de declaratie resp. statement lijst
--check_declared:: [Decl] -> [Stat] -> [Name]
check_declared dl sl = Set.null (Set.difference (Set.fromList dl) (Set.fromList (used_variables sl)))
    -- Functie die check of de input lijst geen variabele namen bevatten die reserved zijn.
check_reserved :: [Name] -> Bool
check_reserved [] = True    
check_reserved (x:xs) = notElem x reservedWords && check_reserved xs

-- OPDRACHT 3
---
-- Ongeaccumuleerd
---
program_context_conditie2::Prog -> Bool
program_context_conditie2 (dl,sl) = context_conditie2 dl

    --Hoofdfunctie die controleert op contextconditie 2
context_conditie2:: [Decl] -> Bool
context_conditie2 dl = not (same_pair (dl))

    -- Functie die checkt of twee opeenvolgende items uit een lijst gelijk zijn aan elkaar
same_pair:: Eq a => [a] -> Bool
same_pair [] = True
same_pair [a] = False
same_pair [a,b] = (a == b)
same_pair (a:b:xs) = (a == b) || same_pair (a:xs) || same_pair (b:xs) 

----
-- geaccumuleerd
----
    --Hoofdfunctie
g_context_conditie2 sl = g_same_pair False (sort sl)

g_same_pair acc (a:b:xs) = g_same_pair (acc || (a == b)) (b:xs)



-- OPDRACHT 4
    -- Hoofdfunctie, die een typecheck uitvoert op een Pico programma
program_type_check::Prog->Bool
program_type_check ([],[]) = True
program_type_check (dl,sl) = check_type_statlist dl sl

    -- Checks the type of a list op statments
check_type_statlist::[Decl]-> [Stat] -> Bool
check_type_statlist _ [] = True
check_type_statlist zs (x:xs) = check_type_stat x zs && check_type_statlist zs xs


    --A help function to get the type of a variable
check_type_var :: String -> [Decl] -> Type
check_type_var a (x:xs) = if a == (fst x) then snd x else check_type_var a xs

    -- Returns the type of an Equation, using the types in the declaration list
check_type_eq :: Expr -> [Decl] -> Type
check_type_eq (SCon _) _ = S
check_type_eq (NCon _) _ = N
check_type_eq (Var v) [] = S
check_type_eq (Var v) (x:xs) =  if v == (fst x) then
                                        (snd x)
                                    else
                                        check_type_eq (Var v) xs

check_type_eq (Bin o e1 e2) zs 
        | o == Cat = if (S == (check_type_eq e1 zs) && S == (check_type_eq e2 zs)) then S else N
        | otherwise = if (N == (check_type_eq e1 zs) && N == (check_type_eq e2 zs) ) then N else S
        
    -- Returns whether a statement is correctly typed
check_type_stat::Stat -> [Decl] -> Bool
check_type_stat (If e xs ys) zs = N == check_type_eq e zs 
                                && check_type_statlist zs xs  
                                && check_type_statlist zs ys
check_type_stat (While e xs) zs = N == check_type_eq e zs 
                                && check_type_statlist zs xs
check_type_stat (Assign a e) zs = (check_type_var a zs) == (check_type_eq e zs)


-- OPDRACHT 5
-- num_assign_statlist
{- Berekent het aantal assignments in een lijst van statements. Maakt gebruik van
de functie num_assign_stats om te discrimineren tussen de verschillende types statements
en alleen de statements van het type assignment te tellen.-}
num_assign_statlist :: [Stat] -> Integer
num_assign_statlist [] = 0
num_assign_statlist (x:xs) =  num_assign_stats x + num_assign_statlist xs

-- num_assign_stats
{- Controleert welk type statement er wordt meegegeven en telt het aantal 
statements van het type assignment.-}
num_assign_stats :: Stat -> Integer
num_assign_stats (Assign n e) = 1
num_assign_stats (If e s1 s2) = num_assign_statlist s1 + num_assign_statlist s2
num_assign_stats (While e s1) = num_assign_statlist s1

-- used_vars_assign_statlist
{-Berekent voor alle statements in de lijst met statements de namen van de 
variabelen die gebruikt worden in statements van het type assignment. Maakt gebruik
van de functie used_vars_assign_stats om te discrimineren tussen de verschillende
types statements en alleen de variabelen in statements van het type assignment te 
evalueren.-}
used_vars_assign_statlist :: [Stat] -> [Name]
used_vars_assign_statlist [] = []
used_vars_assign_statlist (x:xs) = used_vars_assign_stats x ++ used_vars_assign_statlist xs

-- used_vars_assign_stats
{-Berekent de namen van de variabelen die gebruikt worden in een statement van het type 
assignment.-}
used_vars_assign_stats :: Stat -> [Name]
used_vars_assign_stats (Assign n e) = [n] ++ used_vars_expr e
used_vars_assign_stats (If e xs1 xs2) = used_vars_assign_statlist xs1 ++ used_vars_assign_statlist xs2
used_vars_assign_stats (While e xs) = used_vars_assign_statlist xs

-- num_assign_stats_var_names_statlist
{-Berekent een lijst met paren bestaande uit het aantal assignments en diens betrokken variabelen.
Voor ieder statement van het type assignment in de gegeven statementlijst wordt een paar berekend.-} 
num_assign_stats_var_names_statlist :: [Stat] -> [(Integer, [Name])]
num_assign_stats_var_names_statlist [] = []
num_assign_stats_var_names_statlist (x:xs) = [num_assign_stats_var_names x] ++ num_assign_stats_var_names_statlist xs

-- num_assign_stats_var_names
{- Retourneert een paar met het aantal assignments en de betrokken variabelen m.b.t.
die assignments die voorkomen binnen een gegeven statement.-}
num_assign_stats_var_names :: Stat -> (Integer, [Name])
num_assign_stats_var_names s = (num_assign_stats s, nub (used_vars_assign_stats s))


-- OPDRACHT 6
-- stat_statistics
{- Berekent voor een gegeven programma p in one-pass het 3-tuple (nrA; nrI; nrW)
waarbij nrA, nrI en nrW respectievelijk het aantal assignments, if-statements en
while-statements van p voorstellen.-}
stat_statistics :: Prog -> (Integer, Integer, Integer)
stat_statistics (_, sl) = count_used_stats sl

-- count_used_stats
{- Doorloopt recursief een statementlijst en telt per type statement het aantal
statements van dat type.-}
count_used_stats :: [Stat] -> (Integer, Integer, Integer)
count_used_stats [] = (0, 0, 0)
count_used_stats (s:ss) = t3plus (count_used_stat s) (count_used_stats ss)

-- count_used_stat
{- Controleert welk type statement er wordt meegegeven en verhoogt de corresponderende teller
in het 3-tupel (nrA; nrI; nrW).-}
count_used_stat :: Stat -> (Integer, Integer, Integer)
count_used_stat (Assign _ _) = (1, 0, 0)
count_used_stat (If _ ss1 ss2) = t3plus (0, 1, 0) (t3plus (count_used_stats ss1) (count_used_stats ss2))
count_used_stat (While _ ss) = t3plus (0, 0 , 1) (count_used_stats ss)

-- t3plus
-- functie om 3-tupels op te tellen.
t3plus :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
t3plus (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)


-- OPDRACHT 7
-- calc_maccabe
-- Berekent de MacCabe complexiteit van een gegeven pico programma.
calc_maccabe :: Prog -> Integer
calc_maccabe (_, sl) = calc_guards_statlist sl

-- calc_guards_statlist
-- Berekent recursief het aantal guards in een lijst met statements.
calc_guards_statlist :: [Stat] -> Integer
calc_guards_statlist [] = 0
calc_guards_statlist (s:ss) = calc_guards_stat s + calc_guards_statlist ss

-- calc_guards_stat
-- Berekent het aantal guards in een gegeven statement.
calc_guards_stat :: Stat -> Integer
calc_guards_stat (Assign e n) = 0
calc_guards_stat (If e ss1 ss2) = 1 + calc_guards_statlist ss1 + calc_guards_statlist ss2
calc_guards_stat (While e ss) = 1 + calc_guards_statlist ss  


-- Section 3
-----------------------------------
-- In principe wordt de state van het programma puur bepaald door de waarden van de variabelen in de declaratie.
-- We gaan over de lijst van statements heen en updaten daarbij constant de waarde van onze variabele lijst.

-- We bepalen de state van een programma door paren van variabele namen en hun waarde door te geven bij elke stap in de syntax boom.
-- We initializeren getallen op 0 en strings op ""
create_var_list :: [(Name, Type)] -> State
create_var_list [] = []
create_var_list ((name, N):xs) = [(name, (N1 0))] ++ create_var_list xs
create_var_list ((name, S):xs) = [(name, (S1 ""))] ++ create_var_list xs

-- This function updates the value of a given variable (name) in a given variable list
assign_var value name [] = []
assign_var value name ((n, v):xs) = if (n == name)
                                    then [(n, value)] ++ assign_var value name xs
                                    else [(n, v)] ++ assign_var value name xs

-- This function obtains the value of a given variable (name) in a list of variables
get_value name [] = (N1 0)
get_value name ((n, v):xs) = if (n == name) then v else get_value name xs

-- Deze functie evalueert een gegeven PICO programma in Haskell notatie.
program_eval (dl,sl)= eval dl sl 

eval dl sl = if ( (context_conditie1_stats dl sl) && (context_conditie2 dl) )
             then eval_stats sl (create_var_list dl)
             else [("Error", (N1 1))]
             

-- This function takes a list of statements and recursively calculates the value of the variables
eval_stats [] variables = variables
eval_stats (s:statements) variables = eval_stats statements (eval_stat s variables)

-- Evaluation functions for statements
eval_stat (Assign n e) variables = assign_var (eval_expr e variables) n variables

eval_stat (If e s1 s2) variables = if (not (val_is_null (eval_expr e variables)))
                                   then eval_stats s1 variables
                                   else eval_stats s2 variables

eval_stat (While e statements) variables = eval_while e statements variables
eval_while e statements variables = if (val_is_null (eval_expr e variables))
                                    then variables
                                    else eval_while e statements (eval_stats statements variables)

-- Evaluates the value of an expression
eval_expr (NCon c) variables = (N1 c)
eval_expr (SCon s) variables = (S1 s)
eval_expr (Var n) variables = get_value n variables

eval_expr (Bin op e1 e2) variables = eval_op op e1 e2 variables

eval_op o e1 e2 variables
    | (o == Plus) = val_plus (eval_expr e1 variables) (eval_expr e2 variables)
    | (o == Min) = val_min (eval_expr e1 variables) (eval_expr e2 variables)
    | otherwise = val_cat (eval_expr e1 variables) (eval_expr e2 variables)

val_plus (N1 a) (N1 b) = (N1 (a + b))
val_min (N1 a) (N1 b) = (N1 (a - b))
val_cat (S1 a) (S1 b) = (S1 (a ++ b))

val_is_null (N1 a) = (a == 0)
val_is_null (S1 a) = (a == "")