>import Hutton.Parsing
>import List
>import Data.Set (Set)
>import qualified Data.Set as Set

>type Prog = ([Decl],[Stat])
>type Decl = (Name, Type)
>type Name = String

>reservedWords = ["begin", "end", "declare", "natural", "string", "if", "then", "else", "fi", "while", "do", "od"]

>data Type = N | S deriving (Show, Eq)

waarden bij type N en S

>data Val  = N1 Integer | S1 String deriving (Show,Eq)

>data Stat = Assign Name Expr
>          | If Expr [Stat] [Stat]
>          | While Expr [Stat]
>          deriving (Show,Eq)

>data Expr = NCon Integer
>          | SCon String
>          | Var Name
>          | Bin Op Expr Expr
>          deriving (Show,Eq)

>data Op   = Plus | Min | Cat deriving (Show,Eq)

Parses a PICO program and generates a Haskell data structure using the constructors above (if possible, see below)

>picoParse :: FilePath -> IO (Maybe Prog)
>picoParse f = do
>              source <- readFile f
>              return (if_its_correct source)

>testPicoParse f = do p <- picoParse f
>                     putStr (show p)

This function returns a program when both the source is syntactically correct and all the context conditions are true (including type checking)

>if_its_correct :: String -> Maybe Prog
>if_its_correct source = if (parse prog source) /= [] then
>                            if (context_conditie1 program) && (program_context_conditie2 program) && (program_type_check program) then
>                                Just program
>                            else
>                                Nothing
>                        else
>                            Nothing
>                        where
>                            program = (fst (head (parse prog source)))

This parser corresponds to the bnf grammar rule:
PROG ::= "begin" DECLS ";" STATLS "end"
the parser returns a pair of declarations and statements as defined by the constructors above

>prog :: Parser Prog
>prog = do
>       symbol "begin"
>       result_decls <- decls
>       symbol ";"
>       result_statls <- statls
>       symbol "end"
>       return (result_decls, result_statls)

This parser corresponds to the bnf grammar rule:
DECLS   ::= "declare" IDLS
this parser constructs a list of declarations, note that specific declarations are parsed by 'idls'

>decls :: Parser [Decl]
>decls = do
>        symbol "declare"
>        result_idls <- idls
>        return result_idls

This parser corresponds to the bnf grammar rule:
IDLS ::= eps | IDTS
note that epsilon has been implemented as 'return []'

>idls :: Parser [Decl]
>idls = do
>       result <- idts +++ (return [])
>       return result

This parser corresponds to the bnf grammar rule:
IDTS ::= PICO-ID ":" TYPE ( "," PICO-ID ":" TYPE )*
we have chosen to implement idts (first element of the list) and idtss (remainder of the list) as separate parser functions.
Although this approach is more verbose, it more clearly exhibits the destinction between elements that follow a ':' and a single declaration.
The combination of the two construct the actual list of declarations.

>idts :: Parser [Decl]
>idts = do
>       result_picoid <- picoid
>       symbol ":"
>       result_parse_type <- parse_type
>       result_list <- many (idtss)
>       return ((result_picoid, result_parse_type):result_list)

>idtss :: Parser Decl
>idtss = do
>            symbol ","
>            result_picoid <- picoid
>            symbol ":"
>            result_parse_type <- parse_type
>            return (result_picoid, result_parse_type)

This parser corresponds to the bnf grammar rule:
TYPE ::= TYPE_NATURAL | TYPE_STRING
We have strived to maintain the names as they appear in the bnf grammar but since 'type' is a reserved word in Haskell we have made the design decision to prepend such names with 'parse_'
This parser determines the type of a declaration. We have chosen not to put the Parser of each type directly in this function as now new types can be more easily added to the language definition of PICO.

>parse_type :: Parser Type
>parse_type = do
>             result <- type_natural +++ type_string
>             return result

This parser corresponds to the bnf grammar rule:
TYPE_NATURAL ::= "natural"
This parser constructs the Type for a Natural number needed in a declaration.

>type_natural :: Parser Type
>type_natural = do
>               symbol "natural"
>               return N

This parser corresponds to the bnf grammar rule:
TYPE_STRING ::= "string"
This parser constructs the Type for a String needed in a declaration.

>type_string :: Parser Type
>type_string = do
>              symbol "string"
>              return S

This parser corresponds to the bnf grammar rule:
STATLS ::= eps | STATS
note that eps again is implemented as 'return []'

>statls :: Parser [Stat]
>statls = do
>         result <- stats +++ (return [])
>         return result

This parser corresponds to the bnf grammar rule:
STATS ::= STAT ( ";" STAT )*
It contstructs a list of statements.

>stats :: Parser [Stat]
>stats = do
>        result_stat <- stat
>        result_list <- many (statss)
>        return (result_stat:result_list)

>statss :: Parser Stat
>statss = do
>            symbol ";"
>            result_stat <- stat
>            return result_stat

This parser corresponds to the bnf grammar rule:
STAT ::= STAT_ASSIGN | STAT_IF | STAT_WHILE
Similar to the type, this function can easily be extended to include new definitions for statements to be added to the PICO language definition. For example if one were to add a FOR statement, one needs to implement a parser for such FOR statement, but to include in this parser the line 'result <- stat_assign +++ stat_if +++ stat_while' just needs to be extended with '+++ stat_for'.

>stat :: Parser Stat
>stat = do
>       result <- stat_assign +++ stat_if +++ stat_while
>       return result

This parser corresponds to the bnf grammar rule:
STAT_ASSIGN ::= VAR ":=" EXPR
It constructs an assignment from the parsed identifier and expression.

>stat_assign :: Parser Stat
>stat_assign = do
>              result_var <- identifier
>              symbol ":="
>              result_expr <- expr
>              return (Assign result_var result_expr)

This parser corresponds to the bnf grammar rule:
STAT_IF ::= "if" EXPR "then" STATLS "else" STATLS "fi"
An if statement is constructed using the parsed expression and two lists of statements.

>stat_if :: Parser Stat
>stat_if = do
>          symbol "if"
>          result_expr <- expr
>          symbol "then"
>          result_statls <- statls
>          symbol "else"
>          result_statls2 <- statls
>          symbol "fi"
>          return (If result_expr result_statls result_statls2)

This parser corresponds to the bnf grammar rule:
STAT_WHILE ::= "while" EXPR "do" STATLS "od"
Similar to if, this parser constructs a while statement in the parse tree.

>stat_while :: Parser Stat
>stat_while = do
>             symbol "while"
>             result_expr <- expr
>             symbol "do"
>             result_statls <- statls
>             symbol "od"
>             return (While result_expr result_statls)

As one can see from the BNF grammar, the original syntax for expressions is not LL(1). Therefore we rewrite the non-terminals in the following way:
expr ::= term ( '+' expr | '-' expr | '||' expr | eps )
term ::= natural | string | var | '(' expr ')'
this grammar is LL(1), now we can introduce functions that parse that particular non-terminal

>expr :: Parser Expr
>expr = do
>        result_term1 <- term
>        result <- ((expr_plus result_term1) +++ (expr_min result_term1) +++ (expr_cat result_term1) +++ (return result_term1) )
>        return result

as described above this function implements 'term' as laid out by the BNF grammar above.

>term :: Parser Expr
>term = do
>       result <- ( expr_natcon +++ expr_strcon +++ expr_var +++ expr_parenthesis )
>       return result

This parser corresponds to the bnf grammar rule:
EXPR_NATCON    ::= NatCon
This function parses a natural constant which simply uses the natural number parser provided by Hutton. It then constructs that constant in the parse tree.

>expr_natcon :: Parser Expr
>expr_natcon = do
>              result_natcon <- natural
>              return (NCon (toInteger(result_natcon)))

This parser corresponds to the bnf grammar rule:
EXPR_STRCON ::= StrCon
Similar to above, this also uses a Hutton parser to parse a string, with the addition of parsing the start and end double quotes as required by the PICO language specification. It then constructs that string in the parse tree. Note that the Hutton function returns a list of strings (of singleton chars) instead of a string therefore we concatenate it first to obtain a true string.

>expr_strcon :: Parser Expr
>expr_strcon = do
>                symbol "\""
>                result_strcon <- many (parse_char)
>                symbol "\""
>                return (SCon (concat (result_strcon)))

This parser corresponds to the bnf grammar rule (after trivial substitution of VAR with PICOID):
EXPR_VAR ::= PICO-ID
We use the function provided by Hutton to parse an identifier in PICO. We then construct a variable with that name.

>expr_var :: Parser Expr
>expr_var = do
>           result_var <- identifier
>           return (Var result_var)

The following functions use the rewritten grammar as laid out above and return a binary operator according to the right symbol.
Note that the first expression is passed as a parameter by the caller as two expressions are needed to correctly construct a binary operator.

>expr_plus ::  Expr -> Parser Expr
>expr_plus e = do
>                symbol "+"
>                result_expr <- expr
>                return (Bin Plus e result_expr)

>expr_min :: Expr -> Parser Expr
>expr_min e = do
>              symbol "-"
>              result_expr <- expr
>              return (Bin Min e result_expr)

>expr_cat :: Expr -> Parser Expr
>expr_cat e = do
>              symbol "||"
>              result_expr <- expr
>              return (Bin Cat e result_expr)

>expr_parenthesis :: Parser Expr
>expr_parenthesis = do
>                   symbol "("
>                   result_expr <- expr
>                   symbol ")"
>                   return result_expr

This parser corresponds to the bnf grammar rule:
PICO-ID ::= CHAR ( CHAR | DIGIT )*
which is simply a list of allowed characters for identifiers.

>picoid :: Parser Name
>picoid = do
>            result_parse_char <- parse_char
>            result_list <- many (
>                 parse_char +++
>                 parse_digit
>                 )
>            return (concat ([result_parse_char]++result_list))

This parser corresponds to the bnf grammar rule:
CHAR    ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" 
This function parses a single character, note that this can be written shorter. However, this way was chosen as it follows the BNF specification more closely

>parse_char :: Parser String
>parse_char = do
>             result <- (
>                 symbol "a"
>                 +++
>                 symbol "b"
>                 +++
>                 symbol "c"
>                 +++
>                 symbol "d"
>                 +++
>                 symbol "e"
>                 +++
>                 symbol "f"
>                 +++
>                 symbol "g"
>                 +++
>                 symbol "h"
>                 +++
>                 symbol "i"
>                 +++
>                 symbol "j"
>                 +++
>                 symbol "k"
>                 +++
>                 symbol "l"
>                 +++
>                 symbol "m"
>                 +++
>                 symbol "n"
>                 +++
>                 symbol "o"
>                 +++
>                 symbol "p"
>                 +++
>                 symbol "q"
>                 +++
>                 symbol "r"
>                 +++
>                 symbol "s"
>                 +++
>                 symbol "t"
>                 +++
>                 symbol "u"
>                 +++
>                 symbol "v"
>                 +++
>                 symbol "w"
>                 +++
>                 symbol "x"
>                 +++
>                 symbol "y"
>                 +++
>                 symbol "z"
>                 +++
>                 symbol "A"
>                 +++
>                 symbol "B"
>                 +++
>                 symbol "C"
>                 +++
>                 symbol "D"
>                 +++
>                 symbol "E"
>                 +++
>                 symbol "F"
>                 +++
>                 symbol "G"
>                 +++
>                 symbol "H"
>                 +++
>                 symbol "I"
>                 +++
>                 symbol "J"
>                 +++
>                 symbol "K"
>                 +++
>                 symbol "L"
>                 +++
>                 symbol "M"
>                 +++
>                 symbol "N"
>                 +++
>                 symbol "O"
>                 +++
>                 symbol "P"
>                 +++
>                 symbol "Q"
>                 +++
>                 symbol "R"
>                 +++
>                 symbol "S"
>                 +++
>                 symbol "T"
>                 +++
>                 symbol "U"
>                 +++
>                 symbol "V"
>                 +++
>                 symbol "W"
>                 +++
>                 symbol "X"
>                 +++
>                 symbol "Y"
>                 +++
>                 symbol "Z"
>                 )
>             return result

This parser corresponds to the bnf grammar rule:
DIGIT   ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Similar to parsing the digit, this form was chosen to adhere more closely to the BNF syntax of PICO.

>parse_digit :: Parser String
>parse_digit = do
>              result <- (
>                  symbol "0"
>                  +++
>                  symbol "1"
>                  +++
>                  symbol "2"
>                  +++
>                  symbol "3"
>                  +++
>                  symbol "4"
>                  +++
>                  symbol "5"
>                  +++
>                  symbol "6"
>                  +++
>                  symbol "7"
>                  +++
>                  symbol "8"
>                  +++
>                  symbol "9"
>                  )
>              return result



Below are the context conditions as described in assignment 2

Hoofdfunctie die een gegeven programma checkt op context conditie 1

>context_conditie1::Prog->Bool
>context_conditie1 ([],[]) = True
>context_conditie1 (dl,sl) = check_type_statlist dl sl

Functie die gegeven een declaratie lijst en statement lijst kijkt of de variabelen die gebruikt worden in beiden lijsten voorkomen, niet meer, niet minder. Er word gebruik gemaakt van de functie used_variables van Opdracht 2.1

>context_conditie1_stats:: [Decl] -> [Stat] -> Bool
>context_conditie1_stats dl sl = check_declared (fst (unzip dl)) sl && check_reserved (used_variables sl)

Deze functie neemt de doorsnede van twee verzamelingen, die gemaakt worden uit de declaratie resp. statement lijst

>check_declared dl sl = Set.null (Set.difference (Set.fromList dl) (Set.fromList (used_variables sl)))

Functie die check of de input lijst geen variabele namen bevatten die reserved zijn.

>check_reserved :: [Name] -> Bool
>check_reserved [] = True    
>check_reserved (x:xs) = notElem x reservedWords && check_reserved xs

Hoofdfunctie die een lijst retourneert met alle gebruikte variabelen

>program_g_used_variables:: Prog -> [Name]
>program_g_used_variables (dl, sl) = g_used_variables sl

Functie die een lijst retourneert met alle gebruikte variabelen en alle duplicaten verwijdert.
De input word gegeven door een lijst van statements.

>g_used_variables:: [Stat] -> [Name]
>g_used_variables stats = nub (g_used_vars_statlist [] stats)

Functie die alle gebruikte variabelen in een statement lijst retourneert, mbv een accumulator

>g_used_vars_statlist::[String] -> [Stat] -> [String]
>g_used_vars_statlist acc [] = acc
>g_used_vars_statlist acc (x:xs) = g_used_vars_statlist (acc ++ used_vars_stat x) xs

functie voor het opzoeken van gebruikte variabelen in een lijst van Statements

>used_vars_statlist::[Stat] -> [Name]
>used_vars_statlist [] = []
>used_vars_statlist (x:xs) = used_vars_stat x ++ used_vars_statlist xs

Functie die de variabelen in Statements laat zien

>used_vars_stat :: Stat -> [Name]
>used_vars_stat (Assign n e) = [n] ++ used_vars_expr e
>used_vars_stat (If e xs1 xs2) = used_vars_expr e ++ used_vars_statlist xs1 ++ used_vars_statlist xs2
>used_vars_stat (While e xs) = used_vars_expr e ++ used_vars_statlist xs

Functie die de variabelen in expressies (indien mogelijk) retourneert

>used_vars_expr:: Expr -> [Name]
>used_vars_expr (NCon _) = []
>used_vars_expr (SCon _) = []
>used_vars_expr (Var n) = [n]
>used_vars_expr (Bin _ e1 e2) = used_vars_expr e1 ++ used_vars_expr e2

Functie die alle gebruikte variabelen in een statement lijst retourneert

>used_variables:: [Stat] -> [Name]
>used_variables = nub.used_vars_statlist

Context conditie 2

>program_context_conditie2::Prog -> Bool
>program_context_conditie2 (dl,sl) = context_conditie2 dl

Hoofdfunctie die controleert op contextconditie 2

>context_conditie2:: [Decl] -> Bool
>context_conditie2 dl = not (same_pair (dl))

Functie die checkt of twee opeenvolgende items uit een lijst gelijk zijn aan elkaar

>same_pair:: Eq a => [a] -> Bool
>same_pair [] = True
>same_pair [a] = False
>same_pair [a,b] = (a == b)
>same_pair (a:b:xs) = (a == b) || same_pair (a:xs) || same_pair (b:xs) 


Type Checking

Hoofdfunctie, die een typecheck uitvoert op een Pico programma

>program_type_check::Prog->Bool
>program_type_check ([],[]) = True
>program_type_check (dl,sl) = check_type_statlist dl sl

Checks the type of a list op statments

>check_type_statlist::[Decl]-> [Stat] -> Bool
>check_type_statlist _ [] = True
>check_type_statlist zs (x:xs) = check_type_stat x zs && check_type_statlist zs xs


A help function to get the type of a variable

>check_type_var :: String -> [Decl] -> Type
>check_type_var a (x:xs) = if a == (fst x) then snd x else check_type_var a xs

Returns the type of an Equation, using the types in the declaration list

>check_type_eq :: Expr -> [Decl] -> Type
>check_type_eq (SCon _) _ = S
>check_type_eq (NCon _) _ = N
>check_type_eq (Var v) [] = S
>check_type_eq (Var v) (x:xs) =  if v == (fst x) then
>                                        (snd x)
>                                    else
>                                        check_type_eq (Var v) xs

>check_type_eq (Bin o e1 e2) zs 
>        | o == Cat = if (S == (check_type_eq e1 zs) && S == (check_type_eq e2 zs)) then S else N
>        | otherwise = if (N == (check_type_eq e1 zs) && N == (check_type_eq e2 zs) ) then N else S

Returns whether a statement is correctly typed

>check_type_stat::Stat -> [Decl] -> Bool
>check_type_stat (If e xs ys) zs = N == check_type_eq e zs 
>                                && check_type_statlist zs xs  
>                                && check_type_statlist zs ys
>check_type_stat (While e xs) zs = N == check_type_eq e zs 
>                                && check_type_statlist zs xs
>check_type_stat (Assign a e) zs = (check_type_var a zs) == (check_type_eq e zs)