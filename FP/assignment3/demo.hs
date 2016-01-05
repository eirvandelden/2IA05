import Hutton.Parsing

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

-- Declare extra functions here
eps = return []

loadFile f = do
             s <- readFile f
             return (parse prog s)

                

prog :: Parser Prog
prog = do
       symbol "begin"
       result_decls <- decls
       symbol ";"
       result_statls <- statls
       symbol "end"
       return (result_decls, result_statls)

decls :: Parser [Decl]
decls = do
        symbol "declare"
        result_idls <- idls
        return result_idls

idls :: Parser [Decl]
idls = do
       result <- idts +++ eps
       return result

idts :: Parser [Decl]
idts = do
       result_picoid <- picoid
       symbol ":"
       result_parse_type <- parse_type
       result_list <- many (idtss)
       return ((result_picoid, result_parse_type):result_list)

idtss :: Parser Decl
idtss = do 
            symbol ","
            result_picoid <-picoid
            symbol ":"
            result_parse_type <- parse_type
            return (result_picoid, result_parse_type)

parse_type :: Parser Type
parse_type = do
             result <- type_natural +++ type_string
             return result

type_natural :: Parser Type
type_natural = do
               symbol "natural"
               return N

type_string :: Parser Type
type_string = do
              symbol "string"
              return S

statls :: Parser [Stat]
statls = do
         result <- stats +++ eps
         return result

stats :: Parser [Stat]
stats = do
        result_stat <- stat
        result_list <- many (statss)
        return (result_stat:result_list)

statss :: Parser Stat
statss = do  
            symbol ";"
            result_stat <- stat
            return result_stat


stat :: Parser Stat
stat = do
       result <- stat_assign +++ stat_if +++ stat_while 
       return result

stat_assign :: Parser Stat
stat_assign = do
              result_var <- identifier
              symbol ":="
              result_expr <- expr
              return (Assign result_var result_expr)

stat_if :: Parser Stat
stat_if = do
          symbol "if"
          result_expr <- expr
          symbol "then"
          result_statls <- statls
          symbol "else"
          result_statls2 <- statls
          symbol "fi"
          return (If result_expr result_statls result_statls2)

stat_while :: Parser Stat
stat_while = do
             symbol "while"
             result_expr <- expr
             symbol "do"
             result_statls <- statls
             symbol "od"
             return (While result_expr result_statls)

expr :: Parser Expr
expr = do 
        result_term1 <- term
        result <- ((expr_plus result_term1) +++ (expr_min result_term1) +++ (expr_cat result_term1) +++ eps )
        
        return result
        
term :: Parser Expr
term = do
            result <- (do
			            symbol "("
				        result_expr <- expr
				        symbol ")"
				        return result_expr
			) +++
			result <- expr_natcon +++
			result <- exprstr_con
			return result

expr_natcon :: Parser Expr
expr_natcon = do
              result_natcon <- natural
              return (NCon (toInteger(result_natcon)))

expr_strcon :: Parser Expr
expr_strcon = do
                symbol "\""
                result_strcon <- many (parse_char)
                symbol "\""
                return (SCon (concat (result_strcon)))

expr_var :: Parser Expr
expr_var = do
           result_var <- identifier
           return (Var result_var)

expr_plus :: Parser Expr -> Expr
expr_plus e = do
              (symbol "+"
              result_expr <- expr) +++ eps
              return (Bin Plus e result_expr)

expr_plus :: Parser Expr -> Expr
expr_plus e = do
              (symbol "-"
              result_expr <- expr) +++ eps
              return (Bin Min e result_expr)

expr_plus :: Parser Expr -> Expr
expr_plus e = do
              (symbol "||"
              result_expr <- expr) +++ eps
              return (Bin Cat e result_expr)

expr_parenthesis :: Parser Expr
expr_parenthesis = do
                   (symbol "("
                   result_expr <- expr
                   symbol ")")
                   return result_expr


picoid :: Parser Name
picoid = do
            result_parse_char <- parse_char
            result_list <- many (
                 parse_char +++
                 parse_digit
                 )
            return (concat ([result_parse_char]++result_list))

parse_char :: Parser String
parse_char = do
             result <- (
                 symbol "a"
                 +++
                 symbol "b"
                 +++
                 symbol "c"
                 +++
                 symbol "d"
                 +++
                 symbol "e"
                 +++
                 symbol "f"
                 +++
                 symbol "g"
                 +++
                 symbol "h"
                 +++
                 symbol "i"
                 +++
                 symbol "j"
                 +++
                 symbol "k"
                 +++
                 symbol "l"
                 +++
                 symbol "m"
                 +++
                 symbol "n"
                 +++
                 symbol "o"
                 +++
                 symbol "p"
                 +++
                 symbol "q"
                 +++
                 symbol "r"
                 +++
                 symbol "s"
                 +++
                 symbol "t"
                 +++
                 symbol "u"
                 +++
                 symbol "v"
                 +++
                 symbol "w"
                 +++
                 symbol "x"
                 +++
                 symbol "y"
                 +++
                 symbol "z"
                 +++
                 symbol "A"
                 +++
                 symbol "B"
                 +++
                 symbol "C"
                 +++
                 symbol "D"
                 +++
                 symbol "E"
                 +++
                 symbol "F"
                 +++
                 symbol "G"
                 +++
                 symbol "H"
                 +++
                 symbol "I"
                 +++
                 symbol "J"
                 +++
                 symbol "K"
                 +++
                 symbol "L"
                 +++
                 symbol "M"
                 +++
                 symbol "N"
                 +++
                 symbol "O"
                 +++
                 symbol "P"
                 +++
                 symbol "Q"
                 +++
                 symbol "R"
                 +++
                 symbol "S"
                 +++
                 symbol "T"
                 +++
                 symbol "U"
                 +++
                 symbol "V"
                 +++
                 symbol "W"
                 +++
                 symbol "X"
                 +++
                 symbol "Y"
                 +++
                 symbol "Z"
                 )
             return result

parse_digit :: Parser String
parse_digit = do
              result <- (
                  symbol "0"
                  +++
                  symbol "1"
                  +++
                  symbol "2"
                  +++
                  symbol "3"
                  +++
                  symbol "4"
                  +++
                  symbol "5"
                  +++
                  symbol "6"
                  +++
                  symbol "7"
                  +++
                  symbol "8"
                  +++
                  symbol "9"
                  )
              return result

