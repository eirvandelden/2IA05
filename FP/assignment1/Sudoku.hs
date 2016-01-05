module Sudoku where

import Stack

-- set-difference: sdiff xs ys
sdiff :: Eq a => [a] -> [a] -> [a]
sdiff [] zs = []
sdiff (x:xs) zs 
  | elem x zs   = sdiff xs zs
  | otherwise   = x:sdiff xs zs

type Sudoku = [Cel]
type Cel = (Pos, Val)

type Pos = (Index,Index)
type Index = Int
type Val   = Int

allval :: [Val]
allval = [1..9]

emptyVal :: Val
emptyVal = 0
------------------------------------------------------------------------
-- t.a.v. posities
------------------------------------------------------------------------
allpos :: [Pos]
allpos = [(i,j)|i<-[0..8],j<-[0..8]]         -- rijsgewijze opsomming

-- relatie via rijen, kolommen en blokken 
row   (i,j) (k,l) = (i == k)
column (i,j) (k,l) = (j == l)
block  (i,j) (k,l) = (i `div` 3 == k `div` 3) && (j `div` 3 == l `div` 3)

------------------------------------------------------------------------
-- t.a.v. sudokus
------------------------------------------------------------------------
dom = map fst                                -- ingevuld

undef s = sdiff allpos (dom s)               -- openstaand

-- sudoku-spelregel
incident :: Pos -> Pos -> Bool
incident p q = row p q || column p q || block p q

forbid :: Sudoku -> Pos -> [Val]
forbid s p =  map snd (filter (incident p . fst) s)  

free :: Sudoku -> Pos -> [Val]               -- kandidaten voor positie
free s p = allval `sdiff` (forbid s p)

frees s = [(pos, free s pos) | pos <- undef s] -- alle kandidaten per vrije positie

-- sudoku in- en uitvoer
-- rijsgewijze opsomming van val-waarden, emptyVal voor `niet ingevuld'
toSudoku ::[[Val]] ->Sudoku
toSudoku xss = filter ((/=emptyVal).snd) (zip allpos (concat xss))

-- naar rijsgewijze opsomming van val-waarden, uitsluitend complete sudokus
fromSudoku :: Sudoku -> [[Val]]
fromSudoku =  chop 9 . map snd . sort lexord

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n as = x : chop n y
            where (x,y) = splitAt n as

lexord :: (Ord a,Ord b) => (((a,b),c),((a,b),d)) -> Bool
lexord ( ((x1,y1),z1) , ((x2,y2),z2)) = (x1<x2) || (x1==x2 && y1 <y2)

sort p [] = []
sort p (a:xs) = insert p a (sort p xs)

insert p a [] = [a]
insert p a (b:xs) = if p (a,b) then a:b:xs else b: insert p a xs

------------------------------------------------------------------------
-- t.b.v. oplosproces
------------------------------------------------------------------------
-- specificatie: gmin x as = minimum (x:as)
gmin (p,vs) [] = (p,vs)
gmin (p,vs) ((q,ws):cs) 
  | length vs <= length ws  = gmin (p,vs) cs
  | otherwise               = gmin (q,ws) cs

-- sudoku uitbreiden met cel
addCel :: Sudoku -> Cel -> Sudoku
addCel s c = c : s

-- genereer net zoveel uitbreidingen als er kandidaten zijn
extendStack :: Stack Sudoku -> Sudoku -> Pos -> [Val] -> Stack Sudoku
extendStack st s pos [] = st
extendStack st s pos (v:vs) = push (addCel s (pos, v)) (extendStack st s pos vs)

-- sudoku-invoer doorsluizen naar initiele sudoku stack
startStack ns = push (toSudoku ns) empty

-- sudoku-uitvoer doorsluizen naar prettyprinter        
prettyprint = putStr . header . concat . map list2string. fromSudoku

list2string :: [Val] -> String
list2string [] = "\n"
list2string (a:as) = show a ++ " " ++ list2string as

header s = "\n-----------------\n" ++ s ++ "-----------------\n"   

------------------------------------------------------------------
-- het oplosproces
------------------------------------------------------------------
solver :: Stack Sudoku -> [Sudoku]
solver st
  | isEmpty st  = []
  | otherwise   = let { s = top st
                      ; st' = pop st
                      ; ys = frees s
                      }
                  in
                  case ys of
                  []           -> s : solver st'
                  ((p,vs): cs) -> let (q,ws) = gmin (p,vs) cs
                                  in
                                  solver (extendStack st' s q ws)

-- Oplosproces activeren: 
-- call van ``solve'' met aantal gewenste oplossingen en rijsgewijze Vals
solve n = sequence. map prettyprint.take n.solver.startStack









