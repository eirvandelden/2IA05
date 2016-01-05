module Stack ( Stack     
             , push        -- a -> Stack a -> Stack a
             , pop         -- Stack a -> Stack a
             , top         -- Stack a -> a
             , empty       -- Stack a
             , isEmpty     -- Stack a -> Bool
             ) where
           
-------------------------------------------------------------------
data Stack a = EmptyStk | Stk a (Stack a) 
               deriving (Eq,Ord)

instance (Show a) => Show (Stack a) where
  show EmptyStk = "-"
  show(Stk x s) = show x ++ " | " ++ show s

-------------------------------------------------------------------
push = Stk 

pop EmptyStk  = error "pop from an empty stack"
pop (Stk _ s) = s

top EmptyStk  = error "top from an empty stack"
top (Stk x _) = x

empty = EmptyStk

isEmpty EmptyStk  = True
isEmpty (Stk _ _) = False
