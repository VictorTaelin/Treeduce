-- This is the Haskell program we're implementing on busy.c

data Nat        = S Nat | Z deriving Show
data Bits       = O Bits | I Bits | E deriving Show
data Pair a b   = P a b deriving Show
inc E           = E
inc (O x)       = (I x)
inc (I x)       = O (inc x)
add E     E     = E
add E     (O b) = O b
add E     (I b) = I b
add (O a) E     = O a
add (O a) (O b) = O (add a b)
add (O a) (I b) = I (add a b)
add (I a) E     = I a
add (I a) (O b) = I (add a b)
add (I a) (I b) = O (inc (add a b))
dup Z           = P Z Z
dup (S x)       = mapS (dup x)
mapS (P a b)    = P (S a) (S b)
slow Z          = (I (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O E))))))))))))))))))))))))))))))))
slow (S x)      = slowGo (dup x)
slowGo (P a b)  = add (slow a) (slow b)
main            = print (P (slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))) (slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))
