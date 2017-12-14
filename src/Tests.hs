module Tests where

import Types
import Solution

lam = Lam
sym = Sym

infixl 1 *.*
(*.*) = App

iff = Iff 

bt = Boolean True
n5 = Natural 5

x = "x"
y = "y"
z = "z"
a = "a"
b = "b"
c = "c"
f = "f"

test0 = map typeOf [Boolean True, Not $ Boolean True, Not $ Natural 3]
test1 = map typeOf [And bt bt, And n5 bt, And bt n5, And n5 n5] 
test10 = map typeOf [Or bt bt, Or n5 bt, Or bt n5, Or n5 n5] 
test2 = map typeOf [Iff bt bt bt, Iff bt n5 n5, 
                    Iff bt n5 bt, Iff n5 n5 n5, Iff n5 bt bt] 
test3 = map typeOf [Add n5 n5, Add n5 bt, Add bt n5, Add bt bt] 
test4 = map typeOf [Sym x]
test5 = map typeOf lams
lams =             [
                    lam x Bool (Not $ sym x),
                    lam x Nat (Add (sym x) (sym x)),
                    lam y Nat  $ lam x Nat  (Mult (sym x) (sym x)),
                    lam y Bool $ lam x Nat (Mult (sym x) (sym x)),

                    lam y Bool $ lam x Nat (Mult (sym x) (sym y)),
                    lam y Nat  $ lam x Bool (Mult (sym x) (sym x)),
                    lam x Nat  (Not (sym x)),
                    lam x Bool (Add (sym x) (sym x))
                   ]

test6 = map typeOf [
                    lams !! 0 *.* bt,
                    lams !! 1 *.* n5,
                    lams !! 2 *.* n5 *.* n5,
                    lams !! 2 *.* n5,
                    lams !! 3 *.* bt *.* n5,
                    Add n5 $ lams !! 3 *.* bt *.* n5,
                    lam z Nat $ Add (Sym z) $ lams !! 3 *.* bt *.* n5,

                    lam z Nat $ And (Sym z) $ lams !! 3 *.* bt *.* n5,
                    lam z Bool $ Add (Sym z) $ lams !! 3 *.* bt *.* n5,
                    Not $ lams !! 3 *.* bt *.* n5,
                    Not $ lams !! 3 *.* bt *.* n5,
                    lams !! 7 *.* bt,
                    lams !! 6 *.* n5,
                    lams !! 5 *.* n5,
                    lams !! 4 *.* bt *.* n5,
                    lams !! 2 *.* n5 *.* n5,
                    lams !! 2 *.* bt,
                    lams !! 2 *.* bt *.* n5,
                    lams !! 1 *.* bt,
                    lams !! 0 *.* n5
                   ]

pnb  = Pair bt n5
pbb  = Pair bt bt
pnn  = Pair n5 n5
pnp  = Pair n5 pbb
ppp  = Pair pnb pbb

test7 = map typeOf [
                    pnb, pnb, pnn, pnp, ppp
                   ]
         
test8 = map typeOf [
                    Snd pnb, 
                    Fst pnb, 
                    Fst pnn, 
                    Fst ppp, 
                    Snd $ Fst ppp, 
                    
                    Fst (pnn *.* pnp),
                    Fst bt,
                    Snd (lams !! 3)
                   ]

test9 = map typeOf [
                    Pair (lams !! 3) bt,
                    lam x (PairT Bool Nat) $ And (Not (Fst $ Sym x)) (Fst $ Sym x),
                    lam x (Bool) $ lam y (Nat) $ lam z (Bool) $ Pair (Sym x) (Pair (Sym y) (Sym z)),
                    (lam x (Bool) $ lam y (Nat) $ lam z (Bool) $ Pair (Sym x) (Pair (Sym y) (Sym z))) *.* bt *.* n5 *.* bt,
                    lam y (Fun Bool Bool) $ lam z (Bool) $ Pair (Sym y) (Sym z),

                    (lam y (Nat) $ lam z (Bool) $ Pair (Sym x) (Pair (Sym y) (Sym z))) *.* n5 *.* bt,
                    (lam x (Bool) $ lam y (Nat) $ lam z (Bool) $ Pair (Sym x) (Pair (Sym y) (Sym z))) *.* bt *.* bt *.* bt,
                    lams !! 2 *.* pnb *.* pnn,
                    Pair (lams !! 1) (lams !! 7),
                    pnb *.* pbb
                   ]

eln = Nil Nat
elb = Nil Bool

test11 = map typeOf [
                     eln, elb,
                     Cons (lams !! 0) (Nil (Fun Bool Bool)),
                     IsNil $ Cons (lams !! 0) (Nil (Fun Bool Bool)),
                     Head $ Cons (lams !! 0) (Nil (Fun Bool Bool)),
                     Tail $ Tail $ Tail $ Head $ Cons (lams !! 0) (Nil (Fun Bool Bool)),

                     IsNil bt,
                     Cons (lams !! 0) (Nil (Bool))
                    ]
