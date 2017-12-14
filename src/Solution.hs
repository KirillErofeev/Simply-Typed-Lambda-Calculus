module Solution where

import Control.Monad.State.Lazy
import qualified Data.Map as Map (lookup, empty)
import Data.Map (Map(..), insert)
import Types
--import Data.Either (fromRight)
import Data.Either.Unwrap (fromRight)

type Context = Map Symbol Type

errorExpAct exp act = Left $ "Error! Expected type: '" ++ show exp ++ 
                                  "' but actual type: '" ++ show act ++ "'"
errorVarNotInScope x = Left $ "Error! Variable " ++ x ++ " is not in the scope!"

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) m k = Map.lookup k m

typeIfIsSame :: Term -> Term -> State Context (Either String Type)
typeIfIsSame t0 t1 = do
    et0 <- typeOf' t0
    et1 <- typeOf' t1
    return $ case (et0, et1) of
        (Right tx, Right ty) -> if tx == ty 
                                then Right tx 
                                else errorExpAct tx ty 
        (m1, m2)             -> m1 >> m2

typeOf :: Term -> Either String Type
typeOf t = evalState (typeOf' t) Map.empty

{-Lambda-}
typeOf' :: Term -> State Context (Either String Type)
typeOf' (Sym x) = do
    typeX <- gets (!? x)
    return $ case typeX of
        Nothing -> errorVarNotInScope x
        Just x  -> Right x

typeOf' (Lam sym t term) = do
    modify (insert sym t)
    et1 <- typeOf' term
    return $ case et1 of
        Right t1 -> Right $ Fun t t1
        Left m   -> Left m

typeOf' (App t0 t1) = do
    et0 <- typeOf' t0
    et1 <- typeOf' t1
    return $ case (et0, et1) of
        (Right (Fun t11 t12), Right t11') -> if t11 == t11'
                                             then Right t12
                                             else errorExpAct (Fun t11' A) (Fun t11 t12)
        (Right x, Right y)                -> errorExpAct (Fun y A) x
        (m1, m2)                          -> m1 >> m2

{-Bool-}
typeOf' (Boolean _) = return $ Right Bool
typeOf' (Not b)     = do
    et <- typeOf' b
    return $ case et of
       Right Bool -> Right Bool
       Left x     -> Left x
       Right x    -> errorExpAct Bool x

typeOf' (And t0 t1) = do 
    tp0 <- typeOf' (Not t0)
    tp1 <- typeOf' (Not t1)
    return $ tp0 >> tp1

typeOf' (Or t0 t1) = typeOf' $ And t0 t1

typeOf' (Iff b t0 t1) = do
    tpb  <- typeOf' (Not b)
    tp01 <- typeIfIsSame t0 t1
    return $ tpb >> tp01

{-Nat-}
typeOf' (Natural _) = return $ Right Nat
typeOf' (Add x y)   = do
    tx <- typeOf' x 
    ty <- typeOf' y 
    return $ case (tx, ty) of
        (Right Nat, Right Nat) -> Right Nat
        (Right x, Right y)     -> if x == Nat 
                                  then errorExpAct Nat y
                                  else errorExpAct Nat x
        (Left m, _)            -> Left m

typeOf' (Mult x y) = typeOf' $ Add x y

typeOf' (Pair t0 t1) = do
    et0 <- typeOf' t0
    et1 <- typeOf' t1
    return $ case (et0 >> et1) of
        Right _ -> Right $ PairT (fromRight et0) (fromRight et1)
        Left m  -> Left m

typeOf' (Fst t) = do
    et <- typeOf' t
    return $ case et of 
        Right (PairT tl _) -> Right tl
        Right x             -> errorExpAct (PairT A B) x
        x                   -> x

typeOf' (Snd t) = do
    et <- typeOf' t
    return $ case et of 
        Right (PairT _ tr) -> Right tr
        Right x             -> errorExpAct (PairT A B) x
        x                   -> x

typeOf' (Cons te tl) = do
    ete <- typeOf' te
    etl <- typeOf' tl
    return $ case (ete, etl) of
        (Right t, Right (List t')) -> if t == t'
                                      then Right $ List t
                                      else errorExpAct t' t
        (Right t, Right t')        -> errorExpAct (List t) t'
        (x, y)                     -> x >> y

typeOf' (Nil t)   = return $ Right $ List t

typeOf' (IsNil t) = do
    tl <- typeOf' t
    return $ case tl of
        Right (List _) -> Right Bool
        Right x        -> errorExpAct (List A) x
        x              -> x

typeOf' (Head t) = do
    tl <- typeOf' t
    return $ case tl of
        Right (List te) -> Right te
        Right x         -> errorExpAct (List A) x
        x               -> x

typeOf' (Tail t) = do
    tl <- typeOf' t
    return $ case tl of
        Right (List te) -> Right $ List te
        Right x         -> errorExpAct (List A) x
        x               -> x
-- > typeOf $ Lam "x" Nat $ Add (Sym "x") (Natural 5)
-- Right (Fun Nat Nat)

-- > typeOf $ Lam "x" Bool $ Sym "x"
-- Right (Fun Bool Bool)

-- > typeOf $ Add (Natural 5) (Boolean False)
-- Left "..."

-- > typeOf $ App (Lam "x" Nat $ Sym "x") (Natural 5)
-- Right Nat

-- > typeOf $ App (Lam "x" Nat $ Boolean False) (Natural 5)
-- Right Bool

-- > typeOf $ App (Lam "x" Bool $ Boolean False) (Natural 5)
-- Left "..."

-- > typeOf $ Nil Nat
-- Right (List Nat)

-- > typeOf $ Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat
-- Left "..."

