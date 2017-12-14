module LambdaStream
     --(lambdaStream) 
         where

import qualified Data.Set as S
import Types
import Combinators
import Reduction

lambdaStream = concatMap lambdas [1..]

lambdas 1 = [sai]
lambdas n = concatMap makeLambdas (splits n)

makeLambdas (l,r) = map (uncurry app) (pairs (lambdas l) (lambdas r))  

splits n = [ (x, n-x) | x<-[1..n-1]]

pairs x y = [(a,b) | a<-x, b<-y]

lambdaStreamNum = zip lambdaStream [1..]


lambdaStream' = concatMap lambdas' [1..]

lambdas' 1 = [StreamLambda sai 1 (1,0)]
--lambdas' n = concatMap makeLambdas' (splits' n)

makeLambdas' (l,r) = map (uncurry app) (pairs (lambdas l) (lambdas r))  

splits' n = [ (x, n-x) | x<-[1..n-1]]

uniqueLambdaStream = h lambdaStream where
    h (x:xs) = let x' = full'' x in
                   x' : h (filter (x' /=) . map full'' $ xs)

uniqueLambdaStreamNum = h $ zip lambdaStream [1..] where
    h ((x,n):xs) = let 
                       x' = full'' x 
                       xs' = map snd xs 
                       full''' (t,n) = (full'' t, n)
                   in
                       (x',n) : h (filter (\(t,n) -> x' /= t) . map full''' $ xs)
    
