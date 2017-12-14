module Solver where

import Data.Numbers.Primes

import LambdaStream
import Combinators
import Reduction


find t n = take n $ filter (eq t) $ lambdaStream
findLN t l r = filter (eqLN t) $ take r $ drop l $ lambdaStream
findN t n = take n $ filter (\(t',n) -> t' `eq` t) $ lambdaStreamNum
findNU t n = take n $ filter (\(t',n) -> t `eq` t') $ lambdaStreamNum



