module Helper where

import Numeric.IEEE (IEEE, succIEEE, predIEEE, nan)
import Data.List (intercalate)

bottom :: a
bottom = error "bottom"

csv' :: Show a => String -> [a] -> String
csv' s = intercalate s . map show

csv :: Show a => [a] -> String
csv = csv' ","

tsv :: Show a => [a] -> String
tsv = csv' "\t"

data Raw = Raw { unraw :: String }
instance Show Raw where
    show = unraw

cbrt :: Floating a => a -> a
cbrt = (** (1/3))

converge' :: (Num a, Ord a) => a -> a -> [a] -> a
converge' _ _ [x] = x
converge' rtol atol (x:x':xs)
  | abs (x' - x) < atol = x'
  | 2 * abs (x' - x) < rtol * (abs x' + abs x) = x'
  | otherwise = converge' rtol atol (x':xs)

converge :: (Fractional a, Ord a) => [a] -> a
converge = converge' 1e-16 1e-16

epsilonAt :: IEEE a => a -> a -> a
epsilonAt t dir | dir >= 0  = succIEEE t - t
                | otherwise = predIEEE t - t

defaultClipStep :: Double -> Double -> Double -> Double
defaultClipStep f t h
  | h >= 0 && eps >= h = eps
  | h <= 0 && eps <= h = eps
  | otherwise          = h
  where
    eps = f * epsilonAt t h

clipperL :: Double -> (Double -> Double)
clipperL l v | v < l     = l
             | otherwise = v

clipperU :: Double -> (Double -> Double)
clipperU u v | v > u     = u
             | otherwise = v

clipper :: Double -> Double -> (Double -> Double)
clipper l u | l < u     = go l u
            | otherwise = go u l
  where go l u v | v < l     = l
                 | v > u     = u
                 | otherwise = v

-- map nan to something
clipper' :: Double -> Double -> Double -> (Double -> Double)
clipper' l u = go
  where c = clipper l u
        go n v | isNaN v   = n
               | otherwise = c v