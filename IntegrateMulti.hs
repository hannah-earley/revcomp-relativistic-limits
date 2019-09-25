{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module IntegrateMulti where

import System.Random
import Stream
import Vector
import Control.DeepSeq

type StreamFI a = StreamF Integer a

newtype RVector v = RVector { unRVector :: v }

instance Vector v => Vector (RVector v) where
    type VField (RVector v) = VField v
    vconst = RVector . vconst
    vzip f (RVector x) (RVector y) = RVector (vzip f x y)
    vzips f = RVector . vzips f . map unRVector
    vlist = vlist . unRVector
    vmap f = RVector . vmap f . unRVector
    vfold f b = vfold f b . unRVector
    vthread f (RVector v) b = let (v',b') = vthread f v b
                              in (RVector v', b')
    vthreads f vs b = let (v', b') = vthreads f (map unRVector vs) b
                      in (RVector v', b')

instance (Vector v, Random (VField v)) => Random (RVector v) where
    random = vthread (const random) (vconst 0)
    randomR (lo,hi) = vthreads go [lo,hi]
      where go [l,h] = randomR (l,h)

integrateMonte :: ( NFData y, Random (VField x), Fractional (VField x)
                  , CVector x, CVector y, RandomGen g )
                  => (x -> y) -> (x,x) -> (x -> Bool) -> g -> StreamFI (Integer,y,Double)
integrateMonte f (lo,hi) dom g = smapl vol' . stableStats $ map f' xs
  where xs = map unRVector $ randomRs (RVector lo, RVector hi) g
        vol = product . cabslist $ hi `vsub` lo
        vol' (n, m, v) = (n, cscale vol m, vol * v)
        f' x = if dom x then f x else vconst 0

-- Neumaier version of Kahan algorithm
stableSum' :: (NFData y, CVector y) => (x -> y) -> [x] -> StreamFI y
stableSum' f = go (vconst 0) (vconst 0)
  where
    go !s !c xs 0 = Stream (s `vplus` c) (go s c xs)
    go !s !c ~(!x:xs) !n =
        let !y  = force $ f x
            !s' = force $ s `vplus` y
            !bigger  = vzip cmax s y
            !smaller = vzip cmin s y
            !dc = (bigger `vsub` s') `vplus` smaller
            !c' = force $ c `vplus` dc
        in go s' c' xs (force $ n-1)
    cmax !a !b = if cabs a > cabs b then a else b
    cmin !a !b = if cabs a > cabs b then b else a

stableSum :: (NFData x, CVector x) => [x] -> StreamFI x
stableSum = stableSum' id

stableMean' :: (NFData y, Fractional (VField y), CVector y) => (x -> y) -> [x] -> StreamFI (Integer, y)
stableMean' f xs = go (0 :: Integer, stableSum' f xs)
  where go (n,s) dn = let Stream t s' = s dn
                          n' = n + dn
                          m = vmap (/ fromIntegral n') t
                      in Stream (n', m) (go (n',s'))

stableMean :: (NFData x, Fractional (VField x), CVector x) => [x] -> StreamFI (Integer, x)
stableMean = stableMean' id

stableStats :: (NFData x, Fractional (VField x), CVector x) => [x] -> StreamFI (Integer,x,Double)
stableStats = smapl f . stableMean' g
  where
    g !x = x `deepseq` (x, vmap ((^2) . abs) x)
    f (n,(m,m2)) = let var = cnorm1 m2 - cnorm' 2 m
                       sm2 = abs var / fromIntegral (n-1)
                   in (n, m, sqrt sm2)

montePi :: RandomGen g => g -> StreamFI (Integer,Double,Double)
montePi = integrateMonte (const 1) lims dom
  where lims  = ([-1,-1], [1,1] :: [Double])
        dom x = cnorm' 2 x <= 1