{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module IntegrateMulti where

import System.Random
import Stream
import Vector

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

integrateMonte :: ( Random (VField x), Fractional (VField x)
                  , CVector x, CVector y, RandomGen g )
                  => (x -> y) -> (x,x) -> (x -> Bool) -> g -> StreamFI y
integrateMonte f (lo,hi) dom g = go . stableMean $ map f' xs
  where xs = map unRVector $ randomRs (RVector lo, RVector hi) g
        vol = product . cabslist $ hi `vsub` lo
        f' x = if dom x then f x else vconst 0
        go s n = let Stream m s' = s n
                 in Stream (cscale vol m) (go s')

-- Neumaier version of Kahan algorithm
stableSum :: CVector x => [x] -> StreamFI x
stableSum = go (vconst 0) (vconst 0)
  where
    go !s !c xs 0 = Stream (s `vplus` c) (go s c xs)
    go !s !c ~(!x:xs) !n =
        let !s' = s `vplus` x
            !bigger  = vzip cmax s x
            !smaller = vzip cmin s x
            !dc = (bigger `vsub` s') `vplus` smaller
            !c' = c `vplus` dc
        in go s' c' xs (n-1)
    cmax !a !b = if cabs a > cabs b then a else b
    cmin !a !b = if cabs a > cabs b then b else a

stableMean :: (Fractional (VField x), CVector x) => [x] -> StreamFI x
stableMean xs = go (0 :: Integer, stableSum xs)
  where go (n,s) dn = let Stream t s' = s dn
                          n' = n + dn
                          m = vmap (/ fromIntegral n') t
                      in Stream m (go (n',s'))

montePi :: RandomGen g => g -> StreamFI Double
montePi = integrateMonte (const 1) lims dom
  where lims  = ([-1,-1], [1,1] :: [Double])
        dom x = cnorm' 2 x <= 1