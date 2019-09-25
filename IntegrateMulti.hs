{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module IntegrateMulti where
import System.Random
import Control.DeepSeq

import Stream
import Vector
import Helper (first)

type StreamFI a = StreamF Integer a
type StreamMC a = StreamFI (Integer, a, Double)

newtype RVector v = RVector { unRVector :: v }

instance Vector v => Vector (RVector v) where
    type VField (RVector v) = VField v
    vconst = RVector . vconst
    vzip f (RVector x) (RVector y) = RVector (vzip f x y)
    vzips f = RVector . vzips f . map unRVector
    vlist = vlist . unRVector
    vmap f = RVector . vmap f . unRVector
    vfold f b = vfold f b . unRVector
    vthread f (RVector v) = first RVector . vthread f v
    vthreads f vs = first RVector . vthreads f (map unRVector vs)

instance (Vector v, Random (VField v)) => Random (RVector v) where
    random = vthread (const random) (vconst 0)
    randomR (lo,hi) = vthreads go [lo,hi]
      where go [l,h] = randomR (l,h)

integrateMonte :: ( RandomGen g, NFData y, CVector y, CVector x
                  , Random (VField x), Fractional (VField x) )
                  => (x -> y) -> (x,x) -> (x -> Bool) -> g -> StreamMC y
integrateMonte f (lo,hi) dom g = smapl vol' . stableStats $ map f' xs
  where xs = map unRVector $ randomRs (RVector lo, RVector hi) g
        vol = product . cabslist $ hi `vsub` lo
        vol' (n, m, v) = (n, cscale vol m, vol * v)
        f' x = if dom x then f x else vconst 0

-- Neumaier version of Kahan algorithm
stableSum :: (NFData x, CVector x) => [x] -> StreamFI x
stableSum = go (vconst 0) (vconst 0)
  where
    go !s !c xs 0 = Stream (s `vplus` c) (go s c xs)
    go !s !c ~(!x:xs) !n =
        let !s' = force $ s `vplus` x
            !bigger  = vzip cmax s x
            !smaller = vzip cmin s x
            !dc = (bigger `vsub` s') `vplus` smaller
            !c' = force $ c `vplus` dc
        in go s' c' xs (force $ n-1)
    cmax !a !b = if cabs a > cabs b then a else b
    cmin !a !b = if cabs a > cabs b then b else a

stableMean :: (NFData x, Fractional (VField x), CVector x)
              => [x] -> StreamFI (Integer, x)
stableMean xs = go (0 :: Integer, stableSum xs)
  where go (n,s) dn = let Stream t s' = s dn
                          n' = n + dn
                          m = vmap (/ fromIntegral n') t
                      in Stream (n', m) (go (n',s'))

stableStats :: (NFData x, Fractional (VField x), CVector x)
               => [x] -> StreamMC x
stableStats = smapl f . stableMean . map g
  where
    g !x = x `deepseq` (x, vmap ((^2) . abs) x)
    f (n,(m,m2)) = let var = cnorm1 m2 - cnorm' 2 m
                       sm2 = abs var / fromIntegral (n-1)
                   in (n, m, sqrt sm2)

monteBall :: RandomGen g => Int -> Double -> g -> StreamMC Double
monteBall d r = integrateMonte (const 1) lims dom
  where lims  = (replicate d (-r), replicate d r)
        dom x = cnorm' 2 x <= r2
        r2 = r^2

montePi :: RandomGen g => g -> StreamMC Double
montePi = monteBall 2 1