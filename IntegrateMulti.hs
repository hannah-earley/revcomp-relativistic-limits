{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module IntegrateMulti where

import System.Random
import Stream
import Vector

newtype RVector v = RVector { unRVector :: v }

instance (Vector v, Random (VField v)) => Random (RVector v) where
    randomR (lo,hi) g = (RVector v', g')
      where lo' = unRVector lo
            hi' = unRVector hi
            (v',g') = vthreads go [lo',hi'] g
              where go [l,h] = randomR (l,h)

    random g = (RVector v', g')
      where (v', g') = vthread (const random) (vconst 0) g

integrateMonte :: ( Random (VField x), Fractional (VField x)
                  , CVector x, CVector y, RandomGen g )
                  => (x -> y) -> (x,x) -> (x -> Bool) -> g -> [y]
integrateMonte f (lo,hi) dom g = map (cscale vol) fxs
  where xs = map unRVector $ randomRs (RVector lo, RVector hi) g
        vol = product . cabslist $ hi `vsub` lo
        fxs = stake stableMean $ map f' xs
        f' x = if dom x then f x else vconst 0

-- Neumaier version of Kahan algorithm
stableSum :: CVector x => StreamF x x
stableSum = go (vconst 0) (vconst 0)
  where
    go s c x = let s' = s `vplus` x
                   bigger  = vzip cmax s x
                   smaller = vzip cmin s x
                   dc = (bigger `vsub` s') `vplus` smaller
                   c' = c `vplus` dc
               in Stream (s' `vplus` c') (go s' c')
    cmax a b = if cabs a > cabs b then a else b
    cmin a b = if cabs a > cabs b then b else a

stableMean :: (Fractional (VField x), CVector x) => StreamF x x
stableMean = go (0 :: Integer, stableSum)
  where go (n,s) x = let Stream t s' = s x
                         n' = n + 1
                         m = vmap (/ fromIntegral n') t
                     in Stream m (go (n',s'))

montePi :: RandomGen g => g -> [Double]
montePi = integrateMonte (const 1) lims dom
  where lims  = ([-1,-1], [1,1] :: [Double])
        dom x = cnorm' 2 x <= 1