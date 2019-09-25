{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module IntegrateMulti where
import System.Random
import Control.DeepSeq

import Stream
import Vector
import Helper
import StableSum

instance (Vector v, Random (VField v)) => Random (VProxy v) where
    random = vthread (const random) (vconst 0)
    randomR (lo,hi) = vthreads go [lo,hi]
      where go [l,h] = randomR (l,h)

integrateMonte :: ( RandomGen g, NFData y, CVector y, CVector x
                  , Random (VField x), Fractional (VField x) )
                  => (x -> y) -> (x,x) -> (x -> Bool) -> g -> StreamMC y
integrateMonte f (lo,hi) dom = smapl vol' . vstats . map f' . rx
  where rx = map unVProxy . randomRs (VProxy lo, VProxy hi)
        vol = product . cabslist $ hi `vsub` lo
        vol' (n, m, v) = (n, cscale vol m, vol * v)
        f' x = if dom x then f x else vconst 0

monteBall :: RandomGen g => Int -> Double -> g -> StreamMC Double
monteBall d r = integrateMonte (const 1) lims dom
  where lims  = (replicate d (-r), replicate d r)
        dom x = cnorm' 2 x <= r^2

montePi :: RandomGen g => g -> StreamMC Double
montePi = monteBall 2 1