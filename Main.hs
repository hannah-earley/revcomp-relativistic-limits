module Main where

import Integrate (dsolve')
import Optimise (maximise)
import Stream (sget)
import Helper (clipper,clipper',cbrt)

maxVs :: Double -> (Double,Double)
maxVs sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = (sqrt s) * (relRate s $ clrt (1-s/sm))

relRate :: Double -> Double -> Double
relRate vs 1  = sqrt (1 - vs)
relRate vs v0 = r0
  where
    ugr1 = (0, sqrt (1 - vs), 0)
    ode = dsolve' ugr' 1 ugr1
    (u0, g0, r0) = sget ode v0

    ugr' v (u, g, r)
      | v <= v0   = go nu''
      | otherwise = go nu'
      where
        x = (1 -  v0/v)^3 + 3*u
        w =  1 - (v0/v)^3
        m =  1 -  v0   ^3

        nu'  = 0.5 * v * vs * x / (m - v*v*vs*w)
        nu'' = 1.5 * v * vs * u /  m
        r'   =  -3 * v * v  * g /  m

        go nu' = (-(1+u) * nu', g * nu', r')

main :: IO ()
-- main = mapM_ print $ [(sm,maxVs sm) | sm <- ([0,0.02..1] ++ [2..])]
main = mapM_ print $ [(sm,maxVs sm) | sm <- map exp [-20,-19.8..20]]