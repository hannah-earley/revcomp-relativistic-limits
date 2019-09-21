module Main where

import Integrate (dsolve',clipper,clipper')
import Optimise (maximise)
import Stream (sget)

maxVs :: Double -> (Double,Double)
maxVs sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        rate s = (sqrt s) * (relRate s $ cbrt (1-s/sm))
        clip   = clipper' 0 1 0
        cbrt x = (clip x) ** (1/3)

relRate :: Double -> Double -> Double
relRate vs 1 = sqrt (1-vs)
relRate vs v0 = let (_,_,r') = sget ode v0 in r'
  where
    ode = dsolve' (f v0 vs) 1 (0, sqrt(1-vs), 0)
    f v0 vs v (u,g,r)
      | v <= v0   = go nu''
      | otherwise = go nu'
      where
        x = (1 - v0/v)^3
        w = 1 - (v0/v)^3
        m = 1 - v0^3

        nu' = 0.5 * v * vs * (x + 3*u) / (m - v*v*vs*w)
        nu'' = 1.5 * v * vs * u / m
        r' = -3 * v*v * g / m

        go nu' = (-(1+u) * nu', g * nu', r')

main :: IO ()
main = mapM_ print $ [(sm,maxVs sm) | sm <- ([0,0.02..1] ++ [2..])]