module Main where

import Integrate (dsolve')
import Optimise (maximise)
import Stream (sget)
import Helper (clipper,clipper',cbrt,tsv)

maxVs1 :: Double -> (Double,Double)
maxVs1 sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = (sqrt s) * (relRate s $ clrt (1-s/sm))

maxVs2 :: Double -> (Double,Double)
maxVs2 sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = s * (relRate s $ clrt (1-s/sm))

relRate :: Double -> Double -> Double
-- Schwarzschild interior solution
relRate s 0 = (3/16) * (rs*g1*(1+6*s) - asin rs) / rs^3
  where rs = sqrt s
        g1 = sqrt (1-s)

-- thin shell solution
relRate vs 1  = sqrt (1 - vs)

-- numerical solution
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
main = mapM_ (putStrLn.go) $ (map exp $ [-25,-24.8..(-1)] ++ [-0.99,-0.98..(-0.05)]) ++ [0.957,0.9573,0.95734,0.957348,0.9573486,0.95734865,0.957348658,0.9573486580,0.95734865804,0.957348658046,0.9573486580461,0.95734865804611,0.957348658046116,0.9573486580461166] ++ (map exp [0,0.2..25])
  where go sm = let (s1,r1) = maxVs1 sm
                    (s2,r2) = maxVs2 sm
                    l = sqrt sm
                    r1' = r1 * (l**1.5)
                    r2' = r2 * l * 1e2
                    r3' = (l**2.5) * (relRate sm 0)
                    rm = min r1' r2'
                -- in tsv [l,rm]
                -- in tsv [l,r1',r2',r3',rm]
                in show (sm,(s1,r1),(s2,r2),r3',l,rm,r1'<r2')
