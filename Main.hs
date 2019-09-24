module Main where
import Data.List (inits)
import System.Random (getStdGen)

import Integrate (dsolve')
import IntegrateMulti (montePi)
import Optimise (minimise,maximise)
import Stream (sget,stake)
import Helper (clipper,clipper',cbrt,tsv)
import Math

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
main = do pi <- montePi <$> getStdGen
          mapM_ print . stake pi $ map (10^) [0..]
main' = mapM_ (putStrLn . go) xs
  where
    go sm = let (s1,r1) = maxVs1 sm
                (s2,r2) = maxVs2 sm
                l = sqrt sm
                r1' = r1 * (l**1.5)
                r2' = r2 * l * 1e2
                r3' = (l**2.5) * (relRate sm 0)
                rm = min r1' r2'
            -- in tsv [l,rm]
            -- in tsv [l,r1',r2',r3',rm]
            in show (sm,(s1,r1),(s2,r2),r3',l,rm,r1'<r2')

    xs1 = map exp [-25,-24.8..(-1)]
    xs2 = map exp [-0.99,-0.98..(-0.05)]
    xs3 = map (read . ("0.957"++)) (inits "3486580461166")
    xs4 = map exp [0,0.2..25]
    xs = xs1 ++ xs2 ++ xs3 ++ xs4


