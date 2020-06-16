module Main where
import Data.List (inits)
import System.Random (getStdGen)

import Integrate (dsolve')
import IntegrateMulti (montePi,monteBall)
import Optimise (minimise,maximise)
import Stream (sget,stake)
import Helper (clipper,clipper',cbrt,tsv)
import Math

-- power limited reversible
maxVs1 :: Double -> (Double,Double)
maxVs1 sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = (sqrt s) * (relRate s $ clrt (1-s/sm))

-- mass limited
maxVs2 :: Double -> (Double,Double)
maxVs2 sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = s * (relRate s $ clrt (1-s/sm))

-- power limited irreversible
maxVs3 :: Double -> (Double,Double)
maxVs3 sm = maximise rate 0 smax
  where smax   = clipper 0 (8/9) sm
        clrt   = cbrt . clipper' 0 1 0
        rate s = (relRate s $ clrt (1-s/sm))

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
        w =  1 - (v0/v)^3
        m =  1 -  v0   ^3
        x = w + 3*u

        nu'  = 0.5 * v * vs * x / (m - v*v*vs*w)
        nu'' = 1.5 * v * vs * u /  m
        r'   =  -3 * v * v  * g /  m

        go nu' = (-(1+u) * nu', g * nu', r')

main :: IO ()
-- main = do pi <- monteBall 12 1 <$> getStdGen
--           mapM_ print . stake pi $ map (2^) [0..]
main = mapM_ (putStrLn . go) xs
  where --
    revCoeff = 1e2
    irrevCoeff = 1e-3

    go sm = let (s1,r1) = maxVs1 sm
                (s2,r2) = maxVs2 sm
                (s3,r3) = maxVs3 (sqrt sm)
                l = sqrt sm
                r1' = r1 * (l**1.5)
                r2' = r2 * l * revCoeff
                r1'' = (l**2.5) * (relRate sm 0)
                r3' = r3 * l**2 * irrevCoeff
                rm = min r1' r2'
                im = min r2' r3'
            -- in tsv [l,rm]
            in tsv [l,r1',r2',r1'',im,rm]
            -- in show (sm,(s1,r1),(s2,r2),r1'',l,rm,r1'<r2')

    xs1 = map exp [-25,-24.8..(-1)]
    xs2 = map exp [-0.99,-0.98..(-0.05)]
    xs3 = map (read . ("0.957"++)) (inits "3486580461166")
    xs4 = map exp [0,0.2..25]
    xs = xs1 ++ xs2 ++ xs3 ++ xs4
    -- xs = map exp [-25.0,-23.0..25.0]


