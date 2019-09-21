module Main where
import ODE
import Test (test)


maxVs :: Double -> (Double,Double)
maxVs 0 = (0, 0)
maxVs sm = gss 1e-16 rate 0 (clipper 0 (8/9) sm)
  where rate s = (sqrt s) * (relRate s $ cbrt (1-s/sm))
        clip = clipper 0 1
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

-- golden section search for maximum of concave function
gss :: (Floating a, Ord a, Ord t) => a -> (a -> t) -> a -> a -> (a, t)
gss tol f a b = go a' (c, f c) (d, f d) b'
  where
    a' = min a b
    b' = max a b
    h = b' - a'
    c = a' + invphi2 * h
    d = a' + invphi  * h

    invphi  = 0.5 * (sqrt 5 - 1)
    invphi2 = 0.5 * (3 - sqrt 5)

    go a (c, yc) (d, yd) b
      | h <= tol  = if yc >= yd then (c,yc) else (d,yd)
      | yc >= yd  = go a (c', f c') (c, yc) d
      | otherwise = go c (d, yd) (d', f d') b
      where
        h = b - a
        h' = invphi*h
        c' = a + invphi2 * h'
        d' = c + invphi  * h'



main :: IO ()
main = mapM_ print $ [(sm,maxVs sm) | sm <- ([0,0.02..1] ++ [2..])]
-- main = mapM_ print $ [(vs,v0,r vs v0) | vs <- vss, v0 <- v0s]
--   where vss = [0,0.001..0.5]
--         v0s = [0,0.001..1]