module Main where
import ODE
import Test (test)


r vs 1 = sqrt (1-vs)
r vs v0 = let (_,_,r') = sget ode v0 in r'
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
main = mapM_ print $ [(vs,v0,r vs v0) | vs <- vss, v0 <- v0s]
  where vss = [0,0.001..0.5]
        v0s = [0,0.001..1]