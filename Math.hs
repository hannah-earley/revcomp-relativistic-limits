module Math where
import Data.List (scanl')

import Helper (converge)
import Integrate (dsolve,dsolve')
import Stream (sget,spure,scomp,stake,StreamFD)

type DD  = StreamFD  Double
type DD2 = StreamFD (Double,Double)
type DDD = StreamFD [Double]

type FD  =  Double  -> Double
type FLD = [Double] -> Double
type LD  = [Double]

e'          = sget exp' 1                                  :: Double
pi'         = 4 * sget atan' 1                             :: Double
tau'        = 8 * sget atan' 1                             :: Double
eulergamma' = eulgams' !! magic                            :: Double
  where magic = 2408 -- gives best approximation

square'= dsolve' (\t y -> 2*t)     0 0                     :: DD
sqrt'  = dsolve' (\t y -> 1/(2*y)) 1 1                     :: DD

log'  = dsolve' (\t y -> 1/t) 1 0                          :: DD
exp'  = dsolve' (\t y ->   y) 0 1                          :: DD
expm1'= dsolve' (\t y -> y + 1) 0 0                        :: DD
erfs' = dsolve' f 0 [1,0,1]                                :: DDD
  where sp = 2 / sget sqrt' pi'
        f t [x,y,z] = [-2*t*x,sp*x,-sp*x]
erf' = spure (!!1) `scomp` erfs'
erfc' = spure (!!2) `scomp` erfs'

trig  = dsolve' (\t (y,z) -> (z,-y)) 0 (0,1)               :: DD2
sin'  = spure fst `scomp` trig                             :: DD
cos'  = spure snd `scomp` trig                             :: DD
asin' = dsolve' (\t y ->  1/sqrt(1-t^2)) 0 0               :: DD
acos' = dsolve' (\t y -> -1/sqrt(1-t^2)) 0 (pi'/2)         :: DD
atan' = dsolve' (\t y ->  1/    (1+t^2)) 0 0               :: DD

htrig = dsolve' (\t (y,z) -> (z,y)) 0 (0,1)                :: DD2
sinh' = spure fst `scomp` htrig                            :: DD
cosh' = spure snd `scomp` htrig                            :: DD
asinh'= dsolve' (\t y -> 1/sqrt(t^2+1)) 0              0   :: DD
acosh'= dsolve' (\t y -> 1/sqrt(t^2-1)) (sget cosh' 1) 1   :: DD
atanh'= dsolve' (\t y -> 1/    (1-t^2)) 0              0   :: DD

limInf x x0 = map (\h -> x + x0*exp h) [0,-1..]            :: LD
limSup x x0 = map (\h -> x - x0*exp h) [0,-1..]            :: LD
conv = converge . take 100 . takeWhile (not . isNaN)       :: FLD
convLim ode lim = conv $ stake ode lim                     :: Double

igamma' x t0 = dsolve' f t0 (y0,0)                         :: DD2
  where y0 = sget exp' (-t0)
        f t (y,z) = (-y, t**(x-1) * y)
lgamma' = dsolve' (const . digamma') 1 0                   :: DD

lgamma :: Integral a => a -> Double
lgamma n = sum $ map (log . fromIntegral) [1..n-1]

gamma' :: FD
gamma' x = convLim ode' $ limInf 0 1
  where ode' = spure snd `scomp` ode
        ode = dsolve' f 1 (0,0)
        f t (y,z) = (1/t, -p y)
        p = (** (x-1)) . abs

harmonic'' :: Double -> FD
harmonic'' h0 x = convLim ode $ limSup 1 1
  where ode = dsolve' f 0 h0
        f t y = (1 - p t) / (1 - t)
        p = (** (x-1)) . abs

digamma'  = harmonic'' (-eulergamma')                      :: FD
harmonic' = harmonic'' 0                                   :: FD
harmonics'  = scanl' (+) 0 $ map (1/) [1..]                :: LD
harmonics2' = scanl' (+) 0 $ map go   [1..]                :: LD
  where go n = sum $ map (1/) [n*(n-1)+1..n*(n+1)]
eulgams' = tail $ zipWith3 go [0..] harmonics' harmonics2' :: LD
  where go n hn hn2 = 2*hn - hn2 + (n-1)/(6*n^3)