module Math where
import Data.List (scanl')
import Data.Complex (Complex((:+)),realPart,imagPart)

import Helper (converge,clipper')
import Integrate (dsolve,dsolve',residue')
import Stream (sget,spure,scomp,stake,sseek,sseq,StreamFD)

type DD  = StreamFD  Double
type DD2 = StreamFD (Double,Double)
type DDD = StreamFD [Double]

type FD  =  Double  -> Double
type FLD = [Double] -> Double
type LD  = [Double]

type DC  = StreamFD (Complex Double)

--- limits

limInf x x0 = map (\h -> x + x0*exp h) [0,-1..]            :: LD
limSup x x0 = map (\h -> x - x0*exp h) [0,-1..]            :: LD
limPInfty x0 = map (\h -> x0 - 1 + exp h) [0..]            :: LD
limNInfty x0 = map (\h -> x0 - 1 - exp h) [0..]            :: LD
conv = converge . take 100 . takeWhile (not . isNaN)       :: FLD
convLim ode lim = conv $ stake ode lim                     :: Double

--- constants

e'          = sget exp' 1                                  :: Double
pi'         = 4 * sget atan' 1                             :: Double
tau'        = 8 * sget atan' 1                             :: Double
eulergamma' = eulgams' !! magic                            :: Double
  where magic = 2408 -- gives best approximation

--- simple

square'= dsolve' (\t y -> 2*t)     0 0                     :: DD
sqrt'  = dsolve' (\t y -> 1/(2*y)) 1 1                     :: DD

--- exponential

log'  = dsolve' (\t y -> 1/t) 1 0                          :: DD
exp'  = dsolve' (\t y ->   y) 0 1                          :: DD
expm1'= dsolve' (\t y -> y + 1) 0 0                        :: DD
erfs' = dsolve' f 0 [1,0,1]                                :: DDD
  where sp = 2 / sget sqrt' pi'
        f t [x,y,z] = [-2*t*x,sp*x,-sp*x]
erf'  = spure (!!1) `scomp` erfs'                          :: DD
erfc' = spure (!!2) `scomp` erfs'                          :: DD
lamw' = dsolve' f 0 0                                      :: DD
  where f z w | abs z < 0.1 = 1 / (z + exp w)
              | otherwise   = w / (z * (1 + w))

--- trigonometric

trig  = dsolve' (\t (y,z) -> (z,-y)) 0 (0,1)               :: DD2
sin'  = spure fst `scomp` trig                             :: DD
cos'  = spure snd `scomp` trig                             :: DD
tan'' = dsolve' (\t (y,z) -> (y*z, y*y)) 0 (1,0)           :: DD2
sec'  = spure fst `scomp` tan''                            :: DD
tan'  = spure snd `scomp` tan''                            :: DD
cot'' = dsolve' f (pi'/4) (sget sqrt' 2, 1)                :: DD2
  where f t (y,z) = (-y*z, -y*y)
csc'  = spure fst `scomp` cot''                            :: DD
cot'  = spure snd `scomp` cot''                            :: DD
asin' = dsolve' (\t y ->  1/sqrt(1-t^2)) 0 0               :: DD
acos' = dsolve' (\t y -> -1/sqrt(1-t^2)) 0 (pi'/2)         :: DD
atan' = dsolve' (\t y ->  1/    (1+t^2)) 0 0               :: DD
sinc' = spure fst `scomp` dsolve' f 0 (1,0)                :: DD
  where f t (y,z) = (c$ z/t^2, -t^2*y)
        c = clipper' (-l) l 0
        l = 1e20
    -- note, unstable when integrating towards 0!
asec'' = dsolve' f 2 [sget sqrt' 3, pi'/3, pi'/6]          :: DDD
  where f t [s, x, y] = [t/s, 1/(t*s), -1/(t*s)]
asec'  = spure (!!1) `scomp` asec''                        :: DD
acsc'  = spure (!!2) `scomp` asec''                        :: DD
acot'  = dsolve' (\t y -> -1/(1+t^2)) 1 (pi'/4)            :: DD

--- hyperbolic

htrig = dsolve' (\t (y,z) -> (z,y)) 0 (0,1)                :: DD2
sinh' = spure fst `scomp` htrig                            :: DD
cosh' = spure snd `scomp` htrig                            :: DD
tanh''= dsolve' (\t (y,z) -> (-y*z, y*y)) 0 (1,0)          :: DD2
sech' = spure fst `scomp` tanh''                           :: DD
tanh' = spure snd `scomp` tanh''                           :: DD
coth''= dsolve' f (sget log' 2) (4/3, 5/3)                 :: DD2
  where f t (y,z) = (-y*z, -y*y)
csch' = spure fst `scomp` coth''                           :: DD
coth' = spure snd `scomp` coth''                           :: DD
asinh'= dsolve' (\t y -> 1/sqrt(t^2+1)) 0              0   :: DD
acosh'= dsolve' (\t y -> 1/sqrt(t^2-1)) (sget cosh' 1) 1   :: DD
atanh'= dsolve' (\t y -> 1/    (1-t^2)) 0              0   :: DD
asech'= spure snd `scomp` dsolve' f (4/5) init             :: DD
  where f t (s,y) = (-t/s, -1/(t*s))
        init = (3/5, sget log' 2)
acsch'= spure snd `scomp` dsolve' f (4/3) init             :: DD
  where f t (s,y) = (t/s, -1/(t*s))
        init = (5/3, sget log' 2)
acoth'= dsolve' (\t y -> 1/(1-t^2)) (5/3) (sget log' 2)    :: DD

gudermannian' = spure (!!2) `scomp` dsolve' f 0 [1,0,0]    :: DD
  where f t [y,z,g] = [-y*z, y*y, y]

--- gamma function etc

igamma' x t0 = dsolve' f t0 (y0,0)                         :: DD2
  where y0 = sget exp' (-t0)
        f t (y,z) = (-y, t**(x-1) * y)
lgamma'' = dsolve' (const . digamma') 1 0                  :: DD

lgamma :: Integral a => a -> Double
lgamma n = sum $ map (log . fromIntegral) [1..n-1]

lgamma' :: Double -> Double
lgamma' x | 2 <= x && x <= 3 = log (gamma'' x)
          | x < 2            = lgamma' (x + 1) - log x
          | x > 3            = lgamma' (x - 1) + log (x - 1)

gpow x = (** (x - 1)) . abs                                :: FD

gamma'' :: FD
gamma'' x = convLim ode' $ limInf 0 1
  where ode' = spure snd `scomp` ode
        ode = dsolve' f 1 (0,0)
        f t (y,z) = (1/t, -gpow x y)

gamma' :: FD
gamma' x | 2 <= x && x <= 3 = gamma'' x
         | x < 2            = gamma' (x + 1) / x
         | x > 3            = gamma' (x - 1) * (x - 1)

rgamma' :: FD
rgamma' = exp . negate . lgamma'

harmonic'' :: Double -> FD
harmonic'' h0 x = convLim ode $ limSup 1 1
  where ode = dsolve' f 0 h0
        f t y = (1 - gpow x t) / (1 - t)

digamma'  = harmonic'' (-eulergamma')                      :: FD
harmonic' = harmonic'' 0                                   :: FD
harmonics'  = scanl' (+) 0 $ map (1/) [1..]                :: LD
harmonics2' = scanl' (+) 0 $ map go   [1..]                :: LD
  where go n = sum $ map (1/) [n*(n-1)+1..n*(n+1)]
eulgams' = tail $ zipWith3 go [0..] harmonics' harmonics2' :: LD
  where go n hn hn2 = 2*hn - hn2 + (n-1)/(6*n^3)

polygamma' m z = -(-1)^m * (int1 - int2) :: Double
  where ode = dsolve' f 1 0
        f t y = t^m * exp (-z*t) / (1 - exp(-t))
        int1 = convLim ode (limPInfty 2)
        int2 = convLim ode (limInf 0 0.5)

--- special functions

airy'' inits = spure fst `scomp` dsolve' (\t (y,z) -> (z,t*y)) 0 inits :: DD
airyAi' = airy'' (3**(-2/3) / gamma' (2/3), -3**(-1/3) / gamma' (1/3)) :: DD
airyBi' = airy'' (3**(-1/6) / gamma' (2/3),  3**( 1/6) / gamma' (1/3)) :: DD

bessel' :: Double -> (Double,Double) -> DD
bessel' a (y1,y1') = spure (!!1) `scomp` ode `scomp` x2t
  where f t [e, y, z] = [-2*e, z, (a^2 - e)*y]
        ode = dsolve' f 0 [1, y1, -y1']
        x2t = spure (negate . log)

besselJn'' :: Integral a => a -> FD
besselJn'' a x = sget ode pi' / pi'
  where ode = dsolve' (\t y -> cos (a'*t - x*sin t)) 0 0
        a' = fromIntegral a

besselJ'' :: Double -> FD
besselJ'' a x = (int1 - int2) / pi'
  where ode1 = dsolve' (\t y -> cos (a*t - x*sin t)) 0 0
        ode2 = dsolve' (\t y -> exp (-x*sinh t-a*t)) 0 0
        int1 = sget ode1 pi'
        int2 = sin (a*pi') * convLim ode2 (limPInfty 0)

dbesselJn'' :: Integral a => a -> FD
dbesselJn'' a x = sget ode pi' / pi'
  where ode = dsolve' (\t y -> (sin t) * sin (a'*t - x*sin t)) 0 0
        a' = fromIntegral a

dbesselJ'' :: Double -> FD
dbesselJ'' a x = (int1 - int2) / pi'
  where ode1 = dsolve' (\t y -> (sin t) * sin (a*t - x*sin t)) 0 0
        ode2 = dsolve' (\t y -> -(sinh t) * exp (-x*sinh t-a*t)) 0 0
        int1 = sget ode1 pi'
        int2 = sin (a*pi') * convLim ode2 (limPInfty 0)

besselJ'  a = bessel' a  (besselJ''  a 1, dbesselJ''  a 1) :: DD
besselJn' a = bessel' a' (besselJn'' a 1, dbesselJn'' a 1) :: DD
  where a' = fromIntegral a

besselY'' :: Double -> FD
besselY'' a x = (int1 - int2) / pi'
  where ode1 = dsolve' (\t y -> sin (x*sin t - a*t)) 0 0
        ode2 = dsolve' (\t y -> (exp (a*t) + (cos (a*pi)) * exp (-a*t)) * exp (-x*sinh t)) 0 0
        int1 = sget ode1 pi'
        int2 = convLim ode2 (limPInfty 0)

dbesselY'' :: Double -> FD
dbesselY'' a x = (int1 - int2) / pi'
  where ode1 = dsolve' (\t y -> (sin t) * cos (x*sin t - a*t)) 0 0
        ode2 = dsolve' (\t y -> -(sinh t) * (exp (a*t) + (cos (a*pi)) * exp (-a*t)) * exp (-x*sinh t)) 0 0
        int1 = sget ode1 pi'
        int2 = convLim ode2 (limPInfty 0)

besselY'  a = bessel' a  (besselY''  a 1, dbesselY''  a 1) :: DD

fresnel' = spure extract `scomp` dsolve' f 0 [0,1,0,0] :: DD2
  where f t [s,c,ss,cc] = [2*t*c,-2*t*s,s,c]
        extract [_,_,s,c] = (s,c)

fresnel'' = spure extract `scomp` dsolve' f 0 [0,1,0,0] :: DD2
  where f t [s,c,ss,cc] = [pi'*t*c,-pi'*t*s,s,c]
        extract [_,_,s,c] = (s,c)

riemannZeta' s = rgamma' s * convLim ode (limPInfty 1) :: Double
  where ode = dsolve' f 0 0
        f 0 y = 0
        f t y = gpow s t / (exp t - 1)

ellipticF' k = dsolve' (\t y -> (1 - (k * sin t)^2)**(-0.5)) 0 0 :: DD
ellipticE' k = dsolve' (\t y -> (1 - (k * sin t)^2)**( 0.5)) 0 0 :: DD
ellipticPi' n k = dsolve' (\t y -> (1 - (k * sin t)^2)**(-0.5) / (1 - n * (sin t)^2)) 0 0

ellipticC' = dsolve' f 0 (pi'/2,pi'/2) :: DD2
  where f 0 (k,e) = (0, 0)
        f t (k,e) = (e/(t*(1-t^2)) - k/t, (e-k)/t)
ellipticK' = spure fst `scomp` ellipticC' :: DD
ellipticE''= spure snd `scomp` ellipticC' :: DD

ellipticPin' k = dsolve' f 0 kk :: DD
  where f 0 p = (kk - ee) / k^2
        f n p = (ee + (k^2-n)*kk/n + (n^2-k^2)*p/n) / (2*(k^2-n)*(n-1))
        (kk,ee) = sget ellipticC' k

ellipticPik' n = spure (!!2) `scomp` dsolve' f 0 init :: DD
  where f 0 [k,e,p] = [0,0,0]
        f t [k,e,p] = [e/(t*(1-t^2)) - k/t, (e-k)/t, t*(e/(t^2-1)+p)/(n-t^2)]
        init = [pi'/2,pi'/2,pi'/(2*sqrt (1-n))]

ibeta' a b = dsolve' (\t y -> gpow a t * gpow b (1 - t)) 0 0 :: DD
beta' a b = sget (ibeta' a b) 1 :: Double

ein' = dsolve' f 0 0 :: DD
  where f 0 y = 1
        f t y = (1 - exp (-t)) / t

eiPos' = spure f `scomp` sseq [log1, ein1] :: DD
  where einn = ein' `scomp` spure negate
        ein1 = sseek einn 1
        log1 = sseek log' 1
        f [l,e] = eulergamma' + l - e

eiNeg' x = - e1' (-x) :: Double

e1' x = convLim ode (limPInfty (1+x)) :: Double
  where ode = dsolve' (\t y -> exp (-t) / t) x 0

en' n x = convLim ode (limPInfty 2) :: Double
  where ode = dsolve' (\t y -> exp (-x * t) / t ** n) 1 0

li1' = eiPos' `scomp` spure log :: DD

li0' x = - e1' (-log x) :: Double

si' = dsolve' f 0 0 :: DD
  where f 0 y = 1
        f t y = sin t / t

cin' = dsolve' f 0 0 :: DD
  where f 0 y = 0
        f t y = (1 - cos t) / t

ci' = spure f `scomp` sseq [log1, cin1] :: DD
  where cin1 = sseek cin' 1
        log1 = sseek log' 1
        f [l,c] = eulergamma' + l - c

chin' = dsolve' f 0 0 :: DD
  where f 0 y = 0
        f t y = (1 - cosh t) / t

chi' = spure f `scomp` sseq [log1, chin1] :: DD
  where chin1 = sseek chin' 1
        log1  = sseek log' 1
        f [l,c] = eulergamma' + l - c

dawsonp' = spure product `scomp` sseq [pre, ode] :: DD
  where ode = dsolve' (\t y -> exp (t^2)) 0 0
        pre = spure (\t -> exp (-t^2))

dawsonn' = spure product `scomp` sseq [pre, ode] :: DD
  where ode = dsolve' (\t y -> exp (-t^2)) 0 0
        pre = spure (\t -> exp (t^2))

--- complex

-- complex test: integrate y' = iy => y = cos t + i sin t
trig' = dsolve' (\t y -> (0:+1) * y) 0 1                   :: DC
cos'' = spure realPart `scomp` trig'                       :: DD
sin'' = spure imagPart `scomp` trig'                       :: DD

tau'' = imagPart $ residue' (1/) 0 1                       :: Double
pi''  = tau'' / 2                                          :: Double