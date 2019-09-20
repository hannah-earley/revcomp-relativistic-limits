module ODE where
import Numeric.IEEE (IEEE, succIEEE, predIEEE)
import Data.List (mapAccumL)
import Data.Tuple (swap)

---

data Stream t a = Stream a (StreamF t a)
type StreamF t a = t -> Stream t a
type StreamFD a = StreamF Double a

spure :: (t -> a) -> StreamF t a
spure f t = Stream (f t) (spure f)

sconst :: a -> StreamF t a
sconst = spure . const

snull :: StreamF t ()
snull = sconst ()

scomp :: StreamF b c -> StreamF a b -> StreamF a c
scomp f g x = let Stream y g' = g x
                  Stream z f' = f y
              in Stream z $ f' `scomp` g'

sseq :: [StreamF t a] -> StreamF t [a]
sseq fs t = scons . fmap sseq . unzip $ map (flip spop t) fs

scons :: (a, StreamF t a) -> Stream t a
scons = uncurry Stream

suncons :: Stream t a -> (a, StreamF t a)
suncons (Stream x f) = (x, f)

spop :: StreamF t a -> t -> (a, StreamF t a)
spop = (suncons .)

spop' :: StreamF t a -> t -> (StreamF t a, a)
spop' f = swap . spop f

sget :: StreamF t a -> t -> a
sget f = fst . spop f

spops :: StreamF t a -> [t] -> ([a], StreamF t a)
spops f = swap . spops' f

spops' :: StreamF t a -> [t] -> (StreamF t a, [a])
spops' = mapAccumL spop'

stake :: StreamF t a -> [t] -> [a]
stake f = snd . spops' f

stake' :: StreamF t a -> [t] -> [(t,a)]
stake' f ts = zip ts (stake f ts)

---

class Vector v where
    vconst :: Double -> v
    vmap :: (Double -> Double) -> v -> v
    vzip :: (Double -> Double -> Double) -> v -> v -> v
    vlen :: v -> Int
    vtot :: v -> Double
    vtot = vfold (+) 0
    vfold :: (Double -> Double -> Double) -> Double -> v -> Double

instance Vector Double where
    vconst = id
    vmap = id
    vzip = id
    vlen = const 1
    vtot = id
    vfold = id

instance Vector u => Vector [u] where
    vconst = repeat . vconst
    vmap = map . vmap
    vzip = zipWith . vzip
    vlen = sum . map vlen
    vtot = sum . map vtot
    vfold f x = foldr f x . map (vfold f x)

instance (Vector u, Vector v) => Vector (u, v) where
    vconst c = (vconst c, vconst c)
    vmap f (u,v) = (vmap f u, vmap f v)
    vzip f (u1,v1) (u2,v2) = (vzip f u1 u2, vzip f v1 v2)
    vlen (u,v) = vlen u + vlen v
    vtot (u,v) = vtot u + vtot v
    vfold f x (u,v) = ff u . ff v $ x
      where ff u = f (vfold f x u)

instance (Vector u, Vector v, Vector w) => Vector (u, v, w) where
    vconst c = (vconst c, vconst c, vconst c)
    vmap f (u,v,w) = (vmap f u, vmap f v, vmap f w)
    vzip f (u1,v1,w1) (u2,v2,w2) = (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2)
    vlen (u,v,w) = vlen u + vlen v + vlen w
    vtot (u,v,w) = vtot u + vtot v + vtot w
    vfold f x (u,v,w) = ff u . ff v . ff w $ x
      where ff u = f (vfold f x u)

vzero :: Vector v => v
vzero = vconst 0

vplus :: Vector v => v -> v -> v
vplus = vzip (+)

vsub :: Vector v => v -> v -> v
vsub = vzip (-)

vhprod :: Vector v => v -> v -> v
vhprod = vzip (*)

vscale :: Vector v => Double -> v -> v
vscale = vmap . (*)

vperturb :: Vector v => v -> v -> Double -> v
vperturb y0 dy h = vplus y0 (vscale h dy)

vmean :: Vector v => v -> Double
vmean v = vtot v / fromIntegral (vlen v)

vsum :: Vector v => [v] -> v
vsum [] = vzero
vsum [x] = x
vsum (x:xs) = vplus x (vsum xs)

vlc :: Vector v => [(Double,v)] -> v
vlc = vsum . map (uncurry vscale)

vlc' :: Vector v => [Double] -> [v] -> v
vlc' [x]    (v:vs) = vscale x v
vlc' (x:xs) [v]    = vscale x v
vlc' (x:xs) (v:vs) = vplus (vscale x v) (vlc' xs vs)
vlc' _      _      = vzero

---

type Integrand x y = Double -> x -> y -> y
type Integrator' x y o = Integrand x y -> Double -> StreamFD x -> y -> o
type Integrator x y = Integrator' x y (StreamFD y)

type SimpleIntegrand y = Double -> y -> y
type SimpleIntegrator y = SimpleIntegrand y -> Double -> y -> StreamFD y

type StepFunction x y s = Integrand x y -> s -> s

simpleIntegrator :: Integrator x y -> SimpleIntegrator y
simpleIntegrator int f t0 = int (\t _ -> f t) t0 (sconst undefined)

---

euler :: Vector y => Double -> Integrator x y
euler h f t0 x0 y0 t1
  | abs h >= abs dt = Stream y1 $ euler h f t1 x' y1
  | otherwise       = euler h f (t0+h') x' (y' h') t1
  where
    dt = t1 - t0
    y1 = y' dt

    Stream x x' = x0 t0
    y' = vperturb y0 (f t0 x y0)
    h' = if t1 > t0 then abs h else -(abs h)

euler' :: Vector y => Double -> SimpleIntegrator y
euler' = simpleIntegrator . euler

---

type RK4Step x y = (Double, StreamFD x, y)

stepRK4 :: Vector y => Double -> StepFunction x y (RK4Step x y)
stepRK4 h f (t0,xf,y0) = (t2,xg,y2)
  where
    t1 = t0 + 0.5*h
    t2 = t0 + h
    (xg, [x0,x1,x2]) = spops' xf [t0,t1,t2]

    k1 = vscale h $ f t0 x0 (y0)
    k2 = vscale h $ f t1 x1 (vperturb y0 k1 0.5)
    k3 = vscale h $ f t1 x1 (vperturb y0 k2 0.5)
    k4 = vscale h $ f t2 x2 (vplus y0 k3)

    y2 = vperturb y0 (vsum [k1,k2,k2,k3,k3,k4]) (1/6)

rk4 :: Vector y => Double -> Integrator x y
rk4 h f t0 x0 y0 t1
  | abs h >= abs dt = Stream y'' $ rk4 h f t'' x'' y''
  | otherwise       = rk4 h f t' x' y' t1
  where
    dt = t1 - t0
    (t', x', y')  = stepRK4 h  f (t0,x0,y0)
    (t'',x'',y'') = stepRK4 dt f (t0,x0,y0)

rk4' :: Vector y => Double -> SimpleIntegrator y
rk4' = simpleIntegrator . rk4

---

data StepControl v = StepControl
                   { atol :: v
                   , rtol :: v
                   , clipFac :: Double -> Double
                   , clipFac' :: Double -> Double
                   , clipStep :: Double -> Double -> Double
                   }

defaultSC :: Vector y => StepControl y
defaultSC = StepControl
          { atol = vconst 1e-16
          , rtol = vconst 1e-16
          , clipFac  = clipper 0.1 5
          , clipFac' = clipper 0.1 1
          , clipStep = defaultClipStep 10
          }

epsilonAt :: IEEE a => a -> a -> a
epsilonAt t dir | dir >= 0  = succIEEE t - t
                | otherwise = predIEEE t - t

defaultClipStep :: Double -> Double -> Double -> Double
defaultClipStep f t h
  | h >= 0 && eps >= h = eps
  | h <= 0 && eps <= h = eps
  | otherwise          = h
  where
    eps = f * epsilonAt t h

clipperL :: Double -> (Double -> Double)
clipperL l v | v < l     = l
             | otherwise = v

clipperU :: Double -> (Double -> Double)
clipperU u v | v > u     = u
             | otherwise = v

clipper :: Double -> Double -> (Double -> Double)
clipper l u | l < u     = go l u
            | otherwise = go u l
  where go l u v | v < l     = l
                 | v > u     = u
                 | otherwise = v

rknorm :: Vector y => StepControl y -> y -> y -> Double
rknorm c y e = sqrt . vmean . vmap (^2) $ vzip (/) e sc
  where sc = atol c `vplus` (rtol c `vhprod` y)

--- Solving ODEs I - Nonstiff Problems :: II.4, II.5
---  -- Hairer, Nørsett, Wanner

type DOPRI5Step x y = (Double, Double, StreamFD x, y)

stepDOPRI5 :: Vector y => StepControl y -> StepFunction x y (DOPRI5Step x y)
stepDOPRI5 c f (h0,t1,xf,y1) =
    if err <= 1 then                 (h',t6,xg,y7)
                else stepDOPRI5 c' f (h',t1,xf,y1)
  where
    h = clipStep c t1 h0
    [t2,t3,t4,t5,t6] = map ((t1+) . (h*)) [0.2,0.3,0.8,8/9,1]
    (xg, [x1,x2,x3,x4,x5,x6]) = spops' xf [t1,t2,t3,t4,t5,t6]

    k1 = f' t1 x1 $ y1
    k2 = f' t2 x2 $ vperturb y1 k1 0.2
    k3 = f' t3 x3 $ y' [3/40, 9/40]
    k4 = f' t4 x4 $ y' [44/45, -56/15, 32/9]
    k5 = f' t5 x5 $ y' [19372/6561, -25360/2187, 64448/6561, -212/729]
    k6 = f' t6 x6 $ y' [9017/3168, -355/33, 46732/5247, 49/176, -5103/18656]
    k7 = f' t6 x6 $ y7

    y7 = y' [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84]
    dy7 = vlc' [-71/57600, 71/16695, -71/1920, 17253/339200, -22/525, 1/40]
               [k1,k3,k4,k5,k6,k7]

    err = rknorm c (vzip max y1 y7) dy7
    fac = (0.38 / err) ** 0.2
    h' = h * clipFac c fac
    c' = c { clipFac = clipFac' c }

    f' t x y = vscale h $ f t x y
    y' ws = vplus y1 $ vlc' ws [k1,k2,k3,k4,k5,k6]

initialStep :: Vector y => StepControl y -> Integrator' x y Double
initialStep c f t0 xf y0 = min (100 * h0) h1
  where
    Stream x0 xg = xf t0
    y0' = f t0 x0 y0

    d0 = rknorm c y0 y0
    d1 = rknorm c y0 y0'
    h0 = if d0 < 1e-5 || d1 < 1e-5 then 0.01 * (d0/d1) else 1e-6

    t1 = t0 + h0
    y1 = vperturb y0 y0' h0
    Stream x1 xh = xg t1
    y1' = f t1 x1 y1

    d2 = rknorm c y0 (vzip (-) y1' y0') / h0
    h1 = if d1 <= 1e-15 && d2 <= 1e-15
            then max 1e-6 (h0*1e-3)
            else (0.01 / max d1 d2) ** 0.2

dopri5 :: Vector y => StepControl y -> Integrator x y
dopri5 c f t0 x0 y0 = dopri5h c h0 f t0 x0 y0
  where h0 = initialStep c f t0 x0 y0

dopri5h :: Vector y => StepControl y -> Double -> Integrator x y
dopri5h c h f t x y t'
  | abs dt < abs hmin = s
  | otherwise =
        let (h'',t'',x'',y'') = stepDOPRI5 c f (h',t,x,y)
            o = compare t'' t'
            o' = compare t' t''
        in case (if t < t' then o else o') of
            LT -> dopri5h c h'' f t'' x'' y'' t'
            EQ -> Stream y'' $ dopri5h c h'' f t'' x'' y''
            GT -> s
  where
    dt = t' - t
    h' = signum dt * (min (abs h) (abs dt))
    hmin = clipStep c t dt

    -- small step
    (_,_,y') = stepRK4 dt f (t,x,y)
    s = Stream y' $ dopri5h c h' f t x y

dopri5' :: Vector y => StepControl y -> SimpleIntegrator y
dopri5' = simpleIntegrator . dopri5

--- default integrators

type DD = StreamFD Double

dsolve :: Vector y => Integrator x y
dsolve = dopri5 defaultSC

dsolve' :: Vector y => SimpleIntegrator y
dsolve' = dopri5' defaultSC