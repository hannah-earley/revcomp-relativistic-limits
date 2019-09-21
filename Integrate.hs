module Integrate where

import Stream
import Vector
import Helper

---

type Integrand x y = Double -> x -> y -> y
type Integrator' x y o = Integrand x y -> Double -> StreamFD x -> y -> o
type Integrator x y = Integrator' x y (StreamFD y)

type SimpleIntegrand y = Double -> y -> y
type SimpleIntegrator y = SimpleIntegrand y -> Double -> y -> StreamFD y

type StepFunction x y s = Integrand x y -> s -> s

simpleIntegrator :: Integrator x y -> SimpleIntegrator y
simpleIntegrator int f t0 = int (\t _ -> f t) t0 sbot

inan :: Vector v => Stream t v
inan = sconst (vconst (0/0)) bottom

---

euler :: Vector y => Double -> Integrator x y
euler h f t0 x0 y0 t1
  | isNaN (dt + h + vtot y1) = inan
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
  | isNaN (dt + h + vtot y'') = inan
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
    k2 = f' t2 x2 $ vperturb y1 k1 (1/5)
    -- k1 = f' t1 x1 $ y' []
    -- k2 = f' t2 x2 $ y' [1/5]
    k3 = f' t3 x3 $ y' [3/40, 9/40]
    k4 = f' t4 x4 $ y' [44/45, -56/15, 32/9]
    k5 = f' t5 x5 $ y' [19372/6561, -25360/2187, 64448/6561, -212/729]
    k6 = f' t6 x6 $ y' [9017/3168, -355/33, 46732/5247, 49/176, -5103/18656]
    k7 = f' t6 x6 $ y7

    y7 = y' [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84]
    dy7 = dy' [-71/57600, 0, 71/16695, -71/1920, 17253/339200, -22/525, 1/40]

    err = rknorm c (vzip max y1 y7) dy7
    fac = (0.38 / err) ** 0.2
    h' = h * clipFac c fac
    c' = c { clipFac = clipFac' c }

    f' t x y = vscale h $ f t x y
    y' = vplus y1 . dy'
    dy' ws = vlc' ws [k1,k2,k3,k4,k5,k6,k7]

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
  | isNaN (dt + hmin + vtot y') = inan
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

dsolve :: Vector y => Integrator x y
dsolve = dopri5 defaultSC

dsolve' :: Vector y => SimpleIntegrator y
dsolve' = dopri5' defaultSC