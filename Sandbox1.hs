{-# LANGUAGE NoMonomorphismRestriction #-}

module Sandbox1 where
import Numeric.IEEE (IEEE, succIEEE, predIEEE)


euler y' (t:t':ts) y0 = let dt = t' - t
                            y1 = y0 + dt * (y' t y0)
                        in (t,y0) : euler y' (t':ts) y1
euler _ [t] y = [(t,y)]

-- t_squared h t = euler (\t y->2*t) [0,h..] 0 !! n
--   where n = round $ t / h

-- t_squared' = t_squared 0.0001

integrate f y0 h = let ys = euler f [0,h..] y0
                   in \t -> snd $ ys !! round (t/h)

square' = integrate (\t y -> 2*t) 0 0.0001
exp' = integrate (\t y -> y) 1 0.0001
log' = integrate (\t y -> 1/(t+1)) 0 0.0001 . subtract 1

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

stream2tuple :: Stream t a -> (a, StreamF t a)
stream2tuple (Stream x f) = (x, f)

joinStreams :: [StreamF t a] -> StreamF t [a]
joinStreams fs t = let (xs, fs') = unzip $ map (stream2tuple . ($t)) fs
                   in Stream xs (joinStreams fs')

class Vector v where
    rep :: Double -> v
    pw1 :: (Double -> Double) -> v -> v
    pw2 :: (Double -> Double -> Double) -> v -> v -> v
    dim :: v -> Int
    tot :: v -> Double

zero :: Vector v => v
zero = rep 0

scale :: Vector v => Double -> v -> v
scale = pw1 . (*)

add :: Vector v => v -> v -> v
add = pw2 (+)

addScale :: Vector v => v -> v -> Double -> v
addScale y0 dy h = add y0 (scale h dy)

average :: Vector v => v -> Double
average v = tot v / fromIntegral (dim v)

vsum :: Vector v => [v] -> v
vsum [] = zero
vsum [x] = x
vsum (x:xs) = add x (vsum xs)

vlc :: Vector v => [(Double,v)] -> v
vlc = vsum . map (uncurry scale)

vlc' :: Vector v => [Double] -> [v] -> v
vlc' [x]    (v:vs) = scale x v
vlc' (x:xs) [v]    = scale x v
vlc' (x:xs) (v:vs) = add (scale x v) (vlc' xs vs)
vlc' _      _      = zero

instance Vector Double where
    rep = id
    pw1 = id
    pw2 = id
    dim = const 1
    tot = id

instance Vector u => Vector [u] where
    rep = repeat . rep
    pw1 = map . pw1
    pw2 = zipWith . pw2
    dim = sum . map dim
    tot = sum . map tot

eul :: Vector y => Double -> (Double -> x -> y -> y) -> Double -> StreamFD x -> y -> StreamFD y
eul h f t0 x0 y0 t1
  | abs h >= abs (t1-t0) = let y1 = y' (t1-t0)
                           in Stream y1 (eul h f t1 x' y1)
  | otherwise            = eul h f (t0+h') x' (y' h') t1
  where
    Stream x x' = x0 t0
    y' = addScale y0 (f t0 x y0)
    h' = if t1 > t0 then abs h else -(abs h)

eul' :: Vector y => Double -> (Double -> y -> y) -> Double -> y -> StreamFD y
eul' h f t0 = eul h (\t _ -> f t) t0 snull

streamGo :: StreamF t a -> t -> a
streamGo s t = let Stream x _ = s t in x

streamTake :: StreamF t a -> [t] -> ([a], StreamF t a)
streamTake f (t:ts) = let Stream x g = f t
                          (xs, h) = streamTake g ts
                      in (x:xs, h)
streamTake f [] = ([], f)

streamMany :: StreamF t a -> [t] -> [a]
streamMany f (t:ts) = let Stream x g = f t in x : streamMany g ts
streamMany _ [] = []

exp'' = eul' 0.0001 (\t y -> y) 0 1
exp''' = eul 0.0001 (\t x y -> x) 0 exp'' 0

stepRK4 :: Vector y => Double -> (Double -> x -> y -> y) -> (Double, StreamFD x, y) -> (Double, StreamFD x, y)
stepRK4 h f (t0,xf,y0) = (t2,xg,y2)
  where
    t1 = t0 + 0.5*h
    t2 = t0 + h
    ([x0,x1,x2], xg) = streamTake xf [t0,t1,t2]

    k1 = scale h $ f t0 x0 (y0)
    k2 = scale h $ f t1 x1 (addScale y0 k1 0.5)
    k3 = scale h $ f t1 x1 (addScale y0 k2 0.5)
    k4 = scale h $ f t2 x2 (add y0 k3)

    y2 = addScale y0 (vsum [k1,k2,k2,k3,k3,k4]) (1/6)

stepRK4' :: Vector y => Double -> (Double -> y -> y) -> (Double, y) -> (Double, y)
stepRK4' h f (t0,y0) = (t2,y2)
  where
    t1 = t0 + 0.5*h
    t2 = t0 + h

    k1 = scale h $ f t0 (y0)
    k2 = scale h $ f t1 (addScale y0 k1 0.5)
    k3 = scale h $ f t1 (addScale y0 k2 0.5)
    k4 = scale h $ f t2 (add y0 k3)

    y2 = addScale y0 (vsum [k1,k2,k2,k3,k3,k4]) (1/6)


rk4 :: Vector y => Double -> (Double -> x -> y -> y) -> Double -> StreamFD x -> y -> StreamFD y
rk4 h f t0 x0 y0 t1
  | abs h >= abs (t1-t0) = let (t',x',y') = stepRK4 (t1-t0) f (t0,x0,y0)
                           in Stream y' (rk4 h f t' x' y')
  | otherwise            = let (t',x',y') = stepRK4 h f (t0,x0,y0)
                           in rk4 h f t' x' y' t1

rk4' :: Vector y => Double -> (Double -> y -> y) -> Double -> y -> StreamFD y
rk4' h f t0 y0 t1
  | abs h >= abs (t1-t0) = let (t',y') = stepRK4' (t1-t0) f (t0,y0)
                           in Stream y' (rk4' h f t' y')
  | otherwise            = let (t',y') = stepRK4' h f (t0,y0)
                           in rk4' h f t' y' t1

stepRK45 h f (t1,xf,y1) = undefined
  where
    [t2,t3,t4,t5,t6] = map ((t1+).(h*)) [0.25,0.375,12/13,1,0.5]
    ([x1,x2,x3,x6,x4,x5], xg) = streamTake xf [t1,t2,t3,t6,t4,t5]

    k1 = scale h $ f t1 x1 y1
    k2 = scale h $ f t2 x2 (addScale y1 k1 0.25)
    k3 = f' t3 x3 [0.09375,0.28125]
    k4 = f' t4 x4 [1932/2197,-7200/2197,7296/2197]
    k5 = f' t5 x5 [439/216,-8,3680/513,-845/4104]
    k6 = f' t6 x6 [-8/27,2,-3544/2565,1859/4104,-0.275]

    f' t x ws = scale h (f t x (y' ws))
    y' ws = add y1 (vlc' ws [k1,k2,k3,k4,k5,k6])
    y'' ws = add y1 (vlc' ws [k1,k3,k4,k5,k6])

    y2 = y'' [25/216,1408/2565,2197/4101,-0.2]
    z2 = y'' [16/135,6656/12825,28561/56430,-0.18,2/55]
    -- s = ((0.5 * tol * h) / (abs $ z2 - y2)) ** 0.25

data StepControl v = StepControl
                   { atol :: v
                   , rtol :: v
                   , clipFac :: Double -> Double
                   , clipFac' :: Double -> Double
                   , clipStep :: Double -> Double -> Double
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

defaultDC :: Vector y => StepControl y
defaultDC = StepControl
          { atol = rep 1e-16
          , rtol = rep 1e-12
          , clipFac = clipper 0.1 5
          , clipFac' = clipper 0.1 1
          , clipStep = defaultClipStep 10
          }

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

snorm :: Vector y => StepControl y -> y -> y -> Double
snorm c y e = sqrt . average . pw1 (^2) $ pw2 (/) e sc
  where sc = pw2 (+) (atol c) $ pw2 (*) y (rtol c)

stepDOPRI5 :: Vector y => StepControl y -> (Double -> x -> y -> y) -> (Double, Double, StreamFD x, y) -> (Double, Double, StreamFD x, y)
stepDOPRI5 c f (h0,t1,xf,y1) =
    if err <= 1 then                 (h',t6,xg,y7)
                else stepDOPRI5 c' f (h',t1,xf,y1)
  where
    h = clipStep c t1 h0
    [t2,t3,t4,t5,t6] = map ((t1+).(h*)) [0.2,0.3,0.8,8/9,1]
    ([x1,x2,x3,x4,x5,x6], xg) = streamTake xf [t1,t2,t3,t4,t5,t6]

    k1 = scale h $ f t1 x1 y1
    k2 = scale h $ f t2 x2 (addScale y1 k1 0.2)
    k3 = f' t3 x3 [3/40, 9/40]
    k4 = f' t4 x4 [44/45, -56/15, 32/9]
    k5 = f' t5 x5 [19372/6561, -25360/2187, 64448/6561, -212/729]
    k6 = f' t6 x6 [9017/3168, -355/33, 46732/5247, 49/176, -5103/18656]

    y7 = y' [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84]
    -- z7 = y'' [5179/57600, 7571/16695, 393/640, −92097/339200, 187/2100, 1/40]
    k7 = scale h $ f t6 x6 y7
    e7 = vlc' [-71/57600, 71/16695, -71/1920, 17253/339200, -22/525, 1/40] [k1,k3,k4,k5,k6,k7]

    err = snorm c (pw2 max y1 y7) e7
    -- sc = pw2 (+) (atol c) $ pw2 (*) (pw2 max y1 y7) (rtol c)
    -- err = sqrt . average . pw1 (^2) $ pw2 (/) e7 sc
    fac = (0.38 / err) ** 0.2
    h' = h * clipFac c fac
    c' = c { clipFac = clipFac' c }
    -- err = y7 - z7

    f' t x ws = scale h (f t x (y' ws))
    y' ws = add y1 (vlc' ws [k1,k2,k3,k4,k5,k6])
    -- y'' ws = add y1 (vlc' ws [k1,k3,k4,k5,k6,k7])

initialStep :: Vector y => StepControl y -> (Double -> x -> y -> y) -> Double -> StreamFD x -> y -> Double
initialStep c f t0 xf y0 = min (100 * h0) h1
  where
    Stream x0 xg = xf t0
    y0' = f t0 x0 y0

    d0 = snorm c y0 y0
    d1 = snorm c y0 y0'
    h0 = if d0 < 1e-5 || d1 < 1e-5 then 0.01 * (d0/d1) else 1e-6

    t1 = t0 + h0
    y1 = addScale y0 y0' h0
    Stream x1 xh = xg t1
    y1' = f t1 x1 y1

    d2 = snorm c y0 (pw2 (-) y1' y0') / h0
    h1 = if d1 <= 1e-15 && d2 <= 1e-15
            then max 1e-6 (h0*1e-3)
            else (0.01 / max d1 d2) ** 0.2


dopri5 :: Vector y => StepControl y -> (Double -> x -> y -> y) -> Double -> StreamFD x -> y -> StreamFD y
dopri5 c f t0 x0 y0 = go h0 t0 x0 y0
  where
    h0 = initialStep c f t0 x0 y0
    -- go :: Vector y => Double -> Double -> StreamFD x -> y -> StreamFD y
    go h t x y t'
      | abs dt < abs hmin = s
      | otherwise =
            let (h'',t'',x'',y'') = stepDOPRI5 c f (h',t,x,y)
                o = compare t'' t'
                o' = compare t' t''
            in case (if t < t' then o else o') of
                LT -> go h'' t'' x'' y'' t'
                EQ -> Stream y'' $ go h'' t'' x'' y''
                GT -> s
      where
        dt = t' - t
        h' = signum dt * (min (abs h) (abs dt))
        hmin = clipStep c t dt

        -- small step
        (_,_,y') = stepRK4 dt f (t,x,y)
        s = Stream y' $ go h' t x y

dopri5' :: Vector y => StepControl y -> (Double -> y -> y) -> Double -> y -> StreamFD y
dopri5' c f t0 = dopri5 c (\t _ -> f t) t0 snull

exp2 :: StreamFD Double
exp2 = dopri5' defaultDC (\t y -> y) 0 1

log2' :: StreamFD Double
log2' = dopri5' defaultDC (\t y -> 1/(t+1)) 0 0

log3 = rk4' (-0.00001) (\t y -> 1/(t+1)) 0 0

log2 :: StreamFD Double
log2 = log2' `scomp` (spure $ subtract 1)

exp2' :: StreamFD Double
-- exp2' = dopri5 defaultDC (\t x y -> x) 0 exp2 0
exp2' = dopri5 defaultDC (\t x y -> streamGo exp2 y) 0 snull 0



{-

y1 = rk45' f t0 t1 y0
    -- computes y1 using adaptive step size,
       stopping exactly at t1

ys = rk45 f ts y0
    -- computes y(t)s using adaptive step size,
       computes each y(t) exactly at t

can nest rk45s using the ts parameter, as
we feed the required timesteps into ts and
use list fusion

should allow y to be a vector

---

architecture,
 streaming integrator function:
    f t0 = (y0, g)
    g t1 = (y1, h)
    ...
 ti need not be monotonic, as we could integrate back and forth

we can feed in streams as 'parameters' to a stream generator,
eg an integration function

 rk45 f xs t0 y0 => streaming function
    where xs are input stream parameters
    i.e. we are a functional of the xs

-}

--