module Optimise where

type Comp b = b -> b -> Bool
type Optimiser a b = (a -> b) -> a -> a -> (a, b)

-- golden section search for optimum of convex function
--   n      = maximum number iterations
--   r,atol = relative & absolute tolerance
--   (!)    = comparison function
-- finds x such that, forall y/=x, x!y (where a<=x,y<=b)

gss' :: (Floating a, Ord a) => Int -> a -> a -> Comp b -> Optimiser a b
gss' n rtol atol (!) f a b = go n a' (c, f c) (d, f d) b'
  where
    a' = min a b
    b' = max a b
    h = b' - a'
    tol' = rtol * h + atol

    c = a' + invphi2 * h
    d = a' + invphi  * h

    invphi  = 0.5 * (sqrt 5 - 1)
    invphi2 = 0.5 * (3 - sqrt 5)

    go n a (c, yc) (d, yd) b
      | h <= tol' || n <= 0 = if yc ! yd then (c, yc) else (d, yd)
      | yc ! yd             = go (n-1) a (c', f c') (c, yc) d
      | otherwise           = go (n-1) c (d, yd) (d', f d') b
      where
        h = b - a
        h' = invphi * h
        c' = a + invphi2 * h'
        d' = c + invphi  * h'

gss :: (Floating a, Ord a) => Comp b -> Optimiser a b
gss = gss' 100 1e-16 1e-16

maximise :: (Floating a, Ord a, Ord t) => Optimiser a t
maximise = gss (>)

minimise :: (Floating a, Ord a, Ord t) => Optimiser a t
minimise = gss (<)