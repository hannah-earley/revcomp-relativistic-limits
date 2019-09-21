module Vector where

class Vector v where
    vconst :: Double -> v
    vmap :: (Double -> Double) -> v -> v
    vmap f = vzip (const f) vzero
    vzip :: (Double -> Double -> Double) -> v -> v -> v
    vlen :: v -> Int
    vfold :: (Double -> Double -> Double) -> Double -> v -> Double

instance Vector Double where
    vconst = id
    vmap = id
    vzip = id
    vlen = const 1
    vfold = id

instance Vector u => Vector [u] where
    vconst = repeat . vconst
    vmap = map . vmap
    vzip = zipWith . vzip
    vlen = sum . map vlen
    vfold f b = foldr f b . map (vfold f b)

instance Vector () where
    vconst _ = ()
    vmap _ _ = ()
    vzip _ _ _ = ()
    vlen _ = 0
    vfold _ b _ = b

instance (Vector u, Vector v) => Vector (u, v) where
    vconst c = (vconst c, vconst c)
    vmap f (u,v) = (vmap f u, vmap f v)
    vzip f (u1,v1) (u2,v2) = (vzip f u1 u2, vzip f v1 v2)
    vlen (u,v) = vlen u + vlen v
    vfold f b (u,v) = ff u . ff v $ b
      where ff u = f (vfold f b u)

instance (Vector u, Vector v, Vector w) => Vector (u, v, w) where
    vconst c = (vconst c, vconst c, vconst c)
    vmap f (u,v,w) = (vmap f u, vmap f v, vmap f w)
    vzip f (u1,v1,w1) (u2,v2,w2) = (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2)
    vlen (u,v,w) = vlen u + vlen v + vlen w
    vfold f b (u,v,w) = ff u . ff v . ff w $ b
      where ff u = f (vfold f b u)

instance (Vector u, Vector v, Vector w, Vector x) => Vector (u, v, w, x) where
    vconst c = (vconst c, vconst c, vconst c, vconst c)
    vmap f (u,v,w,x) = (vmap f u, vmap f v, vmap f w, vmap f x)
    vzip f (u1,v1,w1,x1) (u2,v2,w2,x2) = (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2, vzip f x1 x2)
    vlen (u,v,w,x) = vlen u + vlen v + vlen w + vlen x
    vfold f b (u,v,w,x) = ff u . ff v . ff w . ff x $ b
      where ff u = f (vfold f b u)

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

vtot :: Vector v => v -> Double
vtot = vfold (+) 0

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