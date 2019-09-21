{-# LANGUAGE NoMonomorphismRestriction #-}

module Vector where

class Vector v where
    vconst :: Double -> v
    vmap   :: (Double -> Double) -> v -> v
    vmap f = vzip (const f) vzero
    vzip   :: (Double -> Double -> Double) -> v -> v -> v
    vlen   :: v -> Int
    vfold  :: (Double -> Double -> Double) -> Double -> v -> Double

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
vsum []     = vzero
vsum [x]    = x
vsum (x:xs) = vplus x (vsum xs)

vlc :: Vector v => [(Double,v)] -> v
vlc = vsum . map (uncurry vscale)

vlc' :: Vector v => [Double] -> [v] -> v
vlc' [x]    (v:vs) = vscale x v
vlc' (x:xs) [v]    = vscale x v
vlc' (x:xs) (v:vs) = vplus (vscale x v) (vlc' xs vs)
vlc' _      _      = vzero

--- instances

instance Vector Double where
    vconst = id
    vmap   = id
    vzip   = id
    vlen   = const 1
    vfold  = id

instance Vector u => Vector [u] where
    vconst    = repeat . vconst
    vmap      = map . vmap
    vzip      = zipWith . vzip
    vlen      = sum . map vlen
    vfold f b = foldr f b . map (vfold f b)

instance Vector () where
    vconst _     = ()
    vmap   _ _   = ()
    vzip   _ _ _ = ()
    vlen   _     = 0
    vfold  _ b _ = b

--- tuples

instance (Vector u, Vector v)
         => Vector (u, v) where

    vconst c =
        let v = vconst c
        in (v, v)

    vmap f (u,v) =
        let g = vmap f
        in (g u, g v)

    vzip f (u1,v1) (u2,v2) =
        let (!) = vzip f
        in (u1!u2, v1!v2)

    vlen (u,v) =
        let g = vlen
        in g u + g v

    vfold f b (u,v) =
        let ff u = f (vfold f b u)
        in ff u . ff v $ b

instance (Vector u, Vector v, Vector w)
         => Vector (u, v, w) where

    vconst c =
        let v = vconst c
        in (v, v, v)

    vmap f (u,v,w) =
        let g = vmap f
        in (g u, g v, g w)

    vzip f (u1,v1,w1) (u2,v2,w2) =
        let (!) = vzip f
        in (u1!u2, v1!v2, w1!w2)

    vlen (u,v,w) =
        let g = vlen
        in g u + g v + g w

    vfold f b (u,v,w) =
        let ff u = f (vfold f b u)
        in ff u . ff v . ff w $ b

instance (Vector u, Vector v, Vector w, Vector x)
         => Vector (u, v, w, x) where

    vconst c =
        let v = vconst c
        in (v, v, v, v)

    vmap f (u,v,w,x) =
        let g = vmap f
        in (g u, g v, g w, g x)

    vzip f (u1,v1,w1,x1) (u2,v2,w2,x2) =
        let (!) = vzip f
        in (u1!u2, v1!v2, w1!w2, x1!x2)

    vlen (u,v,w,x) =
        let g = vlen
        in g u + g v + g w + g x

    vfold f b (u,v,w,x) =
        let ff u = f (vfold f b u)
        in ff u . ff v . ff w . ff x $ b

instance (Vector u, Vector v, Vector w, Vector x, Vector y)
         => Vector (u, v, w, x, y) where

    vconst c =
        let v = vconst c
        in (v, v, v, v, v)

    vmap f (u,v,w,x,y) =
        let g = vmap f
        in (g u, g v, g w, g x, g y)

    vzip f (u1,v1,w1,x1,y1) (u2,v2,w2,x2,y2) =
        let (!) = vzip f
        in (u1!u2, v1!v2, w1!w2, x1!x2, y1!y2)

    vlen (u,v,w,x,y) =
        let g = vlen
        in g u + g v + g w + g x + g y

    vfold f b (u,v,w,x,y) =
        let ff u = f (vfold f b u)
        in ff u . ff v . ff w . ff x . ff y $ b

instance (Vector u, Vector v, Vector w, Vector x, Vector y, Vector z)
         => Vector (u, v, w, x, y, z) where

    vconst c =
        let v = vconst c
        in (v, v, v, v, v, v)

    vmap f (u,v,w,x,y,z) =
        let g = vmap f
        in (g u, g v, g w, g x, g y, g z)

    vzip f (u1,v1,w1,x1,y1,z1) (u2,v2,w2,x2,y2,z2) =
        let (!) = vzip f
        in (u1!u2, v1!v2, w1!w2, x1!x2, y1!y2, z1!z2)

    vlen (u,v,w,x,y,z) =
        let g = vlen
        in g u + g v + g w + g x + g y + g z

    vfold f b (u,v,w,x,y,z) =
        let ff u = f (vfold f b u)
        in ff u . ff v . ff w . ff x . ff y . ff z $ b