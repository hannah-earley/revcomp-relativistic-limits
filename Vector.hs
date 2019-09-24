{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Vector where
import Data.Complex (Complex((:+)),magnitude)
import Data.Ratio (Ratio)

class Num (VField v) => Vector v where
    type VField v

    vconst :: VField v -> v
    
    vzip :: (VField v -> VField v -> VField v) -> v -> v -> v

    vlist :: v -> [VField v]
    vlist = vfold (:) []

    vmap :: (VField v -> VField v) -> v -> v
    vmap f = vzip (const f) vzero

    vfold :: (VField v -> t -> t) -> t -> v -> t
    vfold f b = foldr f b . vlist

class Fractional k => ContinuousScalar k where
    ccoerce :: Double -> k
    cabs    :: k -> Double

type CVector v = (Vector v, ContinuousScalar (VField v))

cabslist :: CVector v => v -> [Double]
cabslist = map cabs . vlist

vlen :: (Vector v, Num a) => v -> a
vlen = vfold (const (+1)) 0

vzero :: Vector v => v
vzero = vconst 0

vplus :: Vector v => v -> v -> v
vplus = vzip (+)

vsub :: Vector v => v -> v -> v
vsub = vzip (-)

vhprod :: Vector v => v -> v -> v
vhprod = vzip (*)

vscale :: Vector v => VField v -> v -> v
vscale = vmap . (*)

cscale :: CVector v => Double -> v -> v
cscale = vscale . ccoerce

vperturb :: Vector v => v -> v -> VField v -> v
vperturb y0 dy h = vplus y0 (vscale h dy)

cperturb :: CVector v => v -> v -> Double -> v
cperturb y0 dy = vperturb y0 dy . ccoerce

vtot :: Vector v => v -> VField v
vtot = vfold (+) 0

vmean :: (Fractional (VField v), Vector v) => v -> VField v
vmean v = vtot v / fromIntegral (vlen v)

cnorm' :: (Integral p, CVector v) => p -> v -> Double
cnorm' p = vfold ((+) . (^p) . cabs) 0

cnorm'' :: CVector v => Double -> v -> Double
cnorm'' p = vfold ((+) . (**p) . cabs) 0 where

cnorm :: CVector v => Double -> v -> Double
cnorm p = (**(1/p)) . cnorm'' p

cnorm1 :: CVector v => v -> Double
cnorm1 = vfold ((+) . cabs) 0

cnormInf :: CVector v => v -> Double
cnormInf = maximum . cabslist

cmean :: CVector v => Double -> v -> Double
cmean p v = (cnorm'' p v / fromIntegral (vlen v)) ** (1/p)

cmean2 :: CVector v => v -> Double
cmean2 v = sqrt (cnorm' 2 v / fromIntegral (vlen v))

-- wnorm' :: (Integral p, CVector v) => p -> [Double] -> v -> Double
-- wnorm' p w = sum . zipWith go w . vlist
--   where go w x = w * (cabs x ^ p)

-- wnorm'' :: CVector v => Double -> [Double] -> v -> Double
-- wnorm'' p w = sum . zipWith go w . vlist
--   where go w x = w * (cabs x ** p)

-- wnorm :: CVector v => Double -> [Double] -> v -> Double
-- wnorm p w = (**(1/p)) . wnorm'' p w

vsum :: Vector v => [v] -> v
vsum []     = vzero
vsum [x]    = x
vsum (x:xs) = x `vplus` vsum xs

vlc :: Vector v => [(VField v,v)] -> v
vlc = vsum . map (uncurry vscale)

vlc' :: Vector v => [VField v] -> [v] -> v
vlc' [x]    (v:vs) = vscale x v
vlc' (x:xs) [v]    = vscale x v
vlc' (x:xs) (v:vs) = vplus (vscale x v) (vlc' xs vs)
vlc' _      _      = vzero

-- --- instances

instance ContinuousScalar Float where
    ccoerce = realToFrac
    cabs    = realToFrac . abs

instance ContinuousScalar Double where
    ccoerce = id
    cabs    = abs

instance (RealFloat a, ContinuousScalar a) => ContinuousScalar (Complex a) where
    ccoerce = (:+0) . ccoerce
    cabs    = cabs . magnitude

instance Vector Int where
    type VField Int = Int
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance Vector Integer where
    type VField Integer = Integer
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance Integral a => Vector (Ratio a) where
    type VField (Ratio a) = Ratio a
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance Vector Float where
    type VField Float = Float
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance Vector Double where
    type VField Double = Double
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance RealFloat a => Vector (Complex a) where
    type VField (Complex a) = Complex a
    vconst = id
    vmap   = id
    vzip   = id
    vlist  = (:[])
    vfold  = flip

instance Vector u => Vector [u] where
    type VField [u] = VField u
    vconst      = repeat . vconst
    vmap        = map . vmap
    vzip        = zipWith . vzip
    vlist       = concat . map vlist

-- --- tuples

instance ( Vector u, Vector v
         , VField u ~ VField v
         ) => Vector (u, v) where
    type VField (u, v) = VField u

    vconst c = (vconst c, vconst c)
    vmap f (u,v) = (vmap f u, vmap f v)
    vzip f (u1,v1) (u2,v2) = (vzip f u1 u2, vzip f v1 v2)
    vlist (u,v) = vlist u ++ vlist v
    vfold f b (u,v) = vfold f (vfold f b v) u

instance ( Vector u, Vector v, Vector w
         , VField u ~ VField v, VField u ~ VField w
         ) => Vector (u, v, w) where
    type VField (u, v, w) = VField u

    vconst c = (vconst c, vconst c, vconst c)
    vmap f (u,v,w) = (vmap f u, vmap f v, vmap f w)
    vzip f (u1,v1,w1) (u2,v2,w2) = (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2)
    vlist (u,v,w) = vlist u ++ vlist v ++ vlist w
    vfold f b (u,v,w) = vfold f (vfold f (vfold f b w) v) u

instance ( Vector u, Vector v, Vector w, Vector x
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x
         ) => Vector (u, v, w, x) where
    type VField (u, v, w, x) = VField u

    vconst c = (vconst c, vconst c, vconst c, vconst c)
    vmap f (u,v,w,x) = (vmap f u, vmap f v, vmap f w, vmap f x)
    vzip f (u1,v1,w1,x1) (u2,v2,w2,x2) =
        (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2, vzip f x1 x2)
    vlist (u,v,w,x) = vlist u ++ vlist v ++ vlist w ++ vlist x
    vfold f b (u,v,w,x) = vfold f (vfold f (vfold f (vfold f b x) w) v) u

instance ( Vector u, Vector v, Vector w, Vector x, Vector y
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x, VField u ~ VField y
         ) => Vector (u, v, w, x, y) where
    type VField (u, v, w, x, y) = VField u

    vconst c = ( vconst c, vconst c, vconst c
               , vconst c, vconst c )
    vmap f (u,v,w,x,y) = ( vmap f u, vmap f v, vmap f w
                         , vmap f x, vmap f y )
    vzip f (u1,v1,w1,x1,y1) (u2,v2,w2,x2,y2) =
        ( vzip f u1 u2, vzip f v1 v2, vzip f w1 w2
        , vzip f x1 x2, vzip f y1 y2)
    vlist (u,v,w,x,y) = vlist u ++ vlist v ++ vlist w
                                ++ vlist x ++ vlist y
    vfold f b (u,v,w,x,y) =
        vfold f (vfold f (vfold f
                (vfold f (vfold f b y) x) w) v) u

instance ( Vector u, Vector v, Vector w, Vector x, Vector y, Vector z
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x, VField u ~ VField y
         , VField u ~ VField z
         ) => Vector (u, v, w, x, y, z) where
    type VField (u, v, w, x, y, z) = VField u

    vconst c = ( vconst c, vconst c, vconst c
               , vconst c, vconst c, vconst c )
    vmap f (u,v,w,x,y,z) = ( vmap f u, vmap f v, vmap f w
                           , vmap f x, vmap f y, vmap f z )
    vzip f (u1,v1,w1,x1,y1,z1) (u2,v2,w2,x2,y2,z2) =
        ( vzip f u1 u2, vzip f v1 v2, vzip f w1 w2
        , vzip f x1 x2, vzip f y1 y2, vzip f z1 z2 )
    vlist (u,v,w,x,y,z) = vlist u ++ vlist v ++ vlist w
                       ++ vlist x ++ vlist y ++ vlist z
    vfold f b (u,v,w,x,y,z) =
        vfold f (vfold f (vfold f (vfold f
                (vfold f (vfold f b z) y) x) w) v) u