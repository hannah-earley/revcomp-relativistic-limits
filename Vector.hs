{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Vector where
import Data.Complex (Complex((:+)),magnitude)
import Data.Ratio (Ratio)
import Helper

class Num (VField v) => Vector v where
    type VField v
    vconst :: VField v -> v
    vzip :: (VField v -> VField v -> VField v) -> v -> v -> v
    vzips :: ([VField v] -> VField v) -> [v] -> v
    vlist :: v -> [VField v]
    vmap :: (VField v -> VField v) -> v -> v
    vmaps :: ([VField v] -> [VField v]) -> [v] -> [v]
    vfold :: (VField v -> t -> t) -> t -> v -> t
    vthread :: (VField v -> t -> (VField v, t)) -> v -> t -> (v, t)
    vthreads :: ([VField v] -> t -> (VField v, t)) -> [v] -> t -> (v, t)
    vthreadss :: ([VField v] -> t -> ([VField v], t)) -> [v] -> t -> ([v], t)

    vzip f xs ys = head $ vmaps (\[x,y] -> [f x y]) [xs,ys]
    vzips f = head . vmaps ((:[]) . f)
    vlist = vfold (:) []
    vmap f = head . vmaps (map f) . (:[])
    vmaps f xs = fst $ vthreadss ((,) . f) xs undefined
    vfold f b v = snd $ vthread (\x b -> (x, f x b)) v b
    vthread f xs = vthreads (\[x] b -> f x b) [xs]
    vthreads f xs = first head . vthreadss (\ys -> first (:[]) . f ys) xs

    {-# MINIMAL vconst, vthreadss #-}

vseq :: Vector v => v -> x -> x
vseq v x = vfold seq x v

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
cnorm'' p = vfold ((+) . (**p) . cabs) 0

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
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance Vector Integer where
    type VField Integer = Integer
    vconst = id
    vmap   = id
    vzip   = id
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance Integral a => Vector (Ratio a) where
    type VField (Ratio a) = Ratio a
    vconst = id
    vmap   = id
    vzip   = id
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance Vector Float where
    type VField Float = Float
    vconst = id
    vmap   = id
    vzip   = id
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance Vector Double where
    type VField Double = Double
    vconst = id
    vmap   = id
    vzip   = id
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance RealFloat a => Vector (Complex a) where
    type VField (Complex a) = Complex a
    vconst = id
    vmap   = id
    vzip   = id
    vzips  = id
    vlist  = (:[])
    vfold  = flip
    vthread = id
    vthreads = id
    vthreadss = id

instance Vector u => Vector [u] where
    type VField [u] = VField u
    vconst  = repeat . vconst
    vzip    = zipWith . vzip
    vzips f = map (vzips f) . transpose2
    vlist   = concat . map vlist
    vmap    = map . vmap
    vmaps f = transpose2 . map (vmaps f) . transpose2
    vfold f = foldr (flip (vfold f))

    vthread f []     b = ([], b)
    vthread f (x:xs) b = let (x', b')  = vthread f x b
                             (xs',b'') = vthread f xs b'
                         in (x':xs', b'')

    vthreads f xss = go (transpose2 xss)
      where go []       b = ([], b)
            go (ys:yss) b = let (ys', b')  = vthreads f ys b
                                (yss',b'') = go yss b'
                            in (ys':yss', b'')

    vthreadss f xss = first transpose2 . go (transpose2 xss)
      where go []       b = ([], b)
            go (ys:yss) b = let (ys', b')  = vthreadss f ys b
                                (yss',b'') = go yss b'
                            in (ys':yss', b'')

-- --- tuples

instance ( Vector u, Vector v
         , VField u ~ VField v
         ) => Vector (u, v) where
    type VField (u, v) = VField u

    vconst c = (vconst c, vconst c)
    vzip f (u1,v1) (u2,v2) = (vzip f u1 u2, vzip f v1 v2)
    vzips f uvs = let (us, vs) = unzip uvs
                  in (vzips f us, vzips f vs)
    vlist (u,v) = vlist u ++ vlist v
    vmap f (u,v) = (vmap f u, vmap f v)
    vfold f b (u,v) = vfold f (vfold f b v) u
    vthread f (u,v) b0 =
        let (u',b1) = vthread f u b0
            (v',b2) = vthread f v b1
        in ((u',v'),b2)
    vthreads f uvs b0 =
        let (us, vs) = unzip uvs
            (u',b1) = vthreads f us b0
            (v',b2) = vthreads f vs b1
        in ((u',v'),b2)
    vthreadss f uvs b0 =
        let (us, vs) = unzip uvs
            (u',b1) = vthreadss f us b0
            (v',b2) = vthreadss f vs b1
        in (zip u' v',b2)

instance ( Vector u, Vector v, Vector w
         , VField u ~ VField v, VField u ~ VField w
         ) => Vector (u, v, w) where
    type VField (u, v, w) = VField u

    vconst c = (vconst c, vconst c, vconst c)
    vzip f (u1,v1,w1) (u2,v2,w2) = (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2)
    vzips f uvws = let (us, vs, ws) = unzip3 uvws
                   in (vzips f us, vzips f vs, vzips f ws)
    vlist (u,v,w) = vlist u ++ vlist v ++ vlist w
    vmap f (u,v,w) = (vmap f u, vmap f v, vmap f w)
    vmaps f uvws = let (us, vs, ws) = unzip3 uvws
                   in zip3 (vmaps f us) (vmaps f vs) (vmaps f ws)
    vfold f b (u,v,w) = vfold f (vfold f (vfold f b w) v) u
    vthread f (u,v,w) b0 =
        let (u',b1) = vthread f u b0
            (v',b2) = vthread f v b1
            (w',b3) = vthread f w b2
        in ((u',v',w'),b3)
    vthreads f uvws b0 =
        let (us, vs, ws) = unzip3 uvws
            (u',b1) = vthreads f us b0
            (v',b2) = vthreads f vs b1
            (w',b3) = vthreads f ws b2
        in ((u',v',w'),b3)
    vthreadss f uvws b0 =
        let (us, vs, ws) = unzip3 uvws
            (u',b1) = vthreadss f us b0
            (v',b2) = vthreadss f vs b1
            (w',b3) = vthreadss f ws b2
        in (zip3 u' v' w',b3)

instance ( Vector u, Vector v, Vector w, Vector x
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x
         ) => Vector (u, v, w, x) where
    type VField (u, v, w, x) = VField u

    vconst c = (vconst c, vconst c, vconst c, vconst c)
    vzip f (u1,v1,w1,x1) (u2,v2,w2,x2) =
        (vzip f u1 u2, vzip f v1 v2, vzip f w1 w2, vzip f x1 x2)
    vzips f uvwxs = let (us, vs, ws, xs) = unzip4 uvwxs
                    in (vzips f us, vzips f vs, vzips f ws, vzips f xs)
    vlist (u,v,w,x) = vlist u ++ vlist v ++ vlist w ++ vlist x
    vmap f (u,v,w,x) = (vmap f u, vmap f v, vmap f w, vmap f x)
    vmaps f uvwxs = let (us, vs, ws, xs) = unzip4 uvwxs
                    in zip4 (vmaps f us) (vmaps f vs)
                            (vmaps f ws) (vmaps f xs) 
    vfold f b (u,v,w,x) = vfold f (vfold f (vfold f (vfold f b x) w) v) u
    vthread f (u,v,w,x) b0 =
        let (u',b1) = vthread f u b0
            (v',b2) = vthread f v b1
            (w',b3) = vthread f w b2
            (x',b4) = vthread f x b3
        in ((u',v',w',x'),b4)
    vthreads f uvwxs b0 =
        let (us, vs, ws, xs) = unzip4 uvwxs
            (u',b1) = vthreads f us b0
            (v',b2) = vthreads f vs b1
            (w',b3) = vthreads f ws b2
            (x',b4) = vthreads f xs b3
        in ((u',v',w',x'),b4)
    vthreadss f uvwxs b0 =
        let (us, vs, ws, xs) = unzip4 uvwxs
            (u',b1) = vthreadss f us b0
            (v',b2) = vthreadss f vs b1
            (w',b3) = vthreadss f ws b2
            (x',b4) = vthreadss f xs b3
        in (zip4 u' v' w' x',b4)

instance ( Vector u, Vector v, Vector w, Vector x, Vector y
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x, VField u ~ VField y
         ) => Vector (u, v, w, x, y) where
    type VField (u, v, w, x, y) = VField u

    vconst c = ( vconst c, vconst c, vconst c
               , vconst c, vconst c )
    vzip f (u1,v1,w1,x1,y1) (u2,v2,w2,x2,y2) =
        ( vzip f u1 u2, vzip f v1 v2, vzip f w1 w2
        , vzip f x1 x2, vzip f y1 y2)
    vzips f uvwxys =
        let (us, vs, ws, xs, ys) = unzip5 uvwxys
        in ( vzips f us, vzips f vs, vzips f ws
           , vzips f xs, vzips f ys )
    vlist (u,v,w,x,y) = vlist u ++ vlist v ++ vlist w
                                ++ vlist x ++ vlist y
    vmap f (u,v,w,x,y) = ( vmap f u, vmap f v, vmap f w
                         , vmap f x, vmap f y )
    vmaps f uvwxys =
        let (us, vs, ws, xs, ys) = unzip5 uvwxys
        in zip5 (vmaps f us) (vmaps f vs) (vmaps f ws)
                (vmaps f xs) (vmaps f ys)
    vfold f b (u,v,w,x,y) =
        vfold f (vfold f (vfold f
                (vfold f (vfold f b y) x) w) v) u
    vthread f (u,v,w,x,y) b0 =
        let (u',b1) = vthread f u b0
            (v',b2) = vthread f v b1
            (w',b3) = vthread f w b2
            (x',b4) = vthread f x b3
            (y',b5) = vthread f y b4
        in ((u',v',w',x',y'),b5)
    vthreads f uvwxys b0 =
        let (us, vs, ws, xs, ys) = unzip5 uvwxys
            (u',b1) = vthreads f us b0
            (v',b2) = vthreads f vs b1
            (w',b3) = vthreads f ws b2
            (x',b4) = vthreads f xs b3
            (y',b5) = vthreads f ys b4
        in ((u',v',w',x',y'),b5)
    vthreadss f uvwxys b0 =
        let (us, vs, ws, xs, ys) = unzip5 uvwxys
            (u',b1) = vthreadss f us b0
            (v',b2) = vthreadss f vs b1
            (w',b3) = vthreadss f ws b2
            (x',b4) = vthreadss f xs b3
            (y',b5) = vthreadss f ys b4
        in (zip5 u' v' w' x' y',b5)

instance ( Vector u, Vector v, Vector w, Vector x, Vector y, Vector z
         , VField u ~ VField v, VField u ~ VField w
         , VField u ~ VField x, VField u ~ VField y
         , VField u ~ VField z
         ) => Vector (u, v, w, x, y, z) where
    type VField (u, v, w, x, y, z) = VField u

    vconst c = ( vconst c, vconst c, vconst c
               , vconst c, vconst c, vconst c )
    vzip f (u1,v1,w1,x1,y1,z1) (u2,v2,w2,x2,y2,z2) =
        ( vzip f u1 u2, vzip f v1 v2, vzip f w1 w2
        , vzip f x1 x2, vzip f y1 y2, vzip f z1 z2 )
    vzips f uvwxyzs =
        let (us, vs, ws, xs, ys, zs) = unzip6 uvwxyzs
        in ( vzips f us, vzips f vs, vzips f ws
           , vzips f xs, vzips f ys, vzips f zs )
    vlist (u,v,w,x,y,z) = vlist u ++ vlist v ++ vlist w
                       ++ vlist x ++ vlist y ++ vlist z
    vmap f (u,v,w,x,y,z) = ( vmap f u, vmap f v, vmap f w
                           , vmap f x, vmap f y, vmap f z )
    vmaps f uvwxyzs =
        let (us, vs, ws, xs, ys, zs) = unzip6 uvwxyzs
        in zip6 (vmaps f us) (vmaps f vs) (vmaps f ws)
                (vmaps f xs) (vmaps f ys) (vmaps f zs)
    vfold f b (u,v,w,x,y,z) =
        vfold f (vfold f (vfold f (vfold f
                (vfold f (vfold f b z) y) x) w) v) u
    vthread f (u,v,w,x,y,z) b0 =
        let (u',b1) = vthread f u b0
            (v',b2) = vthread f v b1
            (w',b3) = vthread f w b2
            (x',b4) = vthread f x b3
            (y',b5) = vthread f y b4
            (z',b6) = vthread f z b5
        in ((u',v',w',x',y',z'),b6)
    vthreads f uvwxyzs b0 =
        let (us, vs, ws, xs, ys, zs) = unzip6 uvwxyzs
            (u',b1) = vthreads f us b0
            (v',b2) = vthreads f vs b1
            (w',b3) = vthreads f ws b2
            (x',b4) = vthreads f xs b3
            (y',b5) = vthreads f ys b4
            (z',b6) = vthreads f zs b5
        in ((u',v',w',x',y',z'),b6)
    vthreadss f uvwxyzs b0 =
        let (us, vs, ws, xs, ys, zs) = unzip6 uvwxyzs
            (u',b1) = vthreadss f us b0
            (v',b2) = vthreadss f vs b1
            (w',b3) = vthreadss f ws b2
            (x',b4) = vthreadss f xs b3
            (y',b5) = vthreadss f ys b4
            (z',b6) = vthreadss f zs b5
        in (zip6 u' v' w' x' y' z',b6)

instance Vector v => Vector (VProxy v) where
    type VField (VProxy v) = VField v
    vconst = VProxy . vconst
    vzip f x y = VProxy (vzip f (unVProxy x) (unVProxy y))
    vzips f = VProxy . vzips f . map unVProxy
    vlist = vlist . unVProxy
    vmap f = VProxy . vmap f . unVProxy
    vmaps f = map VProxy . vmaps f . map unVProxy
    vfold f b = vfold f b . unVProxy
    vthread f v = first VProxy . vthread f (unVProxy v)
    vthreads f vs = first VProxy . vthreads f (map unVProxy vs)
    vthreadss f vs = first (map VProxy) . vthreadss f (map unVProxy vs)