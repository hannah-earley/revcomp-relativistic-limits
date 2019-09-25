{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module StableSum where
import Control.DeepSeq

import Stream
import Vector
import Helper

type StreamMC a = StreamFI (Integer, a, Double)
type StreamFI a = StreamF Integer a
type StableSummer a = [a] -> StreamFI a

class NFData a => StableSummable a where
    sszero :: a
    ssplus :: a -> a -> a
    sssub  :: a -> a -> a
    ssadd  :: a -> a -> (a,a)

class StableSummable a => StableStats a where
    ssscale :: Integer -> a -> a
    sssquare :: a -> a

smean :: StableStats a => StableSummer a -> [a] -> StreamFI (Integer,a)
smean ss xs = go (0 :: Integer, ss xs)
  where go (n,s) dn = let Stream t s' = s dn
                          n' = n + dn
                          m = ssscale n' t
                      in Stream (n', m) (go (n',s'))

sstats :: StableStats a => StableSummer (a,a) -> [a] -> StreamFI (Integer,a,a)
sstats ss = smapl f . smean ss . map g
  where
    g !x = x `deepseq` (x, sssquare x)
    f (n,(m,m2)) = let var = m2 `sssub` sssquare m
                       sm2 = ssscale (n-1) var
                   in (n, m, sm2)

ssum :: StableSummable a => StableSummer a
ssum = neumaier

vstats :: (NFData x, Fractional (VField x), CVector x) => [x] -> StreamMC x
vstats = smapl f . sstats ssum . map VProxy
  where f (n,VProxy m,s2) = (n,m,sqrt $ cnorm1 s2)

neumaier :: StableSummable a => StableSummer a
neumaier = go sszero sszero
  where
    go !s !c [] n = sconst (force $ s `ssplus` c) n
    go !s !c xs 0 = Stream (s `ssplus` c) (go s c xs)
    go !s !c ~(!x:xs) !n =
        let (!s',!dc) = ssadd s x
            !c' = force $ c `ssplus` dc
        in go s' c' xs (force $ n-1)

klein2 :: StableSummable a => StableSummer a
klein2 = go sszero sszero sszero
  where
    go !s !cs !ccs [] n = sconst (force $ s `ssplus` cs `ssplus` ccs) n
    go !s !cs !ccs xs 0 = Stream (s `ssplus` cs `ssplus` ccs) (go s cs ccs xs)
    go !s !cs !ccs ~(!x:xs) !n =
        let (!s', !c)  = force $ ssadd s  x
            (!cs',!cc) = force $ ssadd cs c
            !css'      = force $ ccs `ssplus` cc
        in go s' cs' css' xs (force $ n-1)

klein :: StableSummable a => Int -> StableSummer a
klein d = go (replicate (d+1) sszero)
  where
    go !ss [] n = sconst (foldr1 ssplus ss) n
    go !ss xs 0 = Stream (foldr1 ssplus ss) (go ss xs)
    go !ss ~(!x:xs) !n = let ss' = go' ss x
                         in go ss' xs (force $ n-1)

    go' (s:ss) x = let (!s', !c) = force $ ssadd s x
                   in s' : go' ss c
    go' []     _ = []

scadd :: (Num x, ContinuousScalar x) => [x] -> [x]
scadd [!s,!x] = [s',dc]
  where !s' = (s + x)
        !dc = if cabs s > cabs x
              then (s - s') + x
              else (x - s') + s

instance (NFData x, Num x, ContinuousScalar x)
         => StableSummable (SProxy x) where
    sszero = SProxy 0
    ssplus (SProxy !x) (SProxy !y) = SProxy (force $ x + y)
    sssub  (SProxy !x) (SProxy !y) = SProxy (force $ x - y)
    ssadd  (SProxy !s) (SProxy !x) = let [s',dc] = scadd [s,x]
                                     in (SProxy s', SProxy dc)
instance (NFData x, Fractional x, ContinuousScalar x)
         => StableStats (SProxy x) where
    ssscale n (SProxy !x) = SProxy (x / fromIntegral n)
    sssquare  (SProxy !x) = SProxy (x^2)

instance (NFData x, CVector x)
         => StableSummable (VProxy x) where
    sszero = vconst 0
    ssplus = vplus
    sssub  = vsub
    ssadd !s !x = let [s',dc] = vmaps scadd [s,x]
                  in force (s',dc)
instance (NFData x, CVector x)
         => StableStats (VProxy x) where
    ssscale n = vmap (/ fromIntegral n)
    sssquare  = vmap ((^2) . abs)

instance StableSummable a => StableSummable [a] where
    sszero = repeat sszero
    ssplus = zipWith ssplus
    sssub  = zipWith sssub
    ssadd ss xs = unzip $ zipWith ssadd ss xs
instance StableStats a => StableStats [a] where
    ssscale  = map . ssscale
    sssquare = map sssquare

instance StableSummable () where
    sszero = ()
    ssplus _ _ = ()
    sssub  _ _ = ()
    ssadd  _ _ = ((),())
instance StableStats () where
    ssscale _ _ = ()
    sssquare  _ = ()

instance (StableSummable a, StableSummable b) => StableSummable (a, b) where
    sszero = (sszero, sszero)
    ssplus (a1,a2) (b1,b2) = (ssplus a1 b1, ssplus a2 b2)
    sssub  (a1,a2) (b1,b2) = (sssub  a1 b1, sssub  a2 b2)
    ssadd (!s1,!s2) (!x1,!x2) = let (s1',dc1) = ssadd s1 x1
                                    (s2',dc2) = ssadd s2 x2
                                in ((s1',s2'), (dc1,dc2))
instance (StableStats a, StableStats b) => StableStats (a, b) where
    ssscale n (a,b) = (ssscale n a, ssscale n b)
    sssquare  (a,b) = (sssquare  a, sssquare  b)