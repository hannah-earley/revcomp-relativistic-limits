module Helper where
import Numeric.IEEE (IEEE, succIEEE, predIEEE)
import Data.List (uncons,intercalate)
import Control.DeepSeq

newtype VProxy v = VProxy { unVProxy :: v }
newtype SProxy x = SProxy { unSProxy :: x }

instance NFData v => NFData (VProxy v) where
    rnf = rnf . unVProxy

instance NFData v => NFData (SProxy v) where
    rnf = rnf . unSProxy

first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

bottom :: a
bottom = error "bottom"

---

csv' :: Show a => String -> [a] -> String
csv' s = intercalate s . map show

csv :: Show a => [a] -> String
csv = csv' ","

tsv :: Show a => [a] -> String
tsv = csv' "\t"

data Raw = Raw { unraw :: String }
instance Show Raw where
    show = unraw

---

cbrt :: Floating a => a -> a
cbrt = (** (1/3))

converge' :: (Num a, Ord a) => a -> a -> [a] -> a
converge' _ _ [x] = x
converge' rtol atol (x:x':xs)
  | abs (x' - x) < atol = x'
  | 2 * abs (x' - x) < rtol * (abs x' + abs x) = x'
  | otherwise = converge' rtol atol (x':xs)

converge :: (Fractional a, Ord a) => [a] -> a
converge = converge' 1e-16 1e-16

---

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

-- map nan to something
clipper' :: Double -> Double -> Double -> (Double -> Double)
clipper' l u = go
  where c = clipper l u
        go n v | isNaN v   = n
               | otherwise = c v

---

transpose2 :: [[a]] -> [[a]]
transpose2 = go . sequence . map uncons
  where go Nothing = []
        go (Just xs) = let (ys, yss) = unzip xs
                       in ys : transpose2 yss

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) =
     (a,b,c,d) : zip4 as bs cs ds
zip4 _      _      _      _      = []

unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 = foldr (\(a,b,c,d) ~(as,bs,cs,ds)
                  -> (a:as,b:bs,c:cs,d:ds))
               ([],[],[],[])

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) =
     (a,b,c,d,e) : zip5 as bs cs ds es
zip5 _      _      _      _      _      = []

unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 = foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es)
                  -> (a:as,b:bs,c:cs,d:ds,e:es))
               ([],[],[],[],[])

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
     (a,b,c,d,e,f) : zip6 as bs cs ds es fs
zip6 _      _      _      _      _      _      = []

unzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6 = foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs)
                  -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
               ([],[],[],[],[],[])