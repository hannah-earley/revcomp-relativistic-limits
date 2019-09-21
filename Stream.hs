module Stream where

import Data.List (mapAccumL)
import Data.Tuple (swap)
import Helper (bottom)

data Stream t a = Stream a (StreamF t a)
type StreamF t a = t -> Stream t a
type StreamFD a = StreamF Double a

spure :: (t -> a) -> StreamF t a
spure f t = Stream (f t) (spure f)

sconst :: a -> StreamF t a
sconst = spure . const

sbot :: StreamF t a
sbot = sconst bottom

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