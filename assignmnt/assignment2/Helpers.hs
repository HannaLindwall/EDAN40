module Helpers where

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1))
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)


mcsLength' :: Eq a => [a] -> [a] -> Int
mcsLength' _ [] = 0
mcsLength' [] _ = 0
mcsLength' (x:xs) (y:ys)
    | x == y    = 1 + mcsLength' xs ys
    | otherwise = max (mcsLength' xs (y:ys))
                      (mcsLength' (x:xs) ys)
