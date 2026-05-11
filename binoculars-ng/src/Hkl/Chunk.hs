{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2024, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Chunk
  ( Chunk(..)
  , allChunk
  , quadChunk
  , chunk
  , csize
  , cclip
  , clength
  ) where

import           Hkl.DataSource
import           Hkl.Orphan     ()
import           Hkl.Types

data Chunk n a
    = Chunk !a !n !n
    deriving (Eq, Show)

{-# SPECIALIZE cclip :: Int -> Int -> Chunk Int ScanFilePath -> Chunk Int ScanFilePath #-}
cclip :: (Num n, Ord n) => n -> n -> Chunk n a -> Chunk n a
cclip skipL skipH (Chunk a l h) = let nl = if l + skipL > h then l else l + skipL
                                      nh = if h - skipH < l then h else h - skipH
                                  in Chunk a nl nh

{-# SPECIALIZE allChunk :: Chunk Int ScanFilePath -> (ScanFilePath, [Int])  #-}
allChunk :: (Integral n, Enum n, Num n, Ord n) => Chunk n a -> (a, [n])
allChunk (Chunk n f t) = (n, [f..t-1])

{-# SPECIALIZE quadChunk :: Chunk Int ScanFilePath -> (ScanFilePath, [Int])  #-}
quadChunk :: (Integral n, Enum n, Num n, Ord n) => Chunk n a -> (a, [n])
quadChunk (Chunk n f t) = (n, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t-1])

{-# SPECIALIZE clength :: Chunk Int ScanFilePath -> Int  #-}
clength :: Num n => Chunk n a -> n
clength (Chunk _ l h) = h - l

csize :: (Chunk Int a, DataSourceShape) -> Int
csize (_, s) = size'DataSourceShape s

cweight :: Num n => Chunk n a -> n
cweight (Chunk _ l h) = h - l

csplit :: Num n => Chunk n a -> n -> (Chunk n a, Chunk n a)
csplit (Chunk a l h) n = (Chunk a l (l + n), Chunk a (l+n) h)

{-# SPECIALIZE chunk :: Int -> [Chunk Int ScanFilePath] -> [[Chunk Int ScanFilePath]]  #-}
chunk :: (Num n, Ord n) => n -> [Chunk n a] -> [[Chunk n a]]
chunk target = go target target
  where
    go :: (Num n, Ord n) => n -> n -> [Chunk n a] -> [[Chunk n a]]
    go _ _ []          = []
    go tgt gap [x]     = golast tgt gap x
    go tgt gap ~(x:xs) =
      let gap' = gap - cweight x
      in if | gap' > 0                 -> cons1 x $ go tgt gap' xs
            | gap' == 0                -> [x] : go tgt tgt xs
            | (x1, x2) <- csplit x gap -> [x1] : go tgt tgt (x2 : xs)

    cons1 x cs = (x : Prelude.head cs) : tail cs

    golast tgt gap x =
      if | 0 == gap                 -> [[x]]
         | cweight x <= gap         -> [[x]]
         | (x1, x2) <- csplit x gap -> [x1] : golast tgt tgt x2
