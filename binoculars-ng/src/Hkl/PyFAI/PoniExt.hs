{-# LANGUAGE UnicodeSyntax #-}

{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2022, 2023, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.PyFAI.PoniExt
       ( PoniExt(..)
       , flip
       , move
       , set
       ) where

import           Data.List.NonEmpty                (map)
import           Numeric.LinearAlgebra             (ident)
import           Numeric.Units.Dimensional.Prelude (Angle, Length)

import           Hkl.MyMatrix
import           Hkl.PyFAI.Poni

import           Prelude                           hiding (flip, map)

data PoniExt = PoniExt Poni Pose deriving (Show)

flip :: PoniExt -> PoniExt
flip (PoniExt ps mym1) = PoniExt p mym1
  where
    p = map poniEntryFlip ps

set ∷ PoniExt
    → Length Double --  distance
    → Length Double --  poni1
    → Length Double --  poni2
    → Angle Double --  rot1
    → Angle Double --  rot2
    → Angle Double --  rot3
    → PoniExt
set (PoniExt ps _) d p1 p2 r1 r2 r3 = PoniExt p pose
  where
    p = map (poniEntrySet d p1 p2 r1 r2 r3) ps
    pose = Pose (MyMatrix HklB (ident 3))

move :: PoniExt -> Pose -> PoniExt
move (PoniExt p1 (Pose mym1)) (Pose mym2) = PoniExt p (Pose mym2)
  where
    p = map (poniEntryMove mym1 mym2) p1
