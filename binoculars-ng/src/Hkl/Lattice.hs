{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Hkl.Lattice
  ( Degree(..)
  , Lattice(..)
  , Unit(..)
  , unitToHklUnit
  , withLattice
  ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Foreign                           (ForeignPtr, Ptr,
                                                    newForeignPtr, nullPtr,
                                                    withForeignPtr)
import           Foreign.C.Types                   (CDouble (..))
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (degree, radian, (*~), (/~))

import           Hkl.C.Hkl
import           Hkl.Types

--  Lattice

data Lattice = Cubic -- a = b = c, alpha = beta = gamma = 90
               Double
             | Tetragonal -- a = b != c,  alpha = beta = gamma = 90
               Double -- a, b
               Double -- c
             | Orthorhombic -- a != b != c,  alpha = beta = gamma = 90
               Double -- a
               Double -- b
               Double -- c
             | Rhombohedral -- a = b = c, alpha = beta = gamma != 90
               Double -- a, b, c
               Degree -- alpha, beta, gamma
             | Hexagonal -- a = b != c, alpha = beta = 90, gamma = 120
               Double -- a, b
               Double -- c
             | Monoclinic -- a != b != c, alpha = gamma = 90, beta != 90
               Double -- a
               Double -- b
               Double -- c
               Degree -- beta
             | Triclinic -- a != b != c, alpha != beta != gamma != 90
               Double -- a
               Double -- b
               Double -- c
               Degree -- alpha
               Degree -- beta
               Degree -- gamma
             deriving (Eq, Generic, FromJSON, Show, ToJSON)

withLattice :: Lattice -> (Ptr C'HklLattice -> IO r) -> IO r
withLattice l func = do
  fptr <- newLattice l
  withForeignPtr fptr func

newLattice' :: CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> IO (ForeignPtr C'HklLattice)
newLattice' a b c alpha beta gamma = do
  lattice <- c'hkl_lattice_new a b c alpha beta gamma nullPtr
  newForeignPtr p'hkl_lattice_free lattice

newLattice :: Lattice -> IO (ForeignPtr C'HklLattice)
newLattice  (Cubic la) = do
  let a = CDouble la
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Tetragonal la lc) = do
  let a = CDouble la
  let c = CDouble lc
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a c alpha alpha alpha
newLattice  (Orthorhombic la lb lc) = do
  let a = CDouble la
  let b = CDouble lb
  let c = CDouble lc
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a b c alpha alpha alpha
newLattice (Rhombohedral la (Degree aalpha)) = do
  let a = CDouble la
  let alpha = CDouble (aalpha /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Hexagonal la lc) = do
  let a = CDouble la
  let c = CDouble lc
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let gamma = CDouble ((120 *~ degree) /~ radian)
  newLattice' a a c alpha alpha gamma
newLattice (Monoclinic la lb lc (Degree abeta)) = do
  let a = CDouble la
  let b = CDouble lb
  let c = CDouble lc
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let beta = CDouble (abeta /~ radian)
  newLattice' a b c alpha beta alpha
newLattice (Triclinic la lb lc (Degree aalpha) (Degree abeta) (Degree agamma)) = do
  let a = CDouble la
  let b = CDouble lb
  let c = CDouble lc
  let alpha = CDouble (aalpha /~ radian)
  let beta = CDouble (abeta /~ radian)
  let gamma = CDouble (agamma /~ radian)
  newLattice' a b c alpha beta gamma


-- Unit

data Unit = Unit'NoUnit
          | Unit'Angle'Degree
          | Unit'Angle'Radian
          | Unit'Length'Meter
          | Unit'Length'MilliMeter
  deriving (Bounded, Eq, Enum, Generic, FromJSON, Show, ToJSON)

unitToHklUnit :: Unit -> Ptr C'HklUnit
unitToHklUnit Unit'NoUnit            = p'hkl_unit_angle_rad
unitToHklUnit Unit'Angle'Degree      = p'hkl_unit_angle_deg
unitToHklUnit Unit'Angle'Radian      = p'hkl_unit_angle_rad
unitToHklUnit Unit'Length'Meter      = p'hkl_unit_length_meter
unitToHklUnit Unit'Length'MilliMeter = p'hkl_unit_length_mm
