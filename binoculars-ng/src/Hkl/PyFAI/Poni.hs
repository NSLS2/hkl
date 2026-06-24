{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UnicodeSyntax             #-}

{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2022, 2023, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.PyFAI.Poni
       ( DetectorConfig(..)
       , Pose(..)
         -- Poni
       , Poni
       , PoniPath
       , poniP
         --  PoniEntry
       , PoniEntry(..)
       , poniEntryFlip
       , poniEntryFromList
       , poniEntryRotation
       , poniEntryTranslation
       , poniEntryToList
       , poniEntrySet
       , poniEntryMove
       , PoniKey(..)
         -- other
       , WaveLength
       , fromAxisAndAngle
       , getPixelsCoordinates'FromPoni
       , getPoniParameter
       ) where

import           Control.Applicative               (many, optional, (<|>))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Attoparsec.Text              (Parser, decimal, double,
                                                    endOfLine, isEndOfLine,
                                                    many1, string, takeTill,
                                                    (<?>))
import           Data.List.NonEmpty                (NonEmpty (..), head,
                                                    nonEmpty)
import           Data.Text                         (Text)
import           Data.Vector.Storable              (Vector, fromList)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (castForeignPtr,
                                                    newForeignPtr)
import           Foreign.Marshal.Alloc             (finalizerFree)
import           GHC.Generics                      (Generic)
import           Numeric.LinearAlgebra             (Matrix, atIndex, fromLists,
                                                    ident, scalar, tr, (<>))
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, one, radian, (*~),
                                                    (+), (/~), (/~~))

import           Prelude                           hiding (head, (<>))

import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.MyMatrix
import           Hkl.PyFAI.Detector
import           Hkl.Repa

type WaveLength  = Length Double
type PoniPath = FilePath

--  Pose

newtype Pose = Pose (MyMatrix Double) deriving (Show)

--  Poni

-- # Nota: C-Order, 1 refers to the Y axis, 2 to the X axis
-- # Calibration done on Fri May 22 18:21:20 2026
-- poni_version: 2.1
-- Detector: PilatusCdTe2M
-- Detector_config: {"pixel1": 0.000172, "pixel2": 0.000172, "orientation": 3}
-- Distance: 0.6766662219456674
-- Poni1: 0.1460648413517324
-- Poni2: 0.13025819026342852
-- Rot1: 0.003642703476751769
-- Rot2: 0.17228456551576615
-- Rot3: 1.3824486583007656e-12
-- Wavelength: 7.293188143129427e-11
-- # Calibrant: LaB6
-- # Image: /nfs/ruche/mars-soleil/com-mars/2026/2026_Run2/20252223/published-data/scan_0034_0001.nxs.edf

type Orientation = Int

detectorConfigP :: Parser DetectorConfig
detectorConfigP = do
  _ <- "{"
  p1 <- "\"pixel1\": " *> double
  _ <- ", "
  p2 <- "\"pixel2\": " *> double
  _ <- ", "
  o <- "\"orientation\": " *> decimal
  _ <- "}"
  pure $ DetectorConfig (p1 *~ meter) (p2 *~ meter) o

data DetectorConfig
    = DetectorConfig (Length Double) (Length Double) Orientation
      deriving (Eq, Show)

data PoniEntry
    = PoniEntry { poniEntryHeader          :: [Text]
                , poniEntry'Version        :: Maybe Text
                , poniEntry'Detector       :: Maybe SomeDetector --  Detector Name
                , poniEntry'DetectorConfig :: Maybe DetectorConfig
                , poniEntry'PixelSize1     :: Maybe (Length Double) --  pixels size 1
                , poniEntry'PixelSize2     :: Maybe (Length Double) --  pixels size 1
                , poniEntry'Distance       :: Length Double --  pixels size 2
                , poniEntry'Poni1          :: Length Double --  poni1
                , poniEntry'Poni2          :: Length Double --  poni2
                , poniEntry'Rot1           :: Angle Double --  rot1
                , poniEntry'Rot2           :: Angle Double --  rot2
                , poniEntry'Rot3           :: Angle Double --  rot3
                , poniEntry'Spline         :: Maybe Text --  spline file
                , poniEntry'Wavelength     :: WaveLength --  wavelength
                }
      deriving (Eq, Show)

type Poni = NonEmpty PoniEntry

commentP :: Parser Text
commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

headerP :: Parser [Text]
headerP = many1 commentP <?> "headerP"

textP :: Text -> Parser Text
textP key = string key *> takeTill isEndOfLine <* endOfLine <?> "textP"

doubleP :: Text -> Parser Double
doubleP key = string key *> double <* endOfLine <?> "doubleP"

lengthP :: Text -> Parser (Length Double)
lengthP key = do
  value <- doubleP key
  pure $ value *~ meter

angleP :: Text -> Parser (Angle Double)
angleP key = do
    value <- doubleP key
    pure $ value *~ radian

detectorP ∷ ToPyFAI a ⇒ a → Parser a
detectorP d = do
  _ ← "Detector: " *> string (toPyFAI d) <* endOfLine
  pure d

aDetectorP ∷ Parser SomeDetector
aDetectorP = (SomeDetector <$> detectorP Xpad32) <|> (SomeDetector <$> detectorP ImXpadS140) <|> (SomeDetector <$> detectorP PilatusCdTe2M)

poniEntryP :: Parser PoniEntry
poniEntryP = PoniEntry
             <$> headerP
             <*> optional (textP "poni_version: ")
             <*> optional aDetectorP
             <*> optional ("Detector_config: " *> detectorConfigP <* endOfLine)
             <*> optional (lengthP "PixelSize1: ")
             <*> optional (lengthP "PixelSize2: ")
             <*> lengthP "Distance: "
             <*> lengthP "Poni1: "
             <*> lengthP "Poni2: "
             <*> angleP "Rot1: "
             <*> angleP "Rot2: "
             <*> angleP "Rot3: "
             <*> optional (textP "SplineFile: ")
             <*> lengthP "Wavelength: "
             <?> "poniEntryP"

poniP :: Parser Poni
poniP = do
  ps <- many poniEntryP
  case nonEmpty ps of
    Nothing  -> error "Can not parse Poni"
    Just ps' -> pure ps'

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[ 0, -z,  y],
                            [ z,  0, -x],
                            [-y,  x,  0]]
    where
      x = axis `atIndex` 0
      y = axis `atIndex` 1
      z = axis `atIndex` 2

fromAxisAndAngle :: Vector Double -> Angle Double -> Matrix Double
fromAxisAndAngle axis angle = ident 3 Prelude.+ s * q Prelude.+ c * (q <> q)
    where
      c = scalar (1 - cos (angle /~ one))
      s = scalar (sin (angle /~ one))
      q = crossprod axis

poniEntryFlip :: PoniEntry -> PoniEntry
poniEntryFlip p = p { poniEntry'Rot3 = new_rot3 }
  where
    rot3 = poniEntry'Rot3 p
    new_rot3 = rot3 Numeric.Units.Dimensional.Prelude.+ 180 *~ degree

poniEntryRotation :: PoniEntry ->  Matrix Double -- TODO MyMatrix PyFAIB
poniEntryRotation e = Prelude.foldl (<>) (ident 3) rotations
    where
      rot1 = poniEntry'Rot1 e
      rot2 = poniEntry'Rot2 e
      rot3 = poniEntry'Rot3 e
      rotations = Prelude.map (uncurry fromAxisAndAngle)
                  [ (fromList [0, 0, 1], rot3)
                  , (fromList [0, 1, 0], rot2)
                  , (fromList [1, 0, 0], rot1)]

poniEntryTranslation :: PoniEntry -> Vector Double
poniEntryTranslation e = fromList ( [ poniEntry'Poni1 e
                                    , poniEntry'Poni2 e
                                    , poniEntry'Distance e
                                    ] /~~ meter )

poniEntryMove :: MyMatrix Double -> MyMatrix Double -> PoniEntry -> PoniEntry
poniEntryMove mym1 mym2 e = e { poniEntry'Rot1 = new_rot1
                              , poniEntry'Rot2 = new_rot2
                              , poniEntry'Rot3 = new_rot3
                              }
  where
    rot1 = poniEntry'Rot1 e
    rot2 = poniEntry'Rot2 e
    rot3 = poniEntry'Rot3 e
    rotations = Prelude.map (uncurry fromAxisAndAngle)
                [ (Data.Vector.Storable.fromList [0, 0, 1], rot3)
                , (Data.Vector.Storable.fromList [0, 1, 0], rot2)
                , (Data.Vector.Storable.fromList [1, 0, 0], rot1)]
    -- M1 . R0 = R1
    r1 = Prelude.foldl (<>) (ident 3) rotations -- pyFAIB
    -- M2 . R0 = R2
    -- R2 = M2 . M1.T . R1
    r2 = Prelude.foldl (<>) m2 [tr m1, r1]
    (new_rot1, new_rot2, new_rot3) = toEulerians r2

    (MyMatrix _ m1) = changeBase mym1 PyFAIB
    (MyMatrix _ m2) = changeBase mym2 PyFAIB

poniEntrySet ∷ Length Double --  distance
             → Length Double --  poni1
             → Length Double --  poni2
             → Angle Double --  rot1
             → Angle Double --  rot2
             → Angle Double --  rot3
             → PoniEntry
             → PoniEntry
poniEntrySet d p1 p2 r1 r2 r3 p =
  p { poniEntry'Distance = d
    , poniEntry'Poni1 = p1
    , poniEntry'Poni2 = p2
    , poniEntry'Rot1 = r1
    , poniEntry'Rot2 = r2
    , poniEntry'Rot3 = r3
    }

poniEntryFromList :: PoniEntry -> [Double] -> PoniEntry
poniEntryFromList p [rot1, rot2, rot3, poni1, poni2, d] =
  p { poniEntry'Distance = d *~ meter
    , poniEntry'Poni1 = poni1 *~ meter
    , poniEntry'Poni2 = poni2 *~ meter
    , poniEntry'Rot1 = rot1 *~ radian
    , poniEntry'Rot2 = rot2 *~ radian
    , poniEntry'Rot3 = rot3 *~ radian
    }
poniEntryFromList _ _ = error "Can not convert to a PoniEntry"

poniEntryToList :: PoniEntry -> [Double]
poniEntryToList p = [ poniEntry'Rot1 p /~ radian
                    , poniEntry'Rot2 p /~ radian
                    , poniEntry'Rot3 p /~ radian
                    , poniEntry'Poni1 p /~ meter
                    , poniEntry'Poni2 p /~ meter
                    , poniEntry'Distance p /~ meter
                    ]

-- PoniKey

data PoniKey
    = Rot1 | Rot2 | Rot3 | Poni1 | Poni2 | Sdd
      deriving (Generic, Show, FromJSON, ToJSON)

-- Public Meyhods

getPixelsCoordinates'FromPoni :: Detector Hkl DIM2 -> Poni -> Normalisation -> IO PixelCoordinates
getPixelsCoordinates'FromPoni (Detector2D d _ sh) poni norm = do
  let p = head poni
  let sdd = poniEntry'Distance p
  let p1 = poniEntry'Poni1 p
  let p2 = poniEntry'Poni2 p
  let rot1 = poniEntry'Rot1 p
  let rot2 = poniEntry'Rot2 p
  let rot3 = poniEntry'Rot3 p
  let n = toEnum . fromEnum $ d
  parr <- c'hkl_binoculars_detector_2d_coordinates_get n
  let Z :. height :. width = sh
  c'hkl_binoculars_detector_2d_pyfai_calibration n parr (toEnum width) (toEnum height) (CDouble (p1 /~ meter)) (CDouble (p2 /~ meter)) (CDouble (sdd /~ meter)) (CDouble (rot1 /~ radian)) (CDouble (rot2 /~ radian)) (CDouble (rot3 /~ radian)) (toEnum . fromEnum $ norm)
  arr <- newForeignPtr finalizerFree parr
  return $ fromForeignPtr (ix3 3 height width) (castForeignPtr arr)


getPoniParameter :: Poni -> PoniKey -> Double
getPoniParameter poni k
    = let p = head poni
      in  case k of
            Rot1  -> poniEntry'Rot1 p /~ radian
            Rot2  -> poniEntry'Rot2 p /~ radian
            Rot3  -> poniEntry'Rot3 p /~ radian
            Poni1 -> poniEntry'Poni1 p /~ meter
            Poni2 -> poniEntry'Poni2 p /~ meter
            Sdd   -> poniEntry'Distance p /~ meter
