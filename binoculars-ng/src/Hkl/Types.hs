{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Hkl.Types ( AbsDirPath
                 , Beamline(..)
                 , ConfigContent(..)
                 , Degree(..)
                 , Key
                 , Meter(..)
                 , SampleName
                 , ScanFile(..)
                 , ScanFilePath(..)
                 , ScanNumber(..)
                 , Section
                 , Template(..)
                 , Timestamp(..)
                 , Timescan0(..)
                 , Trajectory
                 , beamlineUpper
                 ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Char                         (toUpper)
import           Data.String                       (IsString)
import           Data.Text                         (Text)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, (*~), (/~))
import           Path                              (Abs, File, Path)
-- import           Test.QuickCheck                   (Arbitrary (..))

-- Common

type AbsDirPath = FilePath
type SampleName = String

--  Beamline

data Beamline
    = Diffabs
    | Sixs

instance Show Beamline where
  show Diffabs = "diffabs"
  show Sixs    = "sixs"

beamlineUpper ∷ Beamline → String
beamlineUpper b = [toUpper x | x ← show b]

-- ConfigContent

newtype ConfigContent
  = ConfigContent Text
  deriving (IsString, Show, FromJSON, ToJSON)

-- Degree

newtype Degree = Degree { unDegree :: Angle Double }
    deriving (Eq, Show)

-- instance Arbitrary Degree where
--   arbitrary = Degree . (*~ degree) <$> arbitrary

instance FromJSON Degree where
  parseJSON = fmap (Degree . (*~ degree)) . parseJSON

instance ToJSON Degree where
  toJSON = toJSON . (/~ degree) . unDegree

-- Meter

newtype Meter = Meter { unMeter :: Length Double }
    deriving (Eq, Show)

-- instance Arbitrary Meter where
--   arbitrary = Meter . (*~ meter) <$> arbitrary

instance FromJSON Meter where
  parseJSON = fmap (Meter . (*~ meter)) . parseJSON

instance ToJSON Meter where
  toJSON = toJSON . (/~ meter) . unMeter

-- Key

-- newtype Key = Key Text
--   deriving (IsString, Show, FromJSON, ToJSON)

type Key = Text

-- ScanFile

data ScanFile l = ScanFile l ScanNumber

-- ScanFilePath

data ScanFilePath = ScanFilePath (Path Abs File) ScanNumber
  deriving Show

-- Scannumber

newtype ScanNumber = ScanNumber { unScanNumber :: Int }
  deriving (Eq, Show, FromJSON, ToJSON)

-- Section

-- newtype Section = Section Text
--   deriving (IsString, Show, FromJSON, ToJSON)

type Section = Text

-- Template

newtype Template = Template { unTemplate :: Text }
    deriving (Eq, Show, FromJSON, ToJSON)

-- Timestamp

newtype Timestamp = Timestamp { unTimestamp :: Double }
  deriving Show

-- Timescan0

newtype Timescan0 = Timescan0 { unTimescan0 :: Double }
  deriving Show

--  Trajectory

type Trajectory = [[Double]]
