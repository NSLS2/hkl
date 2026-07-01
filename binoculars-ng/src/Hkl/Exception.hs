{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}

module Hkl.Exception where

import           Control.Exception     (Exception (..))
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Data.Typeable         (Typeable)
import           Path                  (Abs, Dir, Path)

import           Hkl.Types

data HklDetectorException
  = MaskShapeNotcompatible Text
  | NoDefaultMask
  | MaskInternalError
  | NoDefaultFlat
  | FlatShapeNotcompatible Text
  | MaskTemplateError Text
  deriving (Show, Typeable)
instance Exception HklDetectorException

data HklBinocularsException
    = WrongAttenuation Text Int Double
    deriving (Show, Typeable)
instance Exception HklBinocularsException

data HklBinocularsProjectionsException
    = SaveCube'Failed
    | SaveCube'IsEmpty
    deriving (Show, Typeable)

instance Exception HklBinocularsProjectionsException

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show, Typeable)

instance Exception HklBinocularsProjectionsQCustomException


data HklBinocularsConfigException = NoDataFilesUnderTheGivenDirectory (Path Abs Dir)
                                  | ResolutionNotCompatibleWithProjectionNbOfCoordinates [Double] Int
    deriving (Show, Typeable)

instance Exception HklBinocularsConfigException


data HklDataSourceException
  = CanNotOpenDataSource'Double'Ini Section Key
  | CanNotOpenDataSource'Or HklDataSourceException HklDataSourceException
  | HklDataSourceException'HklH5Exception HklH5Exception
  | HklDataSourceException'NoRemainingDataPath [String]
  deriving (Show, Typeable)

instance Exception HklDataSourceException


data HklH5Exception
  = CanNotFindDatasetWithAttributContent ByteString ByteString
  | CanNotOpenDataset ByteString
  | CanNotOpenGroup ByteString
  | CanNotOpenGroupAt ByteString Int
  | CanNotOpenFile ByteString
  | CanNotOpenH5Or HklH5Exception HklH5Exception
  | ContainNanValue
  deriving (Show, Typeable)

instance Exception HklH5Exception where
  toException = toException . HklDataSourceException'HklH5Exception -- cast up through the Parent type
  fromException se =
    case fromException se of
      Just (HklDataSourceException'HklH5Exception c) -> Just c
      _                                              -> Nothing


data HklPyFAIException
    = CanNotParsePoni String
      deriving  (Show, Typeable)

instance Exception HklPyFAIException

data HklTemplateException
    = CanNotApplyPoniesTemplate PoniesTemplate String
      deriving  (Show, Typeable)

instance Exception HklTemplateException
