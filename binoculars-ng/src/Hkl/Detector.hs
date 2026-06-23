{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Hkl.Detector
       ( Detector(..)
       , Flat
       , Hkl
       , Mask
       , Normalisation(..)
       , PixelCoordinates
       , PyFAI
       , SomeDetector(..)
       , ZeroD
       , defaultDetector
       , detectors
       , getDetectorFlat
       , getDetectorDefaultFlat
       , getDetectorMask
       , getDetectorDefaultMask
       , getPixelsCoordinates
       , inDetector
       , maskOr
       , mkDetector
       , newDetector
       , parseDetector2D
       , shape
       , withDetector
       ) where


import           Control.Monad                     ((<=<))
import           Control.Monad.Catch               (MonadThrow, throwM)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object, pairs, withObject,
                                                    (.:), (.=))
import           Data.List                         (find, sort)
import           Data.Text                         (Text, pack, unpack, unwords)
import           Foreign.C.String                  (peekCString, withCString)
import           Foreign.C.Types                   (CBool, CDouble (..))
import           Foreign.ForeignPtr                (ForeignPtr, castForeignPtr,
                                                    newForeignPtr,
                                                    withForeignPtr)
import           Foreign.Marshal.Alloc             (alloca, finalizerFree)
import           Foreign.Ptr                       (Ptr, nullPtr)
import           Foreign.Storable                  (peek)
import           GHC.Generics                      (Generic)
import           GHC.IO.Unsafe                     (unsafePerformIO)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, meter,
                                                    radian, (/~))
import           Test.QuickCheck                   (Arbitrary (..), elements)

import           Hkl.C.Binoculars
import           Hkl.C.Hkl
import           Hkl.Exception
import           Hkl.Repa

data PyFAI deriving (Eq, Show)
data Hkl deriving (Eq, Show)
data ZeroD deriving (Eq, Show)

data Detector a sh where
  ImXpadS140 :: Detector PyFAI DIM2
  PilatusCdTe2M :: Detector PyFAI DIM2
  Xpad32 :: Detector PyFAI DIM2
  XpadFlatCorrected :: Detector PyFAI DIM2
  ZeroD :: Detector ZeroD DIM0
  Detector2D :: HklBinocularsDetectorEnum -> String -> DIM2 -> Detector Hkl DIM2

deriving instance Show (Detector a sh)
deriving instance Eq (Detector a sh)

instance FromJSON (Detector Hkl DIM2) where
    parseJSON = withObject "Detector2D" $ \v ->
                do n <- v .: "detector"
                   case parseDetector2D n of
                     Left e  -> fail e
                     Right d -> pure d

instance ToJSON (Detector Hkl DIM2) where
    -- this generates a Value
    toJSON (Detector2D _ name _) =
        object ["detector" .= name]

    -- this encodes directly to a bytestring Builder
    toEncoding (Detector2D _ name _) =
        pairs ("detector" .= name)

instance Arbitrary (Detector Hkl DIM2) where
  arbitrary = elements detectors

type Mask = Array F DIM2 CBool

type Flat = Array F DIM2 CDouble

type PixelCoordinates = Array F DIM3 Double

-- Detector

withDetector :: Detector a sh -> (Ptr C'HklDetector -> IO b) -> IO b
withDetector d func = do
  fptr <- newDetector d
  withForeignPtr fptr func

newDetector :: Detector a sh -> IO (ForeignPtr C'HklDetector)
newDetector ZeroD = c'hkl_detector_factory_new 0 >>= newForeignPtr p'hkl_detector_free
newDetector _ = error "Can not use 2D detector with the hkl library"

{-# NOINLINE mkDetector #-}
mkDetector :: HklBinocularsDetectorEnum -> Detector Hkl DIM2
mkDetector d
  = let n = toEnum . fromEnum $ d
    in
      unsafePerformIO $ Detector2D d
      <$> (peekCString =<< c'hkl_binoculars_detector_2d_name_get n)
      <*> alloca (\width ->
                    alloca $ \height -> do
                     c'hkl_binoculars_detector_2d_shape_get n width height
                     w <- peek width
                     h <- peek height
                     return $ ix2 (fromEnum h) (fromEnum w))

{-# NOINLINE detectors #-}
detectors :: [Detector Hkl DIM2]
detectors = unsafePerformIO $ return $ map mkDetector [minBound..maxBound]

-- TODO s140 should be the default
defaultDetector :: Detector Hkl DIM2
defaultDetector = mkDetector HklBinocularsDetectorEnum'ImxpadS140

parseDetector2D :: Text -> Either String (Detector Hkl DIM2)
parseDetector2D t = case find (\(Detector2D _ n _) -> n == unpack t) detectors of
                      (Just d) -> Right d
                      Nothing  -> Left ("Unsupported '" ++ unpack t ++ "' detector, select one of -> " ++ Prelude.unwords (sort [n | (Detector2D _ n _) <- detectors]))

--  SomeDetector

data SomeDetector = forall a sh. SomeDetector (Detector a sh)

instance Show SomeDetector where
  show (SomeDetector v) = show v

instance Eq SomeDetector where
    (==) a b = show a == show b

shape :: Detector a sh -> sh
shape ImXpadS140         = ix2 240 560 -- y x
shape PilatusCdTe2M      = ix2 1679 1475
shape Xpad32             = ix2 960 560
shape XpadFlatCorrected  = ix2 1154 576
shape ZeroD              = Z
shape (Detector2D _ _ s) = s

--  Xpad Family

data Normalisation = NoNormalisation | Normalisation
  deriving (Enum, Generic, Show, FromJSON, ToJSON)
--  deriving anyclass (FromJSON, ToJSON)

getPixelsCoordinates :: Detector Hkl DIM2 -> (Int, Int) -> Length Double -> Angle Double -> Normalisation -> IO PixelCoordinates
getPixelsCoordinates (Detector2D d _ sh) (ix0, iy0) sdd detrot norm = do
  let n = toEnum . fromEnum $ d
  parr <- c'hkl_binoculars_detector_2d_coordinates_get n
  let Z :. height :. width = sh
  c'hkl_binoculars_detector_2d_sixs_calibration n parr (toEnum width) (toEnum height) (toEnum ix0) (toEnum iy0) (CDouble (sdd /~ meter)) (CDouble (detrot /~ radian)) (toEnum . fromEnum $ norm)
  arr <- newForeignPtr finalizerFree parr
  return $ fromForeignPtr (ix3 3 height width) (castForeignPtr arr)

fromPtr :: (MonadThrow m, MonadIO m, Shape sh) => sh -> HklDetectorException -> Ptr a -> m (Array F sh a)
fromPtr sh err ptr =
    if ptr == nullPtr
    then throwM err
    else do
      arr <- liftIO $ newForeignPtr finalizerFree ptr
      return $ fromForeignPtr sh (castForeignPtr arr)

getDetectorDefaultFlat :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> m Flat
getDetectorDefaultFlat (Detector2D d _ sh) = do
  let n = toEnum . fromEnum $ d
  fromPtr sh NoDefaultFlat =<< liftIO (c'hkl_binoculars_detector_2d_flat_get n)

getDetectorFlat :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> Text -> m Flat
getDetectorFlat (Detector2D d name sh) flat = do
  let  err = FlatShapeNotcompatible (Data.Text.unwords [pack name, ": ", flat])
  let n = toEnum . fromEnum $ d
  liftIO $ withCString (unpack flat) $
         fromPtr sh err <=< c'hkl_binoculars_detector_2d_flat_load n

getDetectorDefaultMask :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> m Mask
getDetectorDefaultMask (Detector2D d _ sh) = do
  let n = toEnum . fromEnum $ d
  fromPtr sh NoDefaultMask =<< liftIO (c'hkl_binoculars_detector_2d_mask_get n)

getDetectorMask :: (MonadThrow m, MonadIO m) => Detector Hkl DIM2 -> Text -> m Mask
getDetectorMask (Detector2D d name sh)  mask = do
  let  err = MaskShapeNotcompatible (Data.Text.unwords [pack name, ": ", mask])
  let n = toEnum . fromEnum $ d
  liftIO $ withCString (unpack mask) $
         fromPtr sh err <=< c'hkl_binoculars_detector_2d_mask_load n

maskOr :: (MonadThrow m, MonadIO m) =>
         Detector Hkl DIM2 -> Mask -> Mask -> m Mask
maskOr (Detector2D d _ sh) l r = do
  let n =  toEnum . fromEnum $ d
  liftIO $ withForeignPtr (toForeignPtr l) $ \c'l ->
      withForeignPtr
      (toForeignPtr r)
      (fromPtr sh MaskInternalError <=< c'hkl_binoculars_detector_2d_mask_or n c'l)

inDetector :: (Int, Int) -> Detector Hkl DIM2 -> Bool
inDetector (x, y) det = inShape (shape det) (ix2 y x)
