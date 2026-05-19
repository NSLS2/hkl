{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.DataSource
  ( DataSource(..)
  , DataSourceShape(..)
  , DSKind(..)
  , DSWrap_
  , DSAttenuation(..)
  , DSDataset(..)
  , DSDegree(..)
  , DSDouble(..)
  , DSDoubles(..)
  , DSFlat(..)
  , DSFloat(..)
  , DSGeometry(..)
  , DSImage(..)
  , DSMask(..)
  , DSTimestamp(..)
  , DSTimescan0(..)
  , DSScannumber(..)
  , HklBinocularsException(..)
  , Is0DStreamable(..)
  , Is1DStreamable(..)
  , Is2DStreamable(..)
  , combine'Shape
  , size'DataSourceShape
  , withDataSourcesP
  ) where

import           Bindings.HDF5.Core                (Location)
import           Bindings.HDF5.Dataset             (getDatasetType)
import           Bindings.HDF5.Datatype            (getTypeSize, nativeTypeOf,
                                                    typeIDsEqual)
import           Control.Exception                 (throwIO)
import           Control.Monad.Extra               (ifM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import           Data.Int                          (Int32)
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unpack)
import           Data.Typeable                     (Typeable, typeOf)
import           Data.Vector.Storable              (Vector, fromList)
import           Data.Vector.Storable.Mutable      (IOVector, replicate,
                                                    unsafeNew)
import           Data.Word                         (Word16, Word32)
import           Foreign.C.Types                   (CDouble (..))
#if !MIN_VERSION_base(4, 18, 0)
import           GHC.Base                          (liftA2)
#endif
import           GHC.Float                         (float2Double)
import           GHC.Generics                      (Generic (..), K1 (..),
                                                    M1 (..), Rep, U1 (..),
                                                    (:*:) (..), (:+:) (..))
import           Numeric.Units.Dimensional.Prelude (degree, (*~), (/~))
import           Pipes.Safe                        (MonadSafe, catch, throwM)
import           Text.Printf                       (printf)

import           Prelude                           hiding (filter, replicate)

import           Hkl.Binoculars.Config
import           Hkl.Detector
import           Hkl.Exception
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Repa
import           Hkl.Types


--------------------
-- Is0DStreamable --
--------------------

class Is0DStreamable a e where
    extract0DStreamValue :: a -> IO e

-- Is0DStreamable (instances)

instance Is0DStreamable Degree Double where
    extract0DStreamValue (Degree d) = pure (d /~ degree)

instance Is0DStreamable Degree CDouble where
    extract0DStreamValue (Degree d) = pure $ CDouble (d /~ degree)

instance Is0DStreamable Double CDouble where
    extract0DStreamValue d = pure $ CDouble d

instance Is0DStreamable (DSDataset sh Double DSAcq) Degree where
    extract0DStreamValue (DataSourceAcq'Dataset d)
        = Degree <$> do
            v <- getPosition d 0
            return $ v *~ degree

instance Is0DStreamable (DSDataset sh Double DSAcq) Double where
    extract0DStreamValue (DataSourceAcq'Dataset d) = getPosition d 0

instance Is0DStreamable (DSDataset sh Double DSAcq) CDouble where
    extract0DStreamValue (DataSourceAcq'Dataset d) = getPosition d 0

instance Is0DStreamable (DSDegree DSAcq) Degree where
    extract0DStreamValue (DataSourceAcq'Degree'Hdf5 d)  = extract0DStreamValue d
    extract0DStreamValue (DataSourceAcq'Degree'Const d) = pure d

instance Is0DStreamable (DSDegree DSAcq) Double where
    extract0DStreamValue (DataSourceAcq'Degree'Hdf5 d)  = extract0DStreamValue d
    extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DSDegree DSAcq) CDouble where
    extract0DStreamValue (DataSourceAcq'Degree'Hdf5 d)  = extract0DStreamValue d
    extract0DStreamValue (DataSourceAcq'Degree'Const d) = extract0DStreamValue d

instance Is0DStreamable (DSDouble DSAcq) Double where
    extract0DStreamValue (DataSourceAcq'Double'Hdf5 d)  = extract0DStreamValue d
    extract0DStreamValue (DataSourceAcq'Double'Hdf5WithShift d _) = extract0DStreamValue d
    extract0DStreamValue (DataSourceAcq'Double'Const a) = pure a

instance Is0DStreamable (DSFlat DSAcq) Flat where
    extract0DStreamValue (DataSourceAcq'Flat m)    = pure m

instance Is0DStreamable (DSMask DSAcq) (Maybe Mask) where
    extract0DStreamValue DataSourceAcq'Mask'NoMask = pure Nothing
    extract0DStreamValue (DataSourceAcq'Mask m)    = pure $ Just m

instance Is0DStreamable (DSScannumber DSAcq) Scannumber where
    extract0DStreamValue (DataSourceAcq'Scannumber'Const sn) = pure sn

instance Is0DStreamable (DSTimescan0 DSAcq) Timescan0 where
    extract0DStreamValue (DataSourceAcq'Timescan0'Hdf5 ds) = Timescan0 <$> extract0DStreamValue ds
    extract0DStreamValue DataSourceAcq'Timescan0'NoTimescan0 = pure $ Timescan0 0

--------------------
-- Is1DStreamable --
--------------------

class Is1DStreamable a e where
  extract1DStreamValue :: a -> Int -> IO e

-- Is1DStreamable (instances)

badAttenuation :: Float
badAttenuation = -100

instance Is1DStreamable (DSDataset DIM2 Double DSAcq) CDouble where
    extract1DStreamValue (DataSourceAcq'Dataset ds) = getPosition ds

instance Is1DStreamable (DSDataset DIM2 Double DSAcq) Double where
    extract1DStreamValue (DataSourceAcq'Dataset ds) = getPosition ds

instance Is1DStreamable (DSDataset DIM2 Float DSAcq) Float where
    extract1DStreamValue (DataSourceAcq'Dataset ds) = getPosition ds

instance Is1DStreamable (DSAttenuation DSAcq) Attenuation where
    extract1DStreamValue (DataSourceAcq'Attenuation ds offset coef mmax) i =
        Attenuation <$> do
          v <-  extract1DStreamValue ds (i + offset)
          if v == badAttenuation
          then throwIO (WrongAttenuation "attenuation is wrong" (i + offset) (float2Double v))
          else case mmax of
                 Just m -> if v > m
                          then throwIO (WrongAttenuation "max inserted filters exceeded" (i + offset) (float2Double v))
                          else return (coef ** float2Double v)
                 Nothing -> return (coef ** float2Double v)
    extract1DStreamValue (DataSourceAcq'ApplyedAttenuationFactor ds) i = Attenuation . float2Double <$> extract1DStreamValue ds i
    extract1DStreamValue DataSourceAcq'NoAttenuation _                 = pure $ Attenuation 1

instance Is1DStreamable (DSDouble DSAcq) CDouble where
    extract1DStreamValue (DataSourceAcq'Double'Hdf5 d) i  = extract1DStreamValue d i
    extract1DStreamValue (DataSourceAcq'Double'Hdf5WithShift d s) i  = extract1DStreamValue d (i + s)
    extract1DStreamValue (DataSourceAcq'Double'Const d) _ = extract0DStreamValue d

instance Is1DStreamable (DSDoubles DSAcq) (Data.Vector.Storable.Vector CDouble) where
    extract1DStreamValue (DataSourceAcq'List ds) i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds

instance Is1DStreamable (DSFloat DSAcq) Float where
    extract1DStreamValue (DataSourceAcq'Float'Hdf5 ds) = extract1DStreamValue ds

instance  Is1DStreamable (DSGeometry DSAcq) Geometry where
     extract1DStreamValue (DataSourceAcq'Geometry g w' as') i =
         do w <- extract0DStreamValue w'
            as <- extract1DStreamValue as' i
            let state = GeometryState w as
            pure $ case g of
                     (Geometry'Custom axes _) -> Geometry'Custom axes (Just state)
                     (Geometry'Factory factory _) -> Geometry'Factory factory (Just state)

instance Is1DStreamable (DSImage DSAcq) Image where
    extract1DStreamValue (DataSourceAcq'Image'Dummy buf) _                    = pure $ ImageDouble buf
    extract1DStreamValue (DataSourceAcq'Image'Hdf5'Double det ds buf) i       = ImageDouble <$> getArrayInBuffer buf det ds (Z :. i)
    extract1DStreamValue (DataSourceAcq'Image'Hdf5'Int32 det ds buf) i        = ImageInt32 <$> getArrayInBuffer buf det ds (Z :. i)
    extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word16 det ds buf) i       = ImageWord16 <$> getArrayInBuffer buf det ds (Z :. i)
    extract1DStreamValue (DataSourceAcq'Image'Hdf5'Word32 det ds buf) i       = ImageWord32 <$> getArrayInBuffer buf det ds (Z :. i)
    extract1DStreamValue (DataSourceAcq'Image'Img'Int32 det buf tmpl sn fn) i = ImageInt32 <$> readImgInBuffer buf det (fn tmpl sn i)

instance Is1DStreamable (DSTimestamp DSAcq) Timestamp where
    extract1DStreamValue (DataSourceAcq'Timestamp'Hdf5 ds) i   = Timestamp <$> extract1DStreamValue ds i
    extract1DStreamValue DataSourceAcq'Timestamp'NoTimestamp _ = pure $ Timestamp 0

--------------------
-- Is2DStreamable --
--------------------

class Is2DStreamable a e where
  extract2DStreamValue :: a -> DIM2 -> IO e

-- Is2DStreamable (instances)

instance Is2DStreamable (DSDataset DIM2 Double DSAcq) Double where
    extract2DStreamValue (DataSourceAcq'Dataset ds) = getPositionNew ds

instance Is2DStreamable (DSDataset DIM2 Double DSAcq) CDouble where
    extract2DStreamValue (DataSourceAcq'Dataset ds) = getPositionNew ds

instance Is2DStreamable (DSDataset DIM2 Float DSAcq) Float where
    extract2DStreamValue (DataSourceAcq'Dataset ds) = getPositionNew ds

instance Is2DStreamable (DSDouble DSAcq) CDouble where
    extract2DStreamValue (DataSourceAcq'Double'Hdf5 d) i  = extract2DStreamValue d i
    extract2DStreamValue (DataSourceAcq'Double'Hdf5WithShift d s) (Z :. j :. i)  = extract2DStreamValue d (Z :. j :. (i + s))
    extract2DStreamValue (DataSourceAcq'Double'Const d) _ = extract0DStreamValue d

instance Is2DStreamable (DSFloat DSAcq) Float where
    extract2DStreamValue (DataSourceAcq'Float'Hdf5 ds) = extract2DStreamValue ds

instance Is2DStreamable (DSAttenuation DSAcq) Attenuation where
    extract2DStreamValue (DataSourceAcq'Attenuation ds offset coef mmax) (Z :. j :. i) =
        Attenuation <$> do
          v <-  extract2DStreamValue ds (Z :. j :. (i + offset))
          if v == badAttenuation
          then throwIO (WrongAttenuation "attenuation is wrong" (i + offset) (float2Double v))
          else case mmax of
                 Just m -> if v > m
                          then throwIO (WrongAttenuation "max inserted filters exceeded" (i + offset) (float2Double v))
                          else return (coef ** float2Double v)
                 Nothing -> return (coef ** float2Double v)
    extract2DStreamValue (DataSourceAcq'ApplyedAttenuationFactor ds) i = Attenuation . float2Double <$> extract2DStreamValue ds i
    extract2DStreamValue DataSourceAcq'NoAttenuation _                 = pure $ Attenuation 1

instance Is2DStreamable (DSDoubles DSAcq) (Data.Vector.Storable.Vector CDouble) where
    extract2DStreamValue (DataSourceAcq'List ds) i = fromList <$> Prelude.mapM (`extract2DStreamValue` i) ds

instance Is2DStreamable (DSGeometry DSAcq) Geometry where
     extract2DStreamValue (DataSourceAcq'Geometry g w' as') i =
         do w <- extract0DStreamValue w'
            as <- extract2DStreamValue as' i
            let state = GeometryState w as
            pure $ case g of
                     (Geometry'Custom axes _) -> Geometry'Custom axes (Just state)
                     (Geometry'Factory factory _) -> Geometry'Factory factory (Just state)

instance Is2DStreamable (DSImage DSAcq) Image where
    extract2DStreamValue (DataSourceAcq'Image'Dummy buf) _                    = pure $ ImageDouble buf
    extract2DStreamValue (DataSourceAcq'Image'Hdf5'Double det ds buf) i       = ImageDouble <$> getArrayInBuffer buf det ds i
    extract2DStreamValue (DataSourceAcq'Image'Hdf5'Int32 det ds buf) i        = ImageInt32 <$> getArrayInBuffer buf det ds i
    extract2DStreamValue (DataSourceAcq'Image'Hdf5'Word16 det ds buf) i       = ImageWord16 <$> getArrayInBuffer buf det ds i
    extract2DStreamValue (DataSourceAcq'Image'Hdf5'Word32 det ds buf) i       = ImageWord32 <$> getArrayInBuffer buf det ds i
    extract2DStreamValue (DataSourceAcq'Image'Img'Int32 _det _buf _tmpl _sn _fn) _i = undefined

instance Is2DStreamable (DSTimestamp DSAcq) Timestamp where
    extract2DStreamValue (DataSourceAcq'Timestamp'Hdf5 ds) i   = Timestamp <$> extract2DStreamValue ds i
    extract2DStreamValue DataSourceAcq'Timestamp'NoTimestamp _ = pure $ Timestamp 0

----------------
-- DataSource --
----------------

data DSKind = DSPath | DSAcq

type family DSWrap (k :: DSKind) (t :: Type) :: Type where
   DSWrap DSAcq t = t
   DSWrap DSPath t = [t]

type DSWrap_ f k = DSWrap k (f k)

data DataSourceShape
    = DataSourceShape'Range !DIM1 !DIM1
    | DataSourceShape'Range'DIM2 !DIM1 !DIM1 !DIM1
      deriving (Eq, Show)

combine'Shape :: DataSourceShape -> DataSourceShape -> DataSourceShape
combine'Shape (DataSourceShape'Range _ (Z :. 1)) s = s
combine'Shape s (DataSourceShape'Range _ (Z :. 1)) = s
combine'Shape (DataSourceShape'Range (Z :. f1) (Z :. t1)) (DataSourceShape'Range (Z :. f2) (Z :. t2))
    = DataSourceShape'Range (ix1 (max f1 f2)) (ix1 (min t1 t2))
combine'Shape (DataSourceShape'Range (Z :. f1) (Z :. t1)) (DataSourceShape'Range'DIM2 (Z :. f2) (Z :. t2) n)
    = DataSourceShape'Range'DIM2 (ix1 (max f1 f2)) (ix1 (min t1 t2)) n
combine'Shape (DataSourceShape'Range'DIM2 ( Z :. f1) (Z :. t1) n) (DataSourceShape'Range (Z :. f2) (Z :. t2))
    = DataSourceShape'Range'DIM2 (ix1 (max f1 f2)) (ix1 (min t1 t2)) n
combine'Shape (DataSourceShape'Range'DIM2 (Z :. f1) (Z :. t1) (Z :. n1)) (DataSourceShape'Range'DIM2 (Z :. f2) (Z :. t2) (Z :. n2))
    = DataSourceShape'Range'DIM2 (ix1 (max f1 f2)) (ix1 (min t1 t2)) (ix1 (min n1 n2))

shape1 :: DataSourceShape
shape1 = DataSourceShape'Range (Z :. 0) (Z :. 1)

ds'Shape'Dataset :: Dataset -> IO DataSourceShape
ds'Shape'Dataset ds = do
  (ts, mts) <- datasetShape ds
  pure $ case length ts of
           1 -> DataSourceShape'Range (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0))))
           2 -> DataSourceShape'Range'DIM2 (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0)))) (ix1 (fromIntegral (maybe (ts !! 1) id (mts !! 1))))
           _ -> DataSourceShape'Range'DIM2 (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0)))) (ix1 (fromIntegral (maybe (ts !! 1) id (mts !! 1))))

size'DataSourceShape :: DataSourceShape -> Int
size'DataSourceShape (DataSourceShape'Range (Z :. f) (Z :. t))        = t - f
size'DataSourceShape (DataSourceShape'Range'DIM2 (Z :. f) (Z :. t) (Z :. n)) = (t - f) * n

-- | Generic 'ds'Shape'

generic'ds'Shape :: ( MonadSafe m
                   , Generic (d DSAcq)
                   , GDataSourceAcq (Rep (d DSAcq))
                   )
                 => d DSAcq -> m DataSourceShape
generic'ds'Shape = g'ds'Shape . from

class GDSAK1 a where
   g'ds'Shape'K1 :: MonadSafe m => a -> m DataSourceShape
   g'ds'Shape'K1 _ = pure shape1

instance DataSource a => GDSAK1 (a DSAcq) where
   g'ds'Shape'K1 = ds'Shape

instance GDSAK1 Dataset where
    g'ds'Shape'K1 = liftIO . ds'Shape'Dataset

instance GDSAK1 Degree
instance GDSAK1 (Detector a sh)
instance GDSAK1 Double
instance GDSAK1 Flat
instance GDSAK1 Geometry
instance GDSAK1 Int
instance GDSAK1 (IOVector a)
instance GDSAK1 Mask
instance GDSAK1 Scannumber
instance GDSAK1 Text
instance GDSAK1 (Text -> Scannumber -> Int -> FilePath)

class GDataSourceAcq dataAcq where
   g'ds'Shape :: MonadSafe m => dataAcq x -> m DataSourceShape

instance GDataSourceAcq f => GDataSourceAcq (M1 i c f) where
   g'ds'Shape (M1 f) = g'ds'Shape f

instance (GDataSourceAcq f, GDataSourceAcq f') => GDataSourceAcq (f :*: f') where
   g'ds'Shape (f :*: f') = liftA2 combine'Shape (g'ds'Shape f) (g'ds'Shape f')

instance (GDataSourceAcq f, GDataSourceAcq f') => GDataSourceAcq (f :+: f') where
   g'ds'Shape (L1 f)  = g'ds'Shape f
   g'ds'Shape (R1 f') = g'ds'Shape f'

instance GDSAK1 a => GDataSourceAcq (K1 i a) where
    g'ds'Shape (K1 a) = g'ds'Shape'K1 a

instance GDataSourceAcq U1 where
    g'ds'Shape _ = pure shape1

-- | Generic 'withDataSourceP'

generic'withDataSourceP :: ( Generic (d DSPath)
                          , Generic (d DSAcq)
                          , GDataSourcePath (Rep (d DSPath)) (Rep (d DSAcq))
                          , Location l
                          , MonadSafe m
                          )
                        => ScanFile l -> d DSPath -> (d DSPath -> d DSAcq -> m r) -> m r
generic'withDataSourceP file src gg = g'withDataSourceP file (from src) (\path acq -> gg (to path) (to acq))


class GDataSourcePath dataPath dataAcq where
    g'withDataSourceP :: (Location l, MonadSafe m)
                      => ScanFile l -> dataPath x -> (dataPath x -> dataAcq x -> m r) -> m r

instance GDataSourcePath f g => GDataSourcePath (M1 i c f) (M1 i c' g) where
    g'withDataSourceP f (M1 d) gg = g'withDataSourceP f d (\path acq -> gg (M1 path) (M1 acq))

instance (GDataSourcePath f g, GDataSourcePath f' g') => GDataSourcePath (f :*: f') (g :*: g') where
    g'withDataSourceP file (f :*: f') gg =
        g'withDataSourceP file f $ \path g ->
        g'withDataSourceP file f' $ \path' g' ->
            gg (path :*: path') (g :*: g')

instance (Show (a DSPath), Typeable (a DSPath), DataSource a) => GDataSourcePath (K1 i [a DSPath]) (K1 i (a DSAcq)) where
    g'withDataSourceP file (K1 acqs) gg =
        withDataSourcesP file acqs $ \acq dat ->
            gg (K1 [acq]) (K1 dat)

-- DataSource

class DataSource d where
    ds'Shape :: MonadSafe m => d DSAcq -> m DataSourceShape
    withDataSourceP :: (Location l, MonadSafe m) => ScanFile l -> d DSPath -> (d DSPath -> d DSAcq -> m r) -> m r

    default ds'Shape :: ( MonadSafe m
                       , Generic (d DSAcq)
                       , GDataSourceAcq (Rep (d DSAcq)))
                     => d DSAcq -> m DataSourceShape
    ds'Shape = generic'ds'Shape

    default withDataSourceP :: ( Generic (d DSPath)
                              , Generic (d DSAcq)
                              , GDataSourcePath (Rep (d DSPath)) (Rep (d DSAcq))
                              , Location l
                              , MonadSafe m
                              )
                            => ScanFile l -> d DSPath -> (d DSPath -> d DSAcq -> m r) -> m r
    withDataSourceP = generic'withDataSourceP

withDataSourcesP :: (DataSource d, Location l, MonadSafe m, Show (d DSPath), Typeable (d DSPath))
                 => ScanFile l -> [d DSPath] -> (d DSPath -> d DSAcq -> m r) -> m r
withDataSourcesP f paths g = go paths []
  where
    msg = show . typeOf $ paths
    go [] acc = throwM $ HklDataSourceException'NoRemainingDataPath (msg <> show paths : acc)
    go (s : ss) acc = withDataSourceP f s g `catch` \(exl :: HklDataSourceException) ->
                      case exl of
                        HklDataSourceException'NoRemainingDataPath prev -> do
                                 if null ss
                                 then throwM $ HklDataSourceException'NoRemainingDataPath (msg : prev)
                                 else go ss prev
                        _ -> go ss acc

-- DataSource (instances)

-- Attenuation

data family DSAttenuation (k :: DSKind)
data instance DSAttenuation DSPath
    = DataSourcePath'Attenuation { attenuationPath            :: DSWrap_ DSFloat DSPath
                                 , attenuationPathOffset      :: Int
                                 , attenuationPathCoefficient :: Double
                                 , attenuationPathMax         :: Maybe Float
                                 }
    | DataSourcePath'ApplyedAttenuationFactor { attenuationPath :: DSWrap_ DSFloat DSPath }
    | DataSourcePath'NoAttenuation
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data instance DSAttenuation DSAcq
    = DataSourceAcq'Attenuation { attenuationAcqPath        :: DSWrap_ DSFloat DSAcq
                                , attenuationAcqOffset      :: Int
                                , attenuationAcqCoefficient :: Double
                                , attenuationAcqMax         :: Maybe Float
                                }
    | DataSourceAcq'ApplyedAttenuationFactor { attenuationAcqPath :: DSWrap_ DSFloat DSAcq }
    | DataSourceAcq'NoAttenuation
    deriving (Generic)

instance DataSource DSAttenuation where
  ds'Shape (DataSourceAcq'Attenuation fp off _ _)
      = do sh <- ds'Shape fp
           pure $ case sh of
                    DataSourceShape'Range f (Z :. t) -> DataSourceShape'Range f (Z :. (t - off))
                    DataSourceShape'Range'DIM2 f (Z :. t) n -> DataSourceShape'Range'DIM2 f (Z :. (t - off)) n
  ds'Shape (DataSourceAcq'ApplyedAttenuationFactor fp) = ds'Shape fp
  ds'Shape DataSourceAcq'NoAttenuation                 = pure shape1


  withDataSourceP f (DataSourcePath'Attenuation ps o c m) g = withDataSourcesP f ps $ \p ds -> g (DataSourcePath'Attenuation [p] o c m) (DataSourceAcq'Attenuation ds o c m)
  withDataSourceP f (DataSourcePath'ApplyedAttenuationFactor ps) g = withDataSourcesP f ps $ \p ds -> g (DataSourcePath'ApplyedAttenuationFactor [p]) (DataSourceAcq'ApplyedAttenuationFactor ds)
  withDataSourceP _ DataSourcePath'NoAttenuation g = g DataSourcePath'NoAttenuation DataSourceAcq'NoAttenuation

-- Dataset

data family DSDataset (sh :: Type) (a :: Type) (k :: DSKind)
newtype instance DSDataset sh a DSPath
    = DataSourcePath'Dataset (Hdf5Path sh a)
    deriving (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Show (DSDataset sh a DSPath) where
    show (DataSourcePath'Dataset p) = show p

newtype instance DSDataset sh a DSAcq
    = DataSourceAcq'Dataset Dataset
      deriving Generic

instance DataSource (DSDataset sh a) where
    withDataSourceP (ScanFile f _) path@(DataSourcePath'Dataset p) g
        = withHdf5PathP f p $ \ds -> g path (DataSourceAcq'Dataset ds)

-- Degree

data family DSDegree (k :: DSKind)
data instance DSDegree DSPath
    = DataSourcePath'Degree'Hdf5 (DSWrap_ (DSDataset Z Double) DSPath)
    | DataSourcePath'Degree'Const Degree
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSDegree DSAcq
    = DataSourceAcq'Degree'Hdf5 (DSWrap_ (DSDataset Z Double) DSAcq)
    | DataSourceAcq'Degree'Const Degree
    deriving Generic

instance DataSource DSDegree where
  withDataSourceP f (DataSourcePath'Degree'Hdf5 ps) g
    = withDataSourcesP f ps $ \p p' -> g (DataSourcePath'Degree'Hdf5 [p]) (DataSourceAcq'Degree'Hdf5 p')
  withDataSourceP _ path@(DataSourcePath'Degree'Const d) g = g path (DataSourceAcq'Degree'Const d)

-- Double

data family DSDouble (k :: DSKind)
data instance DSDouble DSPath
    = DataSourcePath'Double'Hdf5 (DSWrap_ (DSDataset DIM2 Double) DSPath)
    | DataSourcePath'Double'Hdf5WithShift (DSWrap_ (DSDataset DIM2 Double) DSPath) Int
    | DataSourcePath'Double'Ini ConfigContent Section Key
    | DataSourcePath'Double'Const Double
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSDouble DSAcq
    = DataSourceAcq'Double'Hdf5 (DSWrap_ (DSDataset DIM2 Double) DSAcq)
    | DataSourceAcq'Double'Hdf5WithShift (DSWrap_ (DSDataset DIM2 Double) DSAcq) Int
    | DataSourceAcq'Double'Const Double
    deriving Generic

instance DataSource DSDouble where
  ds'Shape (DataSourceAcq'Double'Hdf5 p) = ds'Shape p
  ds'Shape (DataSourceAcq'Double'Hdf5WithShift p s)
      = do sh <- ds'Shape p
           pure $ case sh of
                    DataSourceShape'Range f (Z :. t) -> DataSourceShape'Range f (Z :. (t - s))
                    DataSourceShape'Range'DIM2 f (Z :. t) n -> DataSourceShape'Range'DIM2 f (Z :. (t - s)) n
  ds'Shape (DataSourceAcq'Double'Const _) = pure shape1

  withDataSourceP f (DataSourcePath'Double'Hdf5 ps) g
      = withDataSourcesP f ps $ \p ds -> do
                               sh <- ds'Shape ds
                               if sh == shape1
                               then do
                                 v <- liftIO $ extract0DStreamValue ds
                                 g (DataSourcePath'Double'Const v) (DataSourceAcq'Double'Const v)
                               else g (DataSourcePath'Double'Hdf5 [p]) (DataSourceAcq'Double'Hdf5 ds)
  withDataSourceP f (DataSourcePath'Double'Hdf5WithShift ps shift) g
      = withDataSourcesP f ps $ \p ds -> do
                               sh <- ds'Shape ds
                               if sh == shape1
                               then do
                                 v <- liftIO $ extract0DStreamValue ds
                                 g (DataSourcePath'Double'Const v) (DataSourceAcq'Double'Const v)
                               else g (DataSourcePath'Double'Hdf5WithShift [p] shift) (DataSourceAcq'Double'Hdf5WithShift ds shift)
  withDataSourceP _ path@(DataSourcePath'Double'Const a) g = g path (DataSourceAcq'Double'Const a)
  withDataSourceP _ path@(DataSourcePath'Double'Ini (ConfigContent cfg) s k) g =
      eitherF (const $ throwM $ CanNotOpenDataSource'Double'Ini s k) (parse' cfg s k)
      (\case
        Nothing -> throwM $ CanNotOpenDataSource'Double'Ini s k
        Just v  ->  g path (DataSourceAcq'Double'Const v))

-- [Double]

data family DSDoubles (k :: DSKind)
newtype instance DSDoubles DSPath
    = DataSourcePath'List [DSWrap_ DSDouble DSPath]
      deriving (Generic, FromJSON, Show, ToJSON)

newtype instance DSDoubles DSAcq
    = DataSourceAcq'List [DSWrap_ DSDouble DSAcq]
    deriving Generic

instance DataSource DSDoubles where
    ds'Shape  (DataSourceAcq'List ds)
        = do ss <- mapM ds'Shape ds
             pure $ foldl1 combine'Shape ss

    withDataSourceP f (DataSourcePath'List pathss) g
        = go pathss [] []
          where
            go [] ps as = g (DataSourcePath'List ps) (DataSourceAcq'List as)
            go (ps : pss) accPath accAcq = withDataSourcesP f ps $ \p s -> go pss (accPath ++ [[p]]) (accAcq ++ [s])

-- Flat

data family DSFlat (k :: DSKind)
data instance DSFlat DSPath
    = DataSourcePath'Flat FlatLocation (Detector Hkl DIM2)
      deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data instance DSFlat DSAcq
    = DataSourceAcq'Flat Flat
    deriving Generic

instance DataSource DSFlat where
    withDataSourceP (ScanFile _ sn) path@(DataSourcePath'Flat l d) g
        = do  m <- getFlat l d sn
              g path (DataSourceAcq'Flat m)

-- Float

data family DSFloat (k :: DSKind)
newtype instance DSFloat DSPath
    = DataSourcePath'Float'Hdf5 (DSWrap_ (DSDataset DIM2 Float) DSPath)
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
newtype instance DSFloat DSAcq
    = DataSourceAcq'Float'Hdf5 (DSWrap_ (DSDataset DIM2 Float) DSAcq)
    deriving Generic

instance DataSource DSFloat where
    withDataSourceP f (DataSourcePath'Float'Hdf5 ps) g = withDataSourcesP f ps $ \p ds -> g (DataSourcePath'Float'Hdf5 [p]) (DataSourceAcq'Float'Hdf5 ds)

-- Geometry

data family DSGeometry (k :: DSKind)
data instance DSGeometry DSPath
    = DataSourcePath'Geometry { geometryGeometry       :: Geometry
                              , geometryPathWavelength :: DSWrap_ DSDouble DSPath
                              , geometryPathAxes       :: DSWrap_ DSDoubles DSPath
                              }
    | DataSourcePath'Geometry'Fix { geometryPathWavelength :: DSWrap_ DSDouble DSPath }
    deriving (Generic, Show, FromJSON, ToJSON)

data instance DSGeometry DSAcq
    = DataSourceAcq'Geometry
      Geometry
      (DSWrap_ DSDouble DSAcq)
      (DSWrap_ DSDoubles DSAcq)
    deriving Generic

instance DataSource DSGeometry where
  withDataSourceP f (DataSourcePath'Geometry g wPaths asPaths) gg =
    withDataSourcesP f wPaths $ \wPath w' ->
    withDataSourcesP f asPaths $ \asPath as' -> do
    gg (DataSourcePath'Geometry g [wPath] [asPath]) (DataSourceAcq'Geometry g w' as')
  withDataSourceP f (DataSourcePath'Geometry'Fix wPaths) gg =
    withDataSourcesP f wPaths $ \wPath w' -> do
    gg (DataSourcePath'Geometry'Fix [wPath]) (DataSourceAcq'Geometry fixed w' (DataSourceAcq'List []))

-- Image

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

data family DSImage (k :: DSKind)

data instance DSImage DSPath
  = DataSourcePath'Image'Dummy (Detector Hkl DIM2) Double
  | DataSourcePath'Image'Hdf5 (Detector Hkl DIM2) (DSWrap_ (DSDataset DIM3 Int32) DSPath) -- TODO Int32 is wrong
  | DataSourcePath'Image'Img (Detector Hkl DIM2) Text Scannumber
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSImage DSAcq
    = DataSourceAcq'Image'Dummy (IOVector Double)
    | DataSourceAcq'Image'Hdf5'Double (Detector Hkl DIM2) Dataset (IOVector Double)
    | DataSourceAcq'Image'Hdf5'Int32 (Detector Hkl DIM2) Dataset (IOVector Int32)
    | DataSourceAcq'Image'Hdf5'Word16 (Detector Hkl DIM2) Dataset (IOVector Word16)
    | DataSourceAcq'Image'Hdf5'Word32 (Detector Hkl DIM2) Dataset (IOVector Word32)
    | DataSourceAcq'Image'Img'Int32 (Detector Hkl DIM2) (IOVector Int32) Text Scannumber (Text -> Scannumber -> Int -> FilePath)
    deriving Generic


ds'Shape'Dataset'Image :: Dataset -> IO DataSourceShape
ds'Shape'Dataset'Image ds = do
  (ts, mts) <- datasetShape ds
  pure $ case length ts of
           0 -> error "Not an Image dataset 0 dim"
           1 -> error "Not an Image dataset 1 dim"
           2 -> shape1
           3 -> DataSourceShape'Range (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0))))
           4 -> DataSourceShape'Range'DIM2 (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0)))) (ix1 (fromIntegral (maybe (ts !! 1) id (mts !! 1))))
           _ -> DataSourceShape'Range'DIM2 (ix1 0) (ix1 (fromIntegral (maybe (ts !! 0) id (mts !! 0)))) (ix1 (fromIntegral (maybe (ts !! 1) id (mts !! 1))))


instance DataSource DSImage where
    ds'Shape (DataSourceAcq'Image'Dummy _) = pure $ shape1
    ds'Shape (DataSourceAcq'Image'Hdf5'Double _ ds _) = liftIO $ ds'Shape'Dataset'Image ds
    ds'Shape (DataSourceAcq'Image'Hdf5'Int32  _ ds _) = liftIO $ ds'Shape'Dataset'Image ds
    ds'Shape (DataSourceAcq'Image'Hdf5'Word16 _ ds _) = liftIO $ ds'Shape'Dataset'Image ds
    ds'Shape (DataSourceAcq'Image'Hdf5'Word32 _ ds _) = liftIO $ ds'Shape'Dataset'Image ds
    ds'Shape (DataSourceAcq'Image'Img'Int32 _ _ _ _ _) = pure $ shape1

    withDataSourceP _ path@(DataSourcePath'Image'Dummy det v) g
        =  do let n = size . shape $ det
              arr <- liftIO $ replicate n v
              g path (DataSourceAcq'Image'Dummy arr)

    withDataSourceP f (DataSourcePath'Image'Hdf5 det ps) g
        = withDataSourcesP f ps $ \p (DataSourceAcq'Dataset ds) -> do
                              let path = DataSourcePath'Image'Hdf5 det [p]
                              t <- liftIO $ getDatasetType ds
                              s <- liftIO $ getTypeSize t
                              let n = (size . shape $ det) * fromEnum s
                              condM [ (liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Double)), do
                                         arr <- liftIO $ unsafeNew n
                                         g path (DataSourceAcq'Image'Hdf5'Double det ds arr))
                                    , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Int32)), do
                                         arr <- liftIO $ unsafeNew n
                                         g path (DataSourceAcq'Image'Hdf5'Int32 det ds arr))
                                    , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16)), do
                                         arr <- liftIO $ unsafeNew n
                                         g path (DataSourceAcq'Image'Hdf5'Word16 det ds arr))
                                    , (liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32)), do
                                         arr <- liftIO $ unsafeNew n
                                         g path (DataSourceAcq'Image'Hdf5'Word32 det ds arr))
                                    ]

    withDataSourceP (ScanFile _ sn) path@(DataSourcePath'Image'Img det tmpl (Scannumber sn0)) g
        = do let n = size . shape $ det
             arr <- liftIO $ unsafeNew n
             g path (DataSourceAcq'Image'Img'Int32 det arr tmpl sn f)
                 where
                   f :: Text -> Scannumber -> Int -> FilePath
                   f tmpl' (Scannumber sn') i = printf (unpack tmpl') sn0 sn0 ((sn' - sn0) * 1029 + i)

instance HasFieldValue [DSImage DSPath] where
    fieldvalue = autoJSON

-- Int

data family DSInt (k :: DSKind)
newtype instance DSInt DSPath
  = DataSourcePath'Int Int
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype instance DSInt DSAcq
    = DataSourceAcq'Int Int
    deriving Generic

instance DataSource DSInt where
  withDataSourceP _ path@(DataSourcePath'Int p) g = g path (DataSourceAcq'Int p)

-- Mask

data family DSMask (k :: DSKind)
data instance DSMask DSPath
    = DataSourcePath'Mask'NoMask
    | DataSourcePath'Mask MaskLocation (Detector Hkl DIM2)
      deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data instance DSMask DSAcq
    = DataSourceAcq'Mask Mask
    | DataSourceAcq'Mask'NoMask
    deriving Generic

instance DataSource DSMask where
    withDataSourceP _ path@DataSourcePath'Mask'NoMask g = g path DataSourceAcq'Mask'NoMask
    withDataSourceP (ScanFile _ sn) path@(DataSourcePath'Mask l d) g
        = do  m <- getMask l d sn
              g path (DataSourceAcq'Mask m)

-- Scannumber

data family DSScannumber (k :: DSKind)
data instance DSScannumber DSPath
    = DataSourcePath'Scannumber
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

newtype instance DSScannumber DSAcq
    = DataSourceAcq'Scannumber'Const Scannumber
    deriving Generic

instance DataSource DSScannumber where
  withDataSourceP (ScanFile _ s) path@DataSourcePath'Scannumber g = g path (DataSourceAcq'Scannumber'Const s)


-- Timestamp

data family DSTimestamp (k :: DSKind)
data instance DSTimestamp DSPath
    = DataSourcePath'Timestamp'Hdf5 (DSWrap_ (DSDataset DIM2 Double) DSPath)
    | DataSourcePath'Timestamp'NoTimestamp
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSTimestamp DSAcq
    = DataSourceAcq'Timestamp'Hdf5 (DSWrap_ (DSDataset DIM2 Double) DSAcq)
    | DataSourceAcq'Timestamp'NoTimestamp
    deriving Generic

instance DataSource DSTimestamp where
    withDataSourceP f (DataSourcePath'Timestamp'Hdf5 ps) g = withDataSourcesP f ps $ \p ds -> g (DataSourcePath'Timestamp'Hdf5 [p]) (DataSourceAcq'Timestamp'Hdf5 ds)
    withDataSourceP _ path@DataSourcePath'Timestamp'NoTimestamp g = g path DataSourceAcq'Timestamp'NoTimestamp

-- Timescan0

data family DSTimescan0 (k :: DSKind)
data instance DSTimescan0 DSPath
    = DataSourcePath'Timescan0'Hdf5 (DSWrap_ (DSDataset Z Double) DSPath)
    | DataSourcePath'Timescan0'NoTimescan0
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DSTimescan0 DSAcq
    = DataSourceAcq'Timescan0'Hdf5 (DSWrap_ (DSDataset Z Double) DSAcq)
    | DataSourceAcq'Timescan0'NoTimescan0
    deriving Generic

instance DataSource DSTimescan0 where
    withDataSourceP f (DataSourcePath'Timescan0'Hdf5 ps) g = withDataSourcesP f ps $ \p ds -> g (DataSourcePath'Timescan0'Hdf5 [p]) (DataSourceAcq'Timescan0'Hdf5 ds)
    withDataSourceP _ path@DataSourcePath'Timescan0'NoTimescan0 g = g path DataSourceAcq'Timescan0'NoTimescan0
