{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.Test
    ( newTest
    , processTest
    , updateTest
    ) where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.HashMap.Strict                (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Text                          (pack, unpack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           Foreign.Marshal.Utils              (fromBool)
import           GHC.Generics                       (Generic)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (each, runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeP, runSafeT)
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.Config
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.Chunk
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Repa
import           Hkl.Sample
import           Hkl.Types
import           Hkl.Utils

----------------
-- DataPath's --
----------------

data DataFrameTest
    = DataFrameTest
      DataFrameQCustom
      Sample

data DSDataFrameTest (k :: DSKind)
  = DSDataFrameTest
    { dataFrameTest'DataFrameQCustom :: DSWrap_ DSDataFrameQCustom k
    , dataFrameTest'Sample           :: DSWrap_ DSSample k
    }
  deriving (Generic)

instance ChunkP DSDataFrameTest where
instance DataSource DSDataFrameTest
instance FramesP DSDataFrameTest DataFrameTest
instance FromJSON (DSDataFrameTest DSPath)
instance ToJSON (DSDataFrameTest DSPath)
deriving instance Show (DSDataFrameTest DSPath)

instance Is1DStreamable (DSDataFrameTest DSAcq) DataFrameTest where
    extract1DStreamValue (DSDataFrameTest q s) i =
      DataFrameTest
      <$> extract1DStreamValue q i
      <*> extract0DStreamValue s

instance Is2DStreamable (DSDataFrameTest DSAcq) DataFrameTest where
    extract2DStreamValue (DSDataFrameTest q s) i =
      DataFrameTest
      <$> extract2DStreamValue q i
      <*> extract0DStreamValue s

instance HasFieldComment [DSDataFrameTest DSPath] where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue [DSDataFrameTest DSPath] where
  fieldvalue = autoJSON

------------
-- Config --
------------

instance HasIniConfig 'TestProjection where
  data Config 'TestProjection
    = BinocularsConfig'Test
      { binocularsConfig'Test'Common                 :: Config Common
      , binocularsConfig'Test'Sample                 :: Config Sample
      , binocularsConfig'Test'ProjectionType         :: ProjectionType
      , binocularsConfig'Test'ProjectionResolution   :: Resolutions DIM3
      , binocularsConfig'Test'ProjectionLimits       :: Maybe (RLimits DIM3)
      } deriving (Generic)

  newtype Args 'TestProjection = Args'TestProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'Test
        { binocularsConfig'Test'Common = defaultConfig
        , binocularsConfig'Test'Sample = defaultConfig
        , binocularsConfig'Test'ProjectionType = TestProjection
        , binocularsConfig'Test'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'Test'ProjectionLimits  = Nothing
        }

  getConfig content@(ConfigContent cfg) (Args'TestProjection mr) capabilities
      = do binocularsConfig'Test'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'Test'Sample <- getConfig content Args'Sample capabilities
           binocularsConfig'Test'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Test'ProjectionType defaultConfig)
           binocularsConfig'Test'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Test'ProjectionResolution defaultConfig)
           binocularsConfig'Test'ProjectionLimits <- parseMb cfg "projection" "limits"
           pure BinocularsConfig'Test{..}

  toIni c = toIni (binocularsConfig'Test'Common c)
            `mergeIni`
            toIni (binocularsConfig'Test'Sample c)
            `mergeIni`
            Ini { iniSections = fromList [ ("projection", elemFDef' "type" binocularsConfig'Test'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'Test'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'Test'ProjectionLimits c defaultConfig
                                           )
                                         ]
                , iniGlobals = []
                }


getDataPath :: Config 'TestProjection -> DSWrap_ DSDataFrameTest DSPath
getDataPath c
    = let sample = binocularsConfig'Test'Sample c
          common = binocularsConfig'Test'Common c
          content = binocularsConfig'Common'_Content common
      in guess'DataSourcePath'DataFrameTest common sample content

----------------
-- Projection --
----------------

{-# INLINE spaceTest #-}
spaceTest :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe (RLimits DIM3) -> PolarisationCorrection -> Maybe DynamicMask -> Space DIM3 -> DataFrameTest -> IO (DataFrameSpace DIM3)
spaceTest det pixels rs mlimits polarisation mdmask space@(Space fSpace) (DataFrameTest (DataFrameQCustom att g img mmask _ _ _) samplePath) =
    withNPixels det $ \nPixels ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withForeignPtr fSpace $ \pSpace ->
    withGeometry g $ \geometry ->
    withMaybeDynamicMask mdmask $ \c'dmask ->
    withMaybeMask mmask $ \ mask'' ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withPixelsDims pixels $ \ndim dims ->
    withResolutions rs $ \nr r ->
    withSample samplePath $ \sample -> do
      let c'polarisation = fromBool . unPolarisationCorrection $ polarisation
      case img of
        (ImageDouble arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "test_binoculars_space_test_double" #-} c'hkl_binoculars_space_test_double pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
        (ImageInt32 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "test_binoculars_space_test_int32_t" #-} c'hkl_binoculars_space_test_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
        (ImageWord16 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "test_binoculars_space_test_uint16_t" #-} c'hkl_binoculars_space_test_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
        (ImageWord32 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "test_binoculars_space_test_uint32_t" #-} c'hkl_binoculars_space_test_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
      return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processTestP :: (MonadIO m, MonadLogger m, MonadReader (Config 'TestProjection) m, MonadThrow m)
            => m ()
processTestP = do
  conf :: Config 'TestProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Test'Common conf

  let overwrite = binocularsConfig'Common'Overwrite common
  let det = binocularsConfig'Common'Detector common
  let (NCores cap) =  binocularsConfig'Common'NCores common
  let destination = binocularsConfig'Common'Destination common
  let centralPixel' = binocularsConfig'Common'Centralpixel common
  let (Meter sampleDetectorDistance) = binocularsConfig'Common'Sdd common
  let (Degree detrot) = binocularsConfig'Common'Detrot common
  let mImageSumMax = binocularsConfig'Common'ImageSumMax common
  let inputRange = binocularsConfig'Common'InputRange common
  let nexusDir = binocularsConfig'Common'Nexusdir common
  let tmpl = binocularsConfig'Common'Tmpl common
  let mSkipFirstPoints = binocularsConfig'Common'SkipFirstPoints common
  let mSkipLastPoints = binocularsConfig'Common'SkipLastPoints common
  let doPolarizationCorrection = binocularsConfig'Common'PolarizationCorrection common
  let mdmask = binocularsConfig'Common'DynamicMask common

  -- directly from the specific config
  let mlimits = binocularsConfig'Test'ProjectionLimits conf
  let res = binocularsConfig'Test'ProjectionResolution conf
  let projectionType = binocularsConfig'Test'ProjectionType conf


  -- built from the config
  let datapaths = getDataPath conf
  output' <- liftIO $ destination' projectionType Nothing inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir inputRange tmpl
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot NoNormalisation

  let fns = concatMap (replicate 1) (toList filenames)
  chunks' <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP mSkipFirstPoints mSkipLastPoints datapaths
  let chunks = Prelude.map fst chunks'
  let ntot = sum (Prelude.map csize chunks')
  let jobs = chunk (quot ntot cap) chunks

  -- log parameters

  logDebugNSH filenames
  logDebugNSH datapaths
  logDebugNSH chunks'
  logDebugN "start gessing final cube size"

  -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

  guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
    runSafeT $ runEffect $
    each chunks
    >-> Pipes.Prelude.map quadChunk
    >-> framesP datapaths
    >-> project det 3 (spaceTest det pixels res mlimits doPolarizationCorrection mdmask)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  logInfoN (pack $ printf "let's do a Test projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runEffect $ runSafeP $
                             each job
                             >-> Pipes.Prelude.map allChunk
                             -- >-> tee Pipes.Prelude.print
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameTest (DataFrameQCustom _ _ img _ _ _ _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceTest det pixels res mlimits doPolarizationCorrection mdmask)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameTest :: Config Common
                                   -> Config Sample
                                   -> ConfigContent
                                   -> DSWrap_ DSDataFrameTest DSPath
guess'DataSourcePath'DataFrameTest common sample content
  = [ DSDataFrameTest
      (guess'DataSource'DataFrameQCustom common Nothing content)
      (guess'DataSource'Sample common sample)
    ]

---------
-- Cmd --
---------

processTest :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processTest mf mr = cmd processTestP mf (Args'TestProjection mr)

newTest :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newTest cwd = do
  let conf = defaultConfig
             { binocularsConfig'Test'Common = defaultConfig
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateTest :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> Maybe ConfigRange -> m ()
updateTest mf mr = cmd (pure ()) mf (Args'TestProjection mr)
