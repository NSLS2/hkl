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

module Hkl.Binoculars.Projections.Hkl
    ( Config(..)
    , DataFrameHkl(..)
    , newHkl
    , processHkl
    , updateHkl
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

data DataFrameHkl
    = DataFrameHkl
      DataFrameQCustom
      Sample

data DSDataFrameHkl (k :: DSKind)
  = DSDataFrameHkl
    (DSWrap_ DSDataFrameQCustom k)
    (DSWrap_ DSSample k)
  deriving (Generic)


instance ChunkP DSDataFrameHkl
instance DataSource DSDataFrameHkl
instance FramesP DSDataFrameHkl DataFrameHkl
instance FromJSON (DSDataFrameHkl DSPath)
instance ToJSON (DSDataFrameHkl DSPath)
deriving instance Show (DSDataFrameHkl DSPath)

instance Is1DStreamable (DSDataFrameHkl DSAcq) DataFrameHkl where
    extract1DStreamValue (DSDataFrameHkl q s) i
        = DataFrameHkl
          <$> extract1DStreamValue q i
          <*> extract0DStreamValue s

instance Is2DStreamable (DSDataFrameHkl DSAcq) DataFrameHkl where
    extract2DStreamValue (DSDataFrameHkl q s) i
        = DataFrameHkl
          <$> extract2DStreamValue q i
          <*> extract0DStreamValue s

instance HasFieldComment [DSDataFrameHkl DSPath] where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue [DSDataFrameHkl DSPath] where
  fieldvalue = autoJSON

------------
-- Config --
------------

instance HasIniConfig 'HklProjection where
  data Config 'HklProjection
    = BinocularsConfig'Hkl
      { binocularsConfig'Hkl'Common                 :: Config Common
      , binocularsConfig'Hkl'Sample                 :: Config Sample
      , binocularsConfig'Hkl'ProjectionType         :: ProjectionType
      , binocularsConfig'Hkl'ProjectionResolution   :: Resolutions DIM3
      , binocularsConfig'Hkl'ProjectionLimits       :: Maybe (RLimits DIM3)
      } deriving (Generic)

  newtype Args 'HklProjection
      = Args'HklProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'Hkl
        { binocularsConfig'Hkl'Common = defaultConfig
        , binocularsConfig'Hkl'Sample = defaultConfig
        , binocularsConfig'Hkl'ProjectionType = HklProjection
        , binocularsConfig'Hkl'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'Hkl'ProjectionLimits  = Nothing
        }

  getConfig content@(ConfigContent cfg) (Args'HklProjection mr) capabilities
      = do binocularsConfig'Hkl'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'Hkl'Sample <- getConfig content Args'Sample capabilities
           binocularsConfig'Hkl'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Hkl'ProjectionType defaultConfig)
           binocularsConfig'Hkl'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Hkl'ProjectionResolution defaultConfig)
           binocularsConfig'Hkl'ProjectionLimits <- parseMb cfg "projection" "limits"
           pure BinocularsConfig'Hkl{..}

  toIni c = toIni (binocularsConfig'Hkl'Common c)
            `mergeIni`
            toIni (binocularsConfig'Hkl'Sample c)
            `mergeIni`
            Ini { iniSections = fromList [ ("projection", elemFDef' "type" binocularsConfig'Hkl'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'Hkl'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'Hkl'ProjectionLimits c defaultConfig
                                           )
                                         ]
                , iniGlobals = []
                }

getDataPath :: Config 'HklProjection -> DSWrap_ DSDataFrameHkl DSPath
getDataPath c
    = let sample = binocularsConfig'Hkl'Sample c
          common = binocularsConfig'Hkl'Common c
          content = binocularsConfig'Common'_Content common
      in guess'DataSourcePath'DataFrameHkl common sample content

----------------
-- Projection --
----------------

{-# INLINE spaceHkl #-}
spaceHkl :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe (RLimits DIM3) -> PolarisationCorrection -> Maybe DynamicMask -> Space DIM3 -> DataFrameHkl -> IO (DataFrameSpace DIM3)
spaceHkl det pixels rs mlimits polarisation mdmask space@(Space fSpace) (DataFrameHkl (DataFrameQCustom att g img mmask _ _ _) samplePath) =
    withNPixels det $ \nPixels ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withForeignPtr fSpace $ \pSpace ->
    withGeometry g $ \geometry ->
    withMaybeDynamicMask mdmask $ \c'dmask ->
    withMaybeMask mmask $ \ mask'' ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withResolutions rs $ \nr r ->
    withPixelsDims pixels $ \ndim dims ->
    withSample samplePath $ \sample -> do
      let c'polarisation = fromBool . unPolarisationCorrection $ polarisation
      case img of
        (ImageDouble arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "hkl_binoculars_space_hkl_double" #-} c'hkl_binoculars_space_hkl_double pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation  c'dmask
        (ImageInt32 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "hkl_binoculars_space_hkl_int32_t" #-} c'hkl_binoculars_space_hkl_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
        (ImageWord16 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "hkl_binoculars_space_hkl_uint16_t" #-} c'hkl_binoculars_space_hkl_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
        (ImageWord32 arr) -> unsafeWith arr $ \i -> do
                                        {-# SCC "hkl_binoculars_space_hkl_uint32_t" #-} c'hkl_binoculars_space_hkl_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) c'polarisation c'dmask
      return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processHklP :: (MonadIO m, MonadLogger m, MonadReader (Config 'HklProjection) m, MonadThrow m)
            => m ()
processHklP = do
  conf :: Config 'HklProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Hkl'Common conf

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
  let mlimits = binocularsConfig'Hkl'ProjectionLimits conf
  let res = binocularsConfig'Hkl'ProjectionResolution conf
  let projectionType = binocularsConfig'Hkl'ProjectionType conf


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
    >-> project det 3 (spaceHkl det pixels res mlimits doPolarizationCorrection mdmask)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  logInfoN (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runEffect $ runSafeP $
                             each job
                             >-> Pipes.Prelude.map allChunk
                             -- >-> tee Pipes.Prelude.print
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameHkl (DataFrameQCustom _ _ img _ _ _ _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceHkl det pixels res mlimits doPolarizationCorrection mdmask)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameHkl :: Config Common
                                  -> Config Sample
                                  -> ConfigContent
                                  -> DSWrap_ DSDataFrameHkl DSPath
guess'DataSourcePath'DataFrameHkl common sample content
  = [ DSDataFrameHkl
      (guess'DataSource'DataFrameQCustom common Nothing content)
      (guess'DataSource'Sample common sample)
    ]

---------
-- Cmd --
---------

processHkl :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processHkl mf mr = cmd processHklP mf (Args'HklProjection mr)

newHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newHkl cwd = do
  let conf = defaultConfig
             { binocularsConfig'Hkl'Common = defaultConfig
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> Maybe ConfigRange -> m ()
updateHkl mf mr = cmd (pure ()) mf (Args'HklProjection mr)
