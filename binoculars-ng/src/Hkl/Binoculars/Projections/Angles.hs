{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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

module Hkl.Binoculars.Projections.Angles
    ( newAngles
    , processAngles
    , updateAngles
    ) where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO), liftIO)
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.HashMap.Lazy                  (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Text                          (pack, unpack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (each, runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeT)
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.Config
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.Image
import           Hkl.Repa
import           Hkl.Types
import           Hkl.Utils

------------
-- Config --
------------

instance HasIniConfig 'AnglesProjection where
  data instance Config 'AnglesProjection
      = BinocularsConfig'Angles
        { binocularsConfig'Angles'Common                 :: Config Common
        , binocularsConfig'Angles'ProjectionType         :: ProjectionType
        , binocularsConfig'Angles'ProjectionResolution   :: Resolutions DIM3
        , binocularsConfig'Angles'ProjectionLimits       :: Maybe (RLimits DIM3)
        , binocularsConfig'Angles'SampleAxis             :: SampleAxis
        } deriving (Show)

  newtype Args 'AnglesProjection
      = Args'AnglesProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'Angles
        { binocularsConfig'Angles'Common = defaultConfig
        , binocularsConfig'Angles'ProjectionType = AnglesProjection
        , binocularsConfig'Angles'ProjectionResolution = Resolutions3 1 1 1
        , binocularsConfig'Angles'ProjectionLimits = Nothing
        , binocularsConfig'Angles'SampleAxis = SampleAxis "omega"
        }

  getConfig content@(ConfigContent cfg) (Args'AnglesProjection mr) capabilities
      = do binocularsConfig'Angles'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'Angles'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Angles'ProjectionType defaultConfig)
           binocularsConfig'Angles'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Angles'ProjectionResolution defaultConfig)
           binocularsConfig'Angles'ProjectionLimits <- parseMb cfg "projection" "limits"
           binocularsConfig'Angles'SampleAxis <- parseFDef cfg "input" "sample_axis" $ case binocularsConfig'Angles'ProjectionType of
                                                                                        AnglesProjection   -> SampleAxis "omega"
                                                                                        Angles2Projection  -> SampleAxis "mu"
                                                                                        HklProjection      -> undefined
                                                                                        QCustomProjection  -> undefined
                                                                                        QIndexProjection   -> undefined
                                                                                        QparQperProjection -> undefined
                                                                                        QxQyQzProjection   -> undefined
                                                                                        RealSpaceProjection -> undefined
                                                                                        PixelsProjection -> undefined
                                                                                        TestProjection  -> undefined
           pure BinocularsConfig'Angles{..}

  toIni c = toIni (binocularsConfig'Angles'Common c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input", elemFDef "sample_axis" binocularsConfig'Angles'SampleAxis c defaultConfig
                                                     [ "the name of the sample axis"
                                                     , ""
                                                     , "default value: `omega`"
                                                     ]
                                            )
                                         , ("projection", elemFDef' "type" binocularsConfig'Angles'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'Angles'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'Angles'ProjectionLimits c defaultConfig
                                           )]
                , iniGlobals = []
                }

getDataPath :: Config 'AnglesProjection -> DSWrap_ DSDataFrameQCustom DSPath
getDataPath c
    = let common = binocularsConfig'Angles'Common c
          content = binocularsConfig'Common'_Content common
      in guess'DataSource'DataFrameQCustom common Nothing content

-------------------------
-- Angles Projection --
-------------------------

{-# INLINE spaceAngles #-}
spaceAngles :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe (RLimits DIM3) -> SampleAxis -> Space DIM2 -> DataFrameQCustom -> IO (DataFrameSpace DIM2)
spaceAngles det pixels rs mlimits sAxis space@(Space fSpace) (DataFrameQCustom att g img mmask _ _ _) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withSampleAxis sAxis $ \sampleAxis ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageDouble arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_double" #-} c'hkl_binoculars_space_angles_double pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_int32_t" #-} c'hkl_binoculars_space_angles_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint16_t" #-} c'hkl_binoculars_space_angles_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint32_t" #-} c'hkl_binoculars_space_angles_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processAnglesP :: (MonadIO m, MonadLogger m, MonadReader (Config 'AnglesProjection) m, MonadThrow m)
               => m ()
processAnglesP = do
  conf :: Config 'AnglesProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Angles'Common conf

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

  -- directly from the specific config
  let mlimits = binocularsConfig'Angles'ProjectionLimits conf
  let res = binocularsConfig'Angles'ProjectionResolution conf
  let projectionType = binocularsConfig'Angles'ProjectionType conf
  let sampleAxis = binocularsConfig'Angles'SampleAxis conf

  -- built from the config
  let datapaths = getDataPath conf
  output' <- liftIO $ destination' projectionType Nothing inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir inputRange tmpl
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot Normalisation
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
    >-> project det 3 (spaceAngles det pixels res mlimits sampleAxis)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN (pack $ printf "let's do a Angles projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map allChunk
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _ _ _ _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceAngles det pixels res mlimits sampleAxis)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

---------
-- Cmd --
---------

processAngles :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processAngles mf mr = cmd processAnglesP mf (Args'AnglesProjection mr)

newAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Path Abs Dir -> m ()
newAngles cwd = do
  let conf = defaultConfig
             { binocularsConfig'Angles'Common = defaultConfig
                                                { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
             => Maybe FilePath -> Maybe ConfigRange -> m ()
updateAngles mf mr = cmd (pure ()) mf (Args'AnglesProjection mr)
