{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-splices #-}
{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QCustom
    ( Args(..)
    , Config(..)
    , DataFrameQCustom(..)
    , DataSource(..)
    , DSDataFrameQCustom(..)
    , FramesP(..)
    , guess'DataSource'DataFrameQCustom
    , newQCustom
    , processQCustom
    , updateQCustom
    ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logDebugN,
                                                    logInfoN)
import           Control.Monad.Reader              (MonadReader, ask)
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Map.Strict                   (Map, findWithDefault)
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Text                         (Text, pack, unpack)
import           Data.Text.IO                      (putStr)
import           Data.Vector.Storable.Mutable      (unsafeWith)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Utils             (fromBool)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (Angle, degree, radian, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (each, runEffect, (>->))
import           Pipes.Prelude                     (filter, map, print, tee,
                                                    toListM)
import           Pipes.Safe                        (runSafeT)
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.Config
import           Hkl.C.Binoculars
import           Hkl.Chunk
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Repa
import           Hkl.Types
import           Hkl.Utils

-----------------------
-- QCustom Projection --
-----------------------

-- Cursor

data DataFrameQCustom
    = DataFrameQCustom
      Attenuation -- attenuation
      Geometry -- geometry
      PixelCoordinates -- pixels 3D coordinates
      Image -- image
      (Maybe Mask) -- mask
      Flat -- flat
      Timestamp -- timestamp in double
      Timescan0 -- timescan0 in double
      Scannumber -- scannumber in int
    deriving Show

data DSDataFrameQCustom (k :: DSKind)
    = DataSource'DataFrameQCustom
      (DSWrap_ DSAttenuation k)
      (DSWrap_ DSGeometry k)
      (DSWrap_ DSPixelCoordinates k)
      (DSWrap_ DSImage k)
      (DSWrap_ DSMask k)
      (DSWrap_ DSFlat k)
      (DSWrap_ DSTimestamp k)
      (DSWrap_ DSTimescan0 k)
      (DSWrap_ DSScannumber k)
    deriving Generic

instance ChunkP DSDataFrameQCustom
instance DataSource DSDataFrameQCustom
instance FramesP DSDataFrameQCustom DataFrameQCustom
instance FromJSON (DSDataFrameQCustom DSPath)
deriving instance Show (DSDataFrameQCustom DSPath)
instance ToJSON (DSDataFrameQCustom DSPath)


instance Is1DStreamable (DSDataFrameQCustom DSAcq) DataFrameQCustom where
    extract1DStreamValue (DataSource'DataFrameQCustom att geom pixels img msk flat idx t0 s) i =
      DataFrameQCustom
      <$> extract1DStreamValue att i
      <*> extract1DStreamValue geom i
      <*> extract1DStreamValue pixels i
      <*> extract1DStreamValue img i
      <*> extract0DStreamValue msk
      <*> extract0DStreamValue flat
      <*> extract1DStreamValue idx i
      <*> extract0DStreamValue t0
      <*> extract0DStreamValue s

instance Is2DStreamable (DSDataFrameQCustom DSAcq) DataFrameQCustom where
    extract2DStreamValue (DataSource'DataFrameQCustom att geom pixels img msk flat idx t0 s) i =
      DataFrameQCustom
      <$> extract2DStreamValue att i
      <*> extract2DStreamValue geom i
      <*> extract2DStreamValue pixels i
      <*> extract2DStreamValue img i
      <*> extract0DStreamValue msk
      <*> extract0DStreamValue flat
      <*> extract2DStreamValue idx i
      <*> extract0DStreamValue t0
      <*> extract0DStreamValue s

instance HasFieldComment [DSDataFrameQCustom DSPath] where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue [DSDataFrameQCustom DSPath] where
  fieldvalue = autoJSON

------------
-- Config --
------------

instance HasIniConfig 'QCustomProjection where
  data Config 'QCustomProjection
      = BinocularsConfig'QCustom
        { binocularsConfig'QCustom'Common :: Config Common
        , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum     :: HklBinocularsSurfaceOrientationEnum
        , binocularsConfig'QCustom'ProjectionType         :: ProjectionType
        , binocularsConfig'QCustom'ProjectionResolution   :: Resolutions DIM3
        , binocularsConfig'QCustom'ProjectionLimits       :: Maybe (RLimits DIM3)
        , binocularsConfig'QCustom'SubProjection          :: Maybe HklBinocularsQCustomSubProjectionEnum
        , binocularsConfig'QCustom'Uqx                    :: Degree
        , binocularsConfig'QCustom'Uqy                    :: Degree
        , binocularsConfig'QCustom'Uqz                    :: Degree
        , binocularsConfig'QCustom'SampleAxis             :: Maybe SampleAxis
        } deriving (Show, Generic)

  newtype Args 'QCustomProjection
      = Args'QCustomProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'QCustom
        { binocularsConfig'QCustom'Common = defaultConfig
        , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum = HklBinocularsSurfaceOrientationEnum'Vertical
        , binocularsConfig'QCustom'ProjectionType = QCustomProjection
        , binocularsConfig'QCustom'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'QCustom'ProjectionLimits  = Nothing
        , binocularsConfig'QCustom'SubProjection = Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
        , binocularsConfig'QCustom'Uqx = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'Uqy = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'Uqz = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'SampleAxis = Nothing
        }

  getConfig content@(ConfigContent cfg) (Args'QCustomProjection mr) capabilities
      = do binocularsConfig'QCustom'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum <-  parseFDef cfg "input" "surface_orientation" (binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum defaultConfig)
           binocularsConfig'QCustom'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'QCustom'ProjectionType defaultConfig)
           let msubprojection' = parseMbDef cfg "projection" "subprojection" (binocularsConfig'QCustom'SubProjection defaultConfig)
           let binocularsConfig'QCustom'SubProjection
                   = case binocularsConfig'QCustom'ProjectionType of
                       QIndexProjection -> Just HklBinocularsQCustomSubProjectionEnum'QTimestamp
                       QparQperProjection -> Just HklBinocularsQCustomSubProjectionEnum'QparQper
                       QxQyQzProjection -> Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
                       AnglesProjection -> msubprojection'
                       Angles2Projection -> msubprojection'
                       HklProjection -> msubprojection'
                       QCustomProjection -> msubprojection'
                       RealSpaceProjection -> Just HklBinocularsQCustomSubProjectionEnum'XYZ
                       PixelsProjection -> Just HklBinocularsQCustomSubProjectionEnum'YZTimestamp
                       TestProjection -> msubprojection'

           binocularsConfig'QCustom'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'QCustom'ProjectionResolution defaultConfig)
           binocularsConfig'QCustom'ProjectionLimits <- parseMb cfg "projection" "limits"
           binocularsConfig'QCustom'Uqx <- parseFDef cfg "projection" "uqx" (binocularsConfig'QCustom'Uqx defaultConfig)
           binocularsConfig'QCustom'Uqy <- parseFDef cfg "projection" "uqy" (binocularsConfig'QCustom'Uqy defaultConfig)
           binocularsConfig'QCustom'Uqz <- parseFDef cfg "projection" "uqz" (binocularsConfig'QCustom'Uqz defaultConfig)

           let errorMissingSampleAxis
                   = case binocularsConfig'QCustom'SubProjection of
                       Nothing -> Nothing
                       Just sub -> case sub of
                                    HklBinocularsQCustomSubProjectionEnum'QxQyQz -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQper -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQx -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQy -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQz -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QStereo -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> error "expect a valid  [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'XYZ -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QQparQper -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QScannumber -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'TthScannumber -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> Nothing
           let binocularsConfig'QCustom'SampleAxis = eitherF (const errorMissingSampleAxis) (parse' cfg "projection" "sampleaxis")
                                                     (\case
                                                       Nothing -> errorMissingSampleAxis
                                                       Just d  -> Just d
                                                     )
           pure BinocularsConfig'QCustom{..}

  configToIni c
      = configToIni (binocularsConfig'QCustom'Common c)
        `mergeIni`
         Ini { iniSections = fromList [ ("input",
                                         elemFDef' "surface_orientation" binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum c defaultConfig
                                        )
                                      , ("projection",
                                         elemFDef' "type" binocularsConfig'QCustom'ProjectionType c defaultConfig
                                        <> elemFDef' "resolution" binocularsConfig'QCustom'ProjectionResolution c defaultConfig
                                        <> elemFMbDef' "limits" binocularsConfig'QCustom'ProjectionLimits c defaultConfig
                                        <> elemFMbDef' "subprojection" binocularsConfig'QCustom'SubProjection c defaultConfig
                                        <> elemFDef "uqx" binocularsConfig'QCustom'Uqx c defaultConfig
                                         [ "rotation around the x-axis of the sample in the surface basis system -- degree"
                                         , ""
                                         , "in this basis, the x-axis is colinear to the surface of the sample along the x-rays."
                                         , ""
                                         , " `<not set>` - use the default value `0.0`"
                                         , " `a value`   - use this value"
                                         ]
                                        <> elemFDef "uqy" binocularsConfig'QCustom'Uqy c defaultConfig
                                         [ "rotation around the y-axis of the sample in the surface basis system -- degree"
                                         , ""
                                         , "in this basis, the y-axis is colinear to the surface of the sample and"
                                         , "forme a directe basis with x-axis and z-axis."
                                         , ""
                                         , "examples:"
                                         , "  - all motors set to zero and a vertical surface - y-axis along -z (labo basis)"
                                         , "  - all motors set to zero and an horizontal surcafe - y-axis along y (labo basis)"
                                         , ""
                                         , " `<not set>` - use the default value `0.0`"
                                         , " `a value`   - use this value"
                                         ]
                                        <> elemFDef "uqz" binocularsConfig'QCustom'Uqz c defaultConfig
                                         [ "rotation around the z-axis of the sample in the surface basis system -- degree"
                                         , ""
                                         , "in this basis, the z-axis is perpendicular to the surface of the sample."
                                         , ""
                                         , "examples:"
                                         , "  - all motors set to zero and a vertical surface - z-axis along y (labo basis)"
                                         , "  - all motors set to zero and an horizontal surcafe - z-axis along z (labo basis)"
                                         , ""
                                         , ""
                                         , " `<not set>` - use the default value `0.0`"
                                         , " `a value`   - use this value"
                                         ]
                                        <> elemFMbDef "sampleaxis" binocularsConfig'QCustom'SampleAxis c defaultConfig
                                         [ "the name of the sample axis expected by some subprojections."
                                         , ""
                                         , " `<not set>` - for all subprojections which does not expect a value."
                                         , " `a value`   - use this value for the subprojection expecting this axis."
                                         ]
                                        )
                                      ]
             , iniGlobals = []
             }

------------------
-- Input Path's --
------------------

-- Attenuation Path

mk'Attenuation'Path :: InputType -> Maybe Double -> Maybe Float -> Maybe Int  -> DSWrap_ DSAttenuation DSPath
mk'Attenuation'Path inputtype mAttenuationCoefficient mAttenuationMax mAttenuationShift = Prelude.map f attenuation
    where
      f att = case mAttenuationCoefficient of
                Nothing -> case att of
                            DataSourcePath'NoAttenuation -> DataSourcePath'NoAttenuation
                            DataSourcePath'Attenuation{} -> DataSourcePath'NoAttenuation
                            -- logWarnN "The current configuration extract the attenuation from the data files."
                            -- logWarnN "You forgot to provide the attenuation coefficient in the config file."
                            -- logWarnN "I continue without attenuation correction"
                            -- logWarnN "Add attenuation_coefficient=<something> under the [input] section, to fix this"
                            -- return DataSourcePath'NoAttenuation
                            applyed@DataSourcePath'ApplyedAttenuationFactor{} -> applyed
                Just coef -> case att of
                              DataSourcePath'NoAttenuation         -> DataSourcePath'NoAttenuation
                              DataSourcePath'Attenuation p o _ m -> DataSourcePath'Attenuation p o coef m
                              DataSourcePath'ApplyedAttenuationFactor _ -> undefined

      no = [ DataSourcePath'NoAttenuation ]

      sixs'Fly  = [ DataSourcePath'Attenuation
                    [ DataSourcePath'Float'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                                , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation_old")
                                                ]
                    ]
                    (fromMaybe 2 mAttenuationShift) 0 mAttenuationMax
                  ]
      sixs'Sbs = [ DataSourcePath'Attenuation
                   [ DataSourcePath'Float'Hdf5 [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                               , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att"))
                                               , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att_old"))
                                               , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att"))
                                               , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att_old"))
                                               ]
                   ]
                   (fromMaybe 0 mAttenuationShift) 0 mAttenuationMax
                 ]

      attenuation = case inputtype of
                      CristalK6C        -> no
                      Custom            -> no
                      DiffabsCirpad     -> no
                      MarsCx3Sbs        -> no
                      MarsFlyscan       -> no
                      MarsSbs           -> no
                      SixsFlyMedH       -> sixs'Fly
                      SixsFlyMedHGisaxs -> sixs'Fly
                      SixsFlyMedV       -> sixs'Fly
                      SixsFlyMedVGisaxs -> sixs'Fly
                      SixsFlyUhv        -> sixs'Fly
                      SixsFlyUhvGisaxs  -> sixs'Fly
                      SixsSbsMedH       -> sixs'Sbs
                      SixsSbsMedHGisaxs -> sixs'Sbs
                      SixsSbsMedV       -> sixs'Sbs
                      SixsSbsMedVGisaxs -> sixs'Sbs
                      SixsSbsUhv        -> sixs'Sbs
                      SixsSbsUhvGisaxs  -> sixs'Sbs


-- Flat Path

mk'Flat'Path :: Config Common -> DSWrap_ DSFlat DSPath
mk'Flat'Path c
    = [ let det = binocularsConfig'Common'Detector c
        in case binocularsConfig'Common'Flatmatrix c of
             Nothing -> DataSourcePath'Flat (FlatLocation "default") det
             Just m  -> DataSourcePath'Flat m det
      ]

-- Geometry Path

mk'Wavelength'Path :: InputType -> Maybe Double -> DSWrap_ DSDouble DSPath
mk'Wavelength'Path inputtype mw = Prelude.map f wavelength
    where
      f w = maybe w DataSourcePath'Double'Const mw

      wavelength = let cristal = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Monochromator/lambda") ] ]
                       diffabs = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "DIFFABS/d13-1-c03__op__mono/wavelength") ] ]
                       mars = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-c03__op__mono1-config_#2/lambda" ) ]
                              -- TODO /scan_0030/MARS/d03-1-c03__op__mono1_#2/energy
                              , DataSourcePath'Double'Const 0.7293188143129427
                              ]
                       sixs = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength")
                                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-c02-op-mono/lambda")
                                                           ]
                              ]
                   in case inputtype of
                        CristalK6C        -> cristal
                        Custom            -> undefined
                        DiffabsCirpad     -> diffabs
                        MarsCx3Sbs        -> mars
                        MarsFlyscan       -> mars
                        MarsSbs           -> mars
                        SixsFlyMedH       -> sixs
                        SixsFlyMedHGisaxs -> sixs
                        SixsFlyMedV       -> sixs
                        SixsFlyMedVGisaxs -> sixs
                        SixsFlyUhv        -> sixs
                        SixsFlyUhvGisaxs  -> sixs
                        SixsSbsMedH       -> sixs
                        SixsSbsMedHGisaxs -> sixs
                        SixsSbsMedV       -> sixs
                        SixsSbsMedVGisaxs -> sixs
                        SixsSbsUhv        -> sixs
                        SixsSbsUhvGisaxs  -> sixs

mk'Axes'Path :: InputType -> Map Text Int -> ConfigContent -> DSWrap_ DSDoubles DSPath
mk'Axes'Path inputtype shifts cfg = axes
    where
      mk'Axis'Path :: Map Text Int -> Text -> DSWrap_ DSDouble DSPath -> DSWrap_ DSDouble DSPath
      mk'Axis'Path shifts' name ps = let s = findWithDefault 0 name shifts'
                                     in if s /= 0
                                        then
                                            Prelude.map (\p -> case p of
                                                                DataSourcePath'Double'Ini _ _ _ -> p
                                                                DataSourcePath'Double'Hdf5 ds -> DataSourcePath'Double'Hdf5WithShift ds s
                                                                DataSourcePath'Double'Hdf5WithShift ds _ -> DataSourcePath'Double'Hdf5WithShift ds s
                                                                DataSourcePath'Double'Const _ -> p
                                                        ) ps
                                        else
                                            Prelude.map (\p -> case p of
                                                                DataSourcePath'Double'Ini _ _ _ -> p
                                                                DataSourcePath'Double'Hdf5 _ -> p
                                                                DataSourcePath'Double'Hdf5WithShift ds _ -> DataSourcePath'Double'Hdf5 ds
                                                                DataSourcePath'Double'Const _ -> p
                                                        ) ps
      -- geometry
      sixs'eix
          = let name = "eix"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/eix")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/dt/tab-mt_tx.1/position"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/dt/tab-mt_tx.1/position"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tx.1/position_pre")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'eiz
          = let name = "eiz"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/eiz")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/dt/tab-mt_tz.1/position"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/dt/tab-mt_tz.1/position"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tz.1/position_pre")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'Uhv'Mu
          = let name = "mu"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/mu"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu_xps")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'Uhv'Omega
          = let name = "omega"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/omega"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega_xps")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'Uhv'Delta
          = let name = "delta"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5  [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                                   , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                                                   , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/delta"))
                                                   , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta_xps")
                                                   ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'Uhv'Gamma
          = let name = "gamma"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/gamma"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma_xps")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'Med'Beta
          = let name = "beta"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-ex-diff-med-tpp/TPP/Orientation/pitch")
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedH'Mu
          = let name = "mu"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedV'Mu
          = let name = "mu"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/mu"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedV'Omega
          = let name = "omega"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/omega"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedH'Gamma
          = let name = "gamma"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedV'Gamma
          = let name = "gamma"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/gamma"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedH'Delta
          = let name = "delta"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedV'Delta
          = let name = "delta"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/delta"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps
      sixs'MedV'Etaa
          = let name = "etaa"
                ps = [ DataSourcePath'Double'Ini cfg "geometry.values" name
                     , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa")
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))
                                                  , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/eta-a-med-grp/etaa"))
                                                  ]
                     ]
            in mk'Axis'Path shifts name ps

      sixs'medh = [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedH'Mu, sixs'MedH'Gamma, sixs'MedH'Delta ] ]
      sixs'medh'gisaxs = [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedH'Mu, sixs'eix, sixs'eiz ] ]
      sixs'medv = [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedV'Mu, sixs'MedV'Omega, sixs'MedV'Gamma, sixs'MedV'Delta, sixs'MedV'Etaa ] ]
      sixs'medv'gisaxs = [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedV'Mu, sixs'MedV'Omega, sixs'eix, sixs'eiz ] ]
      sixs'uhv = [ DataSourcePath'List [sixs'Uhv'Mu, sixs'Uhv'Omega, sixs'Uhv'Delta, sixs'Uhv'Gamma] ]
      sixs'uhv'gisaxs = [ DataSourcePath'List [ sixs'Uhv'Mu, sixs'Uhv'Omega, sixs'eix, sixs'eiz ] ]

      axes = case inputtype of
               CristalK6C -> [ DataSourcePath'List
                              [ mk'Axis'Path shifts "mu" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-mu/position") ] ]
                              , mk'Axis'Path shifts "komega" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-komega/position") ] ]
                              , mk'Axis'Path shifts "kappa" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-kappa/position") ] ]
                              , mk'Axis'Path shifts "kphi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/actuator_1_1") ] ]
                              , mk'Axis'Path shifts "gamma" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-gamma/position") ] ]
                              , mk'Axis'Path shifts "delta" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-delta/position") ] ]
                              ]
                            ]
               Custom -> undefined
               DiffabsCirpad -> [ DataSourcePath'List
                                 [ mk'Axis'Path shifts "delta" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/ex/cirpad_delta/position")) ] ]
                                 , mk'Axis'Path shifts "gamma" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/ex/dif.1-cirpad-gam/position")) ] ]
                                 ]
                               ]
               MarsCx3Sbs -> [ DataSourcePath'List
                              [ -- mk'Axis'Path shifts "phi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d03-1-cx0/ex/spt_mf-mt_rz/position")) ]
                              --                             , DataSourcePath'Double'Const 0
                              --                             ]
                              -- , mk'Axis'Path shifts "omega" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d03-1-cx0/ex/spt_mf-mt_rx/position")) ]
                              --                               , DataSourcePath'Double'Const 0
                              --                               ]
                              -- , mk'Axis'Path shifts "ty" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d03-1-cx3/dt/spt_dtc_mt_tx/position")) ]
                              --                            , DataSourcePath'Double'Const 0
                              --                            ]
                              -- , mk'Axis'Path shifts "tth" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d03-1-cx3/dt/spt_dtc_mt_rx/position")) ]
                              --                             , DataSourcePath'Double'Const 0
                              --                             ]
                              -- , mk'Axis'Path shifts "tx" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d03-1-cx3/dt/spt_dtc_mt_ts/position")) ]
                              --                            , DataSourcePath'Double'Const 0
                              --                            ]
                              mk'Axis'Path shifts "phi" [ DataSourcePath'Double'Const 0
                                                          ]
                              , mk'Axis'Path shifts "omega" [ DataSourcePath'Double'Const 0
                                                            ]
                              , mk'Axis'Path shifts "ty" [ DataSourcePath'Double'Const 0
                                                         ]
                              , mk'Axis'Path shifts "tth" [ DataSourcePath'Double'Const 0
                                                          ]
                              , mk'Axis'Path shifts "tx" [ DataSourcePath'Double'Const 0
                                                         ]
                              ]
                            ]
               MarsFlyscan -> [ DataSourcePath'List
                               [ mk'Axis'Path shifts "omega" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/omega") ] ]
                               , mk'Axis'Path shifts "chi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/chi") ] ]
                               , mk'Axis'Path shifts "phi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/phi") ] ]
                               , mk'Axis'Path shifts "tth" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/tth") ] ]
                               ]
                             ]
               MarsSbs -> [ DataSourcePath'List
                           [ mk'Axis'Path shifts "omega" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                                                                                      , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__dif-mt_rx.1_#2/raw_value")
                                                                                      ]
                                                         , DataSourcePath'Double'Const 0
                                                         ]
                           , mk'Axis'Path shifts "chi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                                                                                    , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__gonio-mt_rs_#2/raw_value")
                                                                                    ]
                                                       , DataSourcePath'Double'Const 0
                                                       ]
                           , mk'Axis'Path shifts "phi" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                                                                                    , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetpattr ("long_name", "d03-1-cx2/ex/gonio-mt_rz/position"))
                                                                                    , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__gonio-mt_rz_#2/raw_value")
                                                                                    ]
                                                       , DataSourcePath'Double'Const 0
                                                       ]
                           , mk'Axis'Path shifts "tth" [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                                                                                    , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__dif-mt_rx.2_#2/raw_value")
                                                                                    ]
                                                       , DataSourcePath'Double'Const 0
                                                       ]
                           ]
                         ]
               SixsFlyMedH -> sixs'medh
               SixsFlyMedHGisaxs -> sixs'medh'gisaxs
               SixsFlyMedV -> sixs'medv
               SixsFlyMedVGisaxs -> sixs'medv'gisaxs
               SixsFlyUhv -> sixs'uhv
               SixsFlyUhvGisaxs -> sixs'uhv'gisaxs
               SixsSbsMedH -> sixs'medh
               SixsSbsMedHGisaxs -> sixs'medh'gisaxs
               SixsSbsMedV -> sixs'medv
               SixsSbsMedVGisaxs -> sixs'medv'gisaxs
               SixsSbsUhv -> sixs'uhv
               SixsSbsUhvGisaxs -> sixs'uhv'gisaxs

mk'Geometry'Path :: InputType -> Maybe Double -> Geometry -> Map Text Int -> ConfigContent -> DSWrap_ DSGeometry DSPath
mk'Geometry'Path inputtype mWavelength geom shifts cfg = [ DataSourcePath'Geometry geom wavelength axes ]
    where
      wavelength = mk'Wavelength'Path inputtype mWavelength
      axes = mk'Axes'Path inputtype shifts cfg

-- Image Path

mk'Image'Path :: InputType -> Maybe (DSWrap_ DSImage DSPath) -> Detector Hkl DIM2 -> Scannumber -> DSWrap_ DSImage DSPath
mk'Image'Path inputtype mImage detector@(Detector2D d _ _) sn = fromMaybe image mImage
    where
      sixs'Fly = [case d of
                    HklBinocularsDetectorEnum'ImxpadS140 ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ grouppat 0 (datasetp "scan_data/xpad_image"))
                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 (datasetp "scan_data/xpad_s140_image"))
                        ]
                    HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
                    HklBinocularsDetectorEnum'ImxpadS70 ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_s70_image") ]
                    HklBinocularsDetectorEnum'DectrisEiger1M ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/eiger_image") ]
                    HklBinocularsDetectorEnum'Ufxc ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/ufxc_sixs_image") ]
                    HklBinocularsDetectorEnum'Merlin -> undefined
                    HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined
                    HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512 ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/merlin_image") ]
                    HklBinocularsDetectorEnum'Cirpad -> undefined
                    HklBinocularsDetectorEnum'RigakuXspa1M ->
                        DataSourcePath'Image'Img detector "/nfs/ruche/sixs-soleil/com-sixs/2025/Run1/Rigaku_99240224/Scan%d/Beam18keV4_scan%d_%06d.img" sn
                    HklBinocularsDetectorEnum'Dectris'PilatusCdTe2M -> undefined
                 ]

      sixs'Sbs = [case d of
                    HklBinocularsDetectorEnum'ImxpadS140 ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p (datasetpattr ("long_name", "i14-c-c00/dt/xpad.s140/image")))
                        , DataSourcePath'Dataset (hdf5p (datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
                        ]
                    HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
                    HklBinocularsDetectorEnum'ImxpadS70 ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s70/image")) ]
                    HklBinocularsDetectorEnum'DectrisEiger1M ->
                        DataSourcePath'Image'Hdf5
                        detector
                        [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image")) ]
                    HklBinocularsDetectorEnum'Ufxc -> undefined
                    HklBinocularsDetectorEnum'Merlin -> undefined
                    HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined
                    HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512 -> undefined
                    HklBinocularsDetectorEnum'Cirpad -> undefined
                    HklBinocularsDetectorEnum'RigakuXspa1M ->
                        DataSourcePath'Image'Img detector "/nfs/ruche/sixs-soleil/com-sixs/2025/Run1/Rigaku_99240224/Scan%d/Beam18keV4_scan%d_%06d.img" sn
                    HklBinocularsDetectorEnum'Dectris'PilatusCdTe2M -> undefined
                 ]

      image = case inputtype of
                CristalK6C -> [ DataSourcePath'Image'Hdf5
                               detector
                               [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/data_05") ]
                             ]
                Custom -> undefined
                DiffabsCirpad -> [ DataSourcePath'Image'Hdf5
                                  detector
                                  [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/dt/cirpad.1/image")) ]
                                ]
                MarsCx3Sbs -> [ DataSourcePath'Image'Hdf5
                               detector
                               [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/pilatus.1/image"))
                               , DataSourcePath'Dataset (hdf5p $ datasetpattr ("interpretation", "image"))
                               ]
                             ]
                MarsFlyscan -> [ DataSourcePath'Image'Hdf5
                                detector
                                [ DataSourcePath'Dataset (hdf5p $ grouppat 0 (datasetp "scan_data/merlin_image"))
                                , DataSourcePath'Dataset (hdf5p $ grouppat 0 (datasetp "scan_data/merlin_quad_image"))
                                ]
                              ]
                MarsSbs -> [ DataSourcePath'Image'Hdf5
                            detector
                            [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image"))
                            , DataSourcePath'Dataset (hdf5p $ datasetpattr ("interpretation", "image"))
                            ]
                          ]
                SixsFlyMedH -> sixs'Fly
                SixsFlyMedHGisaxs -> sixs'Fly
                SixsFlyMedV -> sixs'Fly
                SixsFlyMedVGisaxs -> sixs'Fly
                SixsFlyUhv -> sixs'Fly
                SixsFlyUhvGisaxs -> sixs'Fly
                SixsSbsMedH -> sixs'Sbs
                SixsSbsMedHGisaxs -> sixs'Sbs
                SixsSbsMedV -> sixs'Sbs
                SixsSbsMedVGisaxs -> sixs'Sbs
                SixsSbsUhv -> sixs'Sbs
                SixsSbsUhvGisaxs -> sixs'Sbs

-- Mask Path

mk'Mask'Path :: Config Common -> DSWrap_ DSMask DSPath
mk'Mask'Path c
    = [ case binocularsConfig'Common'Maskmatrix c of
          Nothing -> DataSourcePath'Mask'NoMask
          Just m  -> DataSourcePath'Mask
                    m
                    (binocularsConfig'Common'Detector c)
      ]

-- PixelCoordinates Path

mk'PixelCoordinates'Path :: Config Common -> DSWrap_ DSPixelCoordinates DSPath
mk'PixelCoordinates'Path c
    = let detector = binocularsConfig'Common'Detector c
          pixel0 = binocularsConfig'Common'CentralPixel c
          sdd = binocularsConfig'Common'Sdd c
          detrot = binocularsConfig'Common'Detrot c
      in [ DataSourcePath'PixelCoordinates'Ponis detector "/nfs/ruche/mars-soleil/com-mars/2026/2026_Run2/20252223/published-data/scan_0030_0001.nxs_%02d.poni"  NoNormalisation ]
      -- in [ DataSourcePath'PixelCoordinates detector pixel0 sdd detrot NoNormalisation ]

-- Timestamp Path

mk'Timestamp'Path :: InputType -> Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimestamp DSPath
mk'Timestamp'Path inputtype msub = Prelude.map f timestamp
    where
      f idx = case msub of
                Nothing -> DataSourcePath'Timestamp'NoTimestamp
                Just sub -> case sub of
                             HklBinocularsQCustomSubProjectionEnum'QxQyQz -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QparQper -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QPhiQx -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QPhiQy -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QPhiQz -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QStereo -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'XYZ -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QQparQper -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> idx
                             HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'QScannumber -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'TthScannumber -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> DataSourcePath'Timestamp'NoTimestamp
                             HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> DataSourcePath'Timestamp'NoTimestamp

      timestamp =
          let sbs = [ DataSourcePath'Timestamp'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps") ] ]
              fly = [ DataSourcePath'Timestamp'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]
          in case inputtype of
               CristalK6C        -> sbs
               Custom            -> undefined
               DiffabsCirpad     -> sbs
               MarsCx3Sbs        -> sbs
               MarsFlyscan       -> fly
               MarsSbs           -> sbs
               SixsFlyMedH       -> fly
               SixsFlyMedHGisaxs -> fly
               SixsFlyMedV       -> fly
               SixsFlyMedVGisaxs -> fly
               SixsFlyUhv        -> fly
               SixsFlyUhvGisaxs  -> fly
               SixsSbsMedH       -> sbs
               SixsSbsMedHGisaxs -> sbs
               SixsSbsMedV       -> sbs
               SixsSbsMedVGisaxs -> sbs
               SixsSbsUhv        -> sbs
               SixsSbsUhvGisaxs  -> sbs


-- Timescan0 Path

mk'Timescan0'Path :: InputType -> Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimescan0 DSPath
mk'Timescan0'Path inputtype msub =
    let timescan0'Sbs = [ DataSourcePath'Timescan0'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps") ] ]
        timescan0'Fly = [ DataSourcePath'Timescan0'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]
        timescan0 = case inputtype of
                      CristalK6C        -> timescan0'Sbs
                      Custom            -> undefined
                      DiffabsCirpad     -> timescan0'Sbs
                      MarsCx3Sbs        -> timescan0'Sbs
                      MarsFlyscan       -> timescan0'Sbs
                      MarsSbs           -> timescan0'Sbs
                      SixsFlyMedH       -> timescan0'Fly
                      SixsFlyMedHGisaxs -> timescan0'Fly
                      SixsFlyMedV       -> timescan0'Fly
                      SixsFlyMedVGisaxs -> timescan0'Fly
                      SixsFlyUhv        -> timescan0'Fly
                      SixsFlyUhvGisaxs  -> timescan0'Fly
                      SixsSbsMedH       -> timescan0'Sbs
                      SixsSbsMedHGisaxs -> timescan0'Sbs
                      SixsSbsMedV       -> timescan0'Sbs
                      SixsSbsMedVGisaxs -> timescan0'Sbs
                      SixsSbsUhv        -> timescan0'Sbs
                      SixsSbsUhvGisaxs  -> timescan0'Sbs
        f idx = case msub of
                  Nothing -> DataSourcePath'Timescan0'NoTimescan0
                  Just sub -> case sub of
                               HklBinocularsQCustomSubProjectionEnum'QxQyQz -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQper -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQx -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQy -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQz -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QStereo -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'XYZ -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QQparQper -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> idx
                               HklBinocularsQCustomSubProjectionEnum'QScannumber -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'TthScannumber -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> DataSourcePath'Timescan0'NoTimescan0
    in Prelude.map f timescan0


guess'DataSource'DataFrameQCustom :: Config Common
                                  -> Maybe HklBinocularsQCustomSubProjectionEnum
                                  -> ConfigContent
                                  -> DSWrap_ DSDataFrameQCustom DSPath
guess'DataSource'DataFrameQCustom common msub cfg =
    let inputtype = binocularsConfig'Common'InputType common
        mAttenuationCoefficient = binocularsConfig'Common'AttenuationCoefficient common
        detector =  binocularsConfig'Common'Detector common
        mImage = binocularsConfig'Common'Image common
        mAttenuationMax = binocularsConfig'Common'AttenuationMax common
        mAttenuationShift = binocularsConfig'Common'AttenuationShift common
        geometry = binocularsConfig'Common'Geometry common
        shifts = binocularsConfig'Common'GeometryShifts common
        mWavelength = binocularsConfig'Common'Wavelength common
        sn0 = getInitialScannumber $ binocularsConfig'Common'InputRange common
    in [ DataSource'DataFrameQCustom
         ( mk'Attenuation'Path inputtype mAttenuationCoefficient mAttenuationMax mAttenuationShift )
         ( mk'Geometry'Path inputtype mWavelength geometry shifts cfg )
         ( mk'PixelCoordinates'Path common )
         ( mk'Image'Path inputtype mImage detector sn0 )
         ( mk'Mask'Path common )
         ( mk'Flat'Path common )
         ( mk'Timestamp'Path inputtype msub )
         ( mk'Timescan0'Path inputtype msub )
         [ DataSourcePath'Scannumber ]
       ]

getDataPath :: Config 'QCustomProjection -> DSWrap_ DSDataFrameQCustom DSPath
getDataPath c
    = let sub = binocularsConfig'QCustom'SubProjection c
          common = binocularsConfig'QCustom'Common c
          content = binocularsConfig'Common'_Content common
      in guess'DataSource'DataFrameQCustom common sub content


{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2
             -> Resolutions DIM3
             -> HklBinocularsSurfaceOrientationEnum
             -> Maybe (RLimits DIM3)
             -> HklBinocularsQCustomSubProjectionEnum
             -> Angle Double -> Angle Double -> Angle Double
             -> Maybe SampleAxis
             -> PolarisationCorrection
             -> Maybe DynamicMask
             -> Space DIM3
             -> DataFrameQCustom
             -> IO (DataFrameSpace DIM3)
spaceQCustom det rs surf mlimits subprojection uqx uqy uqz mSampleAxis polarisation mdmask space@(Space fSpace) (DataFrameQCustom att g pixels img mmask flat index timescan0 scannumber) =
  withGeometry g $ \c'geometry ->
  withForeignPtr (toForeignPtr pixels) $ \c'pix ->
  withForeignPtr fSpace $ \c'pSpace ->
  withMaybeDynamicMask mdmask $ \ c'dmask ->
  withMaybeMask mmask $ \ c'mask ->
  withForeignPtr (toForeignPtr flat) $ \ c'flat ->
  withMaybeLimits mlimits rs $ \nlimits c'limits ->
  withMaybeSampleAxis mSampleAxis $ \c'sampleAxis ->
  withNPixels det $ \c'nPixels ->
  withPixelsDims pixels $ \ndim c'dims ->
  withResolutions rs $ \nr c'r -> do
    let c'polarisation = fromBool . unPolarisationCorrection $ polarisation
    let c'attenuation = CDouble . unAttenuation $ att
    let c'ndim = toEnum ndim
    let c'nr = toEnum nr
    let c'surf = toEnum . fromEnum $ surf
    let c'nlimits = toEnum nlimits
    let c'timestamp = CDouble . unTimestamp $ index
    let c'timescan0 = CDouble . unTimescan0 $ timescan0
    let c'scannumber = toEnum . fromEnum . unScannumber $ scannumber
    let c'subprojection = toEnum . fromEnum $ subprojection
    let c'uqx = CDouble (uqx /~ radian)
    let c'uqy = CDouble (uqy /~ radian)
    let c'uqz = CDouble (uqz /~ radian)
    case img of
      (ImageDouble arr) -> unsafeWith arr $ \i -> do
                                 {-# SCC "hkl_binoculars_space_qcustom_double" #-} c'hkl_binoculars_space_qcustom_double c'pSpace c'geometry i c'nPixels c'attenuation c'flat c'pix c'ndim c'dims c'r c'nr c'mask c'surf c'limits c'nlimits c'timestamp c'timescan0 c'scannumber c'subprojection c'uqx c'uqy c'uqz c'sampleAxis c'polarisation c'dmask
      (ImageInt32 arr) -> unsafeWith arr $ \i -> do
                                 {-# SCC "hkl_binoculars_space_qcustom_int32_t" #-} c'hkl_binoculars_space_qcustom_int32_t c'pSpace c'geometry i c'nPixels c'attenuation c'flat c'pix c'ndim c'dims c'r c'nr c'mask c'surf c'limits c'nlimits c'timestamp c'timescan0 c'scannumber c'subprojection c'uqx c'uqy c'uqz c'sampleAxis c'polarisation c'dmask
      (ImageWord16 arr) -> unsafeWith arr $ \i -> do
                                 {-# SCC "hkl_binoculars_space_qcustom_uint16_t" #-} c'hkl_binoculars_space_qcustom_uint16_t c'pSpace c'geometry i c'nPixels c'attenuation c'flat c'pix c'ndim c'dims c'r c'nr c'mask c'surf c'limits c'nlimits c'timestamp c'timescan0 c'scannumber c'subprojection c'uqx c'uqy c'uqz c'sampleAxis c'polarisation c'dmask
      (ImageWord32 arr) -> unsafeWith arr $ \i -> do
                                 {-# SCC "hkl_binoculars_space_qcustom_uint32_t" #-} c'hkl_binoculars_space_qcustom_uint32_t c'pSpace c'geometry i c'nPixels c'attenuation c'flat c'pix c'ndim c'dims c'r c'nr c'mask c'surf c'limits c'nlimits c'timestamp c'timescan0 c'scannumber c'subprojection c'uqx c'uqy c'uqz c'sampleAxis c'polarisation c'dmask

    return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processQCustomP :: ( MonadIO m, MonadLogger m
                  , MonadReader (Config 'QCustomProjection) m
                  , MonadThrow m)
                => m ()
processQCustomP = do
  (conf :: Config 'QCustomProjection) <- ask

  logDebugNSH conf

  -- directly from the common config
  let common = binocularsConfig'QCustom'Common conf
  let overwrite = binocularsConfig'Common'Overwrite common
  let det = binocularsConfig'Common'Detector common
  let (NCores cap) =  binocularsConfig'Common'NCores common
  let destination = binocularsConfig'Common'Destination common
  let mImageSumMax = binocularsConfig'Common'ImageSumMax common
  let inputRange = binocularsConfig'Common'InputRange common
  let nexusDir = binocularsConfig'Common'Nexusdir common
  let tmpl = binocularsConfig'Common'Tmpl common
  let mSkipFirstPoints = binocularsConfig'Common'SkipFirstPoints common
  let mSkipLastPoints = binocularsConfig'Common'SkipLastPoints common
  let doPolarizationCorrection = binocularsConfig'Common'PolarizationCorrection common
  let mdmask =  binocularsConfig'Common'DynamicMask common

  -- directly from the specific config
  let mlimits = binocularsConfig'QCustom'ProjectionLimits conf
  let res = binocularsConfig'QCustom'ProjectionResolution conf
  let surfaceOrientation = binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum conf
  let subprojection = fromJust (binocularsConfig'QCustom'SubProjection conf) -- should not be Maybe
  let projectionType = binocularsConfig'QCustom'ProjectionType conf
  let (Degree uqx) = binocularsConfig'QCustom'Uqx conf
  let (Degree uqy) = binocularsConfig'QCustom'Uqy conf
  let (Degree uqz) = binocularsConfig'QCustom'Uqz conf
  let mSampleAxis = binocularsConfig'QCustom'SampleAxis conf

  let datapaths = getDataPath conf

  -- built from the config
  output' <- liftIO $ destination' projectionType (Just subprojection) inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir inputRange tmpl

  logDebugNSH filenames

  -- compute the jobs

  let fns = concatMap (replicate 1) (toList filenames)
  chunks' <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP mSkipFirstPoints mSkipLastPoints datapaths
  let chunks = Prelude.map fst chunks'
  let ntot = sum (Prelude.map csize chunks')
  let ndim1 = sum (Prelude.map clength chunks)
  let jobs = chunk (quot ndim1 cap) chunks

  -- log parameters

  logDebugNSH datapaths
  logDebugNSH chunks'
  logDebugNSH ntot
  logDebugNSH jobs
  logDebugN "start gessing final cube size"

  -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

  guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
    runSafeT $ runEffect $
    each chunks
    >-> Pipes.Prelude.map quadChunk
    >-> framesP datapaths
    >-> project det 3 (spaceQCustom det res surfaceOrientation mlimits subprojection uqx uqy uqz mSampleAxis doPolarizationCorrection mdmask)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN $ pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map allChunk
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ _ img _ _ _ _ _) -> filterSumImage mImageSumMax img)
                             -- >-> Pipes.Prelude.print
                             >-> project det 3 (spaceQCustom det res surfaceOrientation mlimits subprojection uqx uqy uqz mSampleAxis doPolarizationCorrection mdmask)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'


---------
-- Cmd --
---------

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom mf mr = cmd processQCustomP mf (Args'QCustomProjection mr)

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = defaultConfig
             { binocularsConfig'QCustom'Common = defaultConfig
                                                 { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> Maybe ConfigRange -> m ()
updateQCustom mf mr = cmd (pure ()) mf (Args'QCustomProjection mr)
