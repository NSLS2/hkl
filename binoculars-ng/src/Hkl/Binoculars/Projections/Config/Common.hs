{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

module Hkl.Binoculars.Projections.Config.Common
    ( Args(..)
    , Common
    , Config(..)
    ) where

import           Control.Applicative               ((<|>))
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Ini.Config                   (parseIniFile)
import           Data.Ini.Config.Bidir             (FieldValue (..))
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Map.Strict                   (Map, empty, foldMapWithKey)
import           Data.Text                         (Text, pack)
import           GHC.Generics                      (Generic)
import           Numeric.Interval                  (singleton)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.Orphan                        ()
import           Hkl.Repa
import           Hkl.Types

data Common

instance ToIni (Map Text Int) where
    toIni m
        = if null m
          then mempty
          else Ini { iniSections = fromList [ ( "geometry.values", foldMapWithKey (\k v -> [(k <> "_shift", pack $ printf "%d" v)]) m ) ]
                    , iniGlobals = []
                    }

instance HasIniConfig Common where

    data Config Common
        = BinocularsConfig'Common
          { binocularsConfig'Common'NCores                 :: NCores
          , binocularsConfig'Common'Destination            :: DestinationTmpl
          , binocularsConfig'Common'Overwrite              :: Bool
          , binocularsConfig'Common'InputType              :: InputType
          , binocularsConfig'Common'Nexusdir               :: Maybe (Path Abs Dir)
          , binocularsConfig'Common'Tmpl                   :: Maybe InputTmpl
          , binocularsConfig'Common'InputRange             :: ConfigRange
          , binocularsConfig'Common'Detector               :: Detector Hkl DIM2
          , binocularsConfig'Common'Image                  :: Maybe (DSWrap_ DSImage DSPath) -- TODO document...
          , binocularsConfig'Common'Centralpixel           :: (Int, Int)
          , binocularsConfig'Common'Sdd                    :: Meter
          , binocularsConfig'Common'Detrot                 :: Degree
          , binocularsConfig'Common'AttenuationCoefficient :: Maybe Double
          , binocularsConfig'Common'AttenuationMax         :: Maybe Float
          , binocularsConfig'Common'AttenuationShift       :: Maybe Int
          , binocularsConfig'Common'Maskmatrix             :: Maybe MaskLocation
          , binocularsConfig'Common'Flatmatrix             :: Maybe FlatLocation
          , binocularsConfig'Common'Wavelength             :: Maybe Double
          , binocularsConfig'Common'ImageSumMax            :: Maybe Double
          , binocularsConfig'Common'SkipFirstPoints        :: Maybe Int
          , binocularsConfig'Common'SkipLastPoints         :: Maybe Int
          , binocularsConfig'Common'PolarizationCorrection :: PolarisationCorrection
          , binocularsConfig'Common'Geometry               :: Geometry
          , binocularsConfig'Common'DynamicMask            :: Maybe DynamicMask
          , binocularsConfig'Common'GeometryValues         :: Map Text Double
          , binocularsConfig'Common'GeometryShifts         :: Map Text Int
          , binocularsConfig'Common'_Content               :: ConfigContent  -- TO DELETE
          } deriving (Show, Generic)

    data Args Common
        = Args'Common (Maybe ConfigRange)

    defaultConfig
        = BinocularsConfig'Common
          { binocularsConfig'Common'NCores = NCores 4
          , binocularsConfig'Common'Destination = DestinationTmpl "{projection}_{first}-{last}_{limits}.h5"
          , binocularsConfig'Common'Overwrite = False
          , binocularsConfig'Common'InputType = SixsFlyUhv
          , binocularsConfig'Common'Nexusdir = Nothing
          , binocularsConfig'Common'Tmpl = Nothing
          , binocularsConfig'Common'InputRange  = ConfigRange (InputRange (Numeric.Interval.singleton 1) :| [])
          , binocularsConfig'Common'Detector = defaultDetector
          , binocularsConfig'Common'Image = Nothing
          , binocularsConfig'Common'Centralpixel = (0, 0)
          , binocularsConfig'Common'Sdd = Meter (1 *~ meter)
          , binocularsConfig'Common'Detrot = Degree (0 *~ degree)
          , binocularsConfig'Common'AttenuationCoefficient = Nothing
          , binocularsConfig'Common'AttenuationMax = Nothing
          , binocularsConfig'Common'AttenuationShift = Nothing
          , binocularsConfig'Common'Maskmatrix = Nothing
          , binocularsConfig'Common'Flatmatrix = Nothing
          , binocularsConfig'Common'Wavelength = Nothing
          , binocularsConfig'Common'ImageSumMax = Nothing
          , binocularsConfig'Common'SkipFirstPoints = Nothing
          , binocularsConfig'Common'SkipLastPoints = Nothing
          , binocularsConfig'Common'PolarizationCorrection = PolarisationCorrection False
          , binocularsConfig'Common'Geometry = Geometry'Factory Uhv Nothing
          , binocularsConfig'Common'DynamicMask = Nothing
          , binocularsConfig'Common'GeometryValues = empty
          , binocularsConfig'Common'GeometryShifts = empty
          , binocularsConfig'Common'_Content = ConfigContent "" -- TODELETE
          }

    configToIni c
        = toIni (binocularsConfig'Common'Geometry c)
          `mergeIni`
           toIni (binocularsConfig'Common'GeometryShifts c)
          `mergeIni`
          Ini { iniSections = fromList $ [ ("dispatcher",
                                            elemFDef "ncores" binocularsConfig'Common'NCores c defaultConfig
                                            [ "the number of cores use for computation."
                                            , "the effective number of core used depends on the real number of core available on your computer."
                                            , "at maximum this value could be one less that the physical number of core."
                                            , ""
                                            , "if `zero` use the maximum of capabilities."
                                            ]
                                           <> elemFDef "destination" binocularsConfig'Common'Destination c defaultConfig
                                            [ "the template used to produce the destination file"
                                            , ""
                                            , "You can write what you want in the field but it need to end with .h5"
                                            , "The generated file is an hdf5 file. You can add in the text some specific"
                                            , "string which will be replace with values extracted from the configuration"
                                            , "when saving the file."
                                            , ""
                                            , "These parameters are:"
                                            , "  `{first}`      - replaced by the first scan numer of the serie"
                                            , "  `{last}`       - replaced by the last scan numer of the serie"
                                            , "  `{limits}`     - replaced by the limits of the final cube or nolimits"
                                            , "  `{projection}` - the projection name"
                                            ]
                                           <> elemFDef "overwrite" binocularsConfig'Common'Overwrite c defaultConfig
                                            [ " `true` - the output file name is always the same, so it is overwriten when"
                                            , "recomputed with the same parameter."
                                            , " `false` - the output is modifier and `_<number>` is added to the filename in order"
                                            , " to avoid overwriting them."
                                            ]
                                           )
                                         , ("input",
                                            elemFDef "type" binocularsConfig'Common'InputType c defaultConfig
                                            ([ "Define the experimental setup and the type of scan used to acquire the data"
                                             , ""
                                                          , "the list of the available values are:"
                                             , ""
                                             ]
                                            <> [" - " <> fvEmit fieldvalue v | v <- [minBound..maxBound :: InputType]]
                                            <> [ ""
                                               , "`sbs` means step by step scan."
                                               , ""
                                               , "`fly` or `flyscan` means flyscan."
                                               , ""
                                               , "`medh` or `medv` or `uhv` refers to the diffractometer."
                                               , ""
                                               , "Some configurations specify the detector like eiger/s70..."
                                               , "look at the `detector` parameter if you need to select another one."
                                               , ""
                                               , "This parameter defines how to find the datas in the hdf5 file."
                                               ]
                                            )
                                           <> elemFMbDef "nexusdir" binocularsConfig'Common'Nexusdir c defaultConfig
                                            [ "Directory path where binoculars-ng looks for the data."
                                            , ""
                                            , "All sub directories are explored."
                                            , "It can be relative to the current directory or absolute"
                                            ]
                                           <> elemFMbDef "inputtmpl" binocularsConfig'Common'Tmpl c defaultConfig
                                            [ "data files are matched with this template in order to decide if it must be used for computation."
                                            , ""
                                            , "the template is pre-processed with the printf logic and an integer value comming from the the `inputrange`"
                                            , "for example with the default value `%05d` and this `inputrange=1-10`, all supported files which contains"
                                            , "00001, 00002, ... 00010 are used during the computation."
                                            ]
                                           <> elemFDef "inputrange" binocularsConfig'Common'InputRange c defaultConfig
                                            [ "Indexes of the scans you want to process"
                                            , ""
                                            , "There are two ways to enter these values."
                                            , "  `n`         - a single scan with index n"
                                            , "  `n-m`       - a range of scans from n to m (included)"
                                            , ""
                                            , "  `n-m,p,...` - combination of scans indexes separated by a coma (no space allowed)."
                                            ]
                                           <> elemFDef "detector" binocularsConfig'Common'Detector c defaultConfig
                                            ([ "The detector name"
                                             , ""
                                             , "In order to process the data, we need the 3D position of each pixel"
                                             , "in the laboratory basis. Since every detector has it's own geometry,"
                                             , "it is necessary to provide this information unambiguously."
                                             , ""
                                             , "Some data files contain more than one detectors, so this parameter allows"
                                             , "to select the right one."
                                             , ""
                                             , "the list of known detectors is:"]
                                            <> [" - " <> pack n | (Detector2D _ n _) <- detectors]
                                            )
                                           <> elemFMbDef "images" binocularsConfig'Common'Image c defaultConfig
                                            [ "where to find the image in the files JSON encoded" ]
                                           <> elemFDef "centralpixel" binocularsConfig'Common'Centralpixel c defaultConfig
                                            [ "x,y coordinates in pixels of the direct beam on the detector."
                                            , ""
                                            , "with `sdd` and `detrot` this parameter allow to set the"
                                            , "detector position on the detector's arm when all motors"
                                            , "are set equal to `0`."
                                            ]
                                           <> elemFDef "sdd" binocularsConfig'Common'Sdd c defaultConfig
                                            [ "sample-detector-distance expressed in meter."
                                            , ""
                                            , "with `centralpixel` and `detrot` this parameter allow to set the"
                                            , "detector position on the detector's arm when all motors"
                                            , "are set equal to `0`."
                                            ]
                                           <> elemFDef "detrot" binocularsConfig'Common'Detrot c defaultConfig
                                            [ "rotation of the detector along the x axis expressed in Degrees."
                                            , ""
                                            , "with `centralpixel` and `sdd` this parameter allow to set the"
                                            , "detector position on the detector's arm when all motors"
                                            , "are set equal to `0`."
                                            ]
                                           <> elemFMbDef "attenuation_coefficient" binocularsConfig'Common'AttenuationCoefficient c defaultConfig
                                            ["the attenuation coefficient used to correct the detector's data."
                                            , ""
                                            , " `<not set>` - no correction is applyed"
                                            , " `value`     - a correction is applyed"
                                            , ""
                                            , "The `value` depends on the energy of the experiment."
                                            , ""
                                            , "On Sixs there is two different kind of attenuation system."
                                            , " `new attenuation` - based on piezo actuators (fast)"
                                            , " `old attenuation` - based on pneumatic` actuators (slower)"
                                            , ""
                                            , "both system save the same kind of data's in the data files."
                                            , "An attenuation value `v` at each step of the scan which allows"
                                            , "to compute the image correction with this formula:"
                                            , ""
                                            , "  `attenuation_coef ** v`"
                                            , ""
                                            , "The only difference is a shift of the index for the the new attenuation."
                                            , "The applyed correction for the n-th point is computed with the `value`"
                                            , "at n + 2"
                                            ]
                                           <> elemFMbDef "attenuation_max" binocularsConfig'Common'AttenuationMax c defaultConfig
                                            [ "maximum attenuation allow for the attenuation correction."
                                            , ""
                                            , " `<not set>` - always apply the attenuation correction."
                                            , " `max`       - apply the attenuation if `v` is lower or equal to `max`"
                                            , ""
                                            , "`v` is the value stored by the attenuation system in the data file."
                                            , ""
                                            , "This parameter has an effect only if the `attenuation_coefficient`"
                                            , "was previously set."
                                            ]
                                           <> elemFMbDef "attenuation_shift" binocularsConfig'Common'AttenuationShift c defaultConfig
                                            [ "shift the index of the attenuation."
                                            , ""
                                            , " `<not set>` - use the default offset. (0 or 2 depending of the kind of scan)"
                                            , " `shift`     - force the shift to the given value."
                                            , ""
                                            , "This `shift` correspond to a decalage between the attenuator values and the images."
                                            , "the attenuation corresponding to the image at the position `n` in a scan"
                                            , "is in fact `n + shift`"
                                            , ""
                                            , "This parameter has an effect only if the `attenuation_coefficient`"
                                            , "was previously set."
                                            ]
                                           <> elemFMbDef "maskmatrix" binocularsConfig'Common'Maskmatrix c defaultConfig
                                            [ "name of the file which contain the detector mask or `default`"
                                            , ""
                                            , "The supported type of file is for now only .npy (double | bool)"
                                            , ""
                                            , "the `default` value can be set in order to use the default"
                                            , "mask of the detector. Some of them have by default some area"
                                            , "which required to be masked (pixels with non standard surface, "
                                            , "area with holes)."
                                            , ""
                                            , "sometime you need to assign a specific mask to a scan number."
                                            , "In that case, just add `{scannumber:<format>}` in the filename where"
                                            , "<format> is equivalent to the Python f-string logic."
                                            , "This small template will be used to compute the real mask filename."
                                            , "for exemple for the scannumber 8"
                                            , "   - mask_{scannumber:02d}.npy -> mask_08.npy"
                                            , "   - mask_{scannumber:03d}.npy -> mask_008.npy"
                                            , ""
                                            , "You can combine multiple masks with an or logic using the `|` operator"
                                            , "like this:"
                                            , "  `mask_{scannumber:02d}.npy | mask.npy | default`"
                                            , ""
                                            , "Most of the time a mask file was generated during the experiment."
                                            ]
                                           <> elemFMbDef "flatmatrix" binocularsConfig'Common'Flatmatrix c defaultConfig
                                            [ "name of the file which contain the detector flat or `default`"
                                            , ""
                                            , "The supported type of file is for now only .npy (double)"
                                            , ""
                                            , "the `default` value can be set in order to use the default"
                                            , "flat of the detector. Some of them have by default some area"
                                            , "which required to be flatten (pixels with non standard surface, "
                                            , "area with holes)."
                                            , ""
                                            , "sometime you need to assign a specific flat to a scan number."
                                            , "In that case, just add `{scannumber:<format>}` in the filename where"
                                            , "<format> is equivalent to the Python f-string logic."
                                            , "This small template will be used to compute the real falt filename."
                                            , "for exemple for the scannumber 8"
                                            , "   - flat_{scannumber:02d}.npy -> flat_08.npy"
                                            , "   - flat_{scannumber:03d}.npy -> flat_008.npy"
                                            , ""
                                            , "Most of the time a flat file was generated during the experiment."
                                            ]
                                           <> elemFMbDef "wavelength" binocularsConfig'Common'Wavelength c defaultConfig
                                            [ "overwrite the wavelength from the data file with the one provided."
                                            , ""
                                            , " `<not set>`  - use the data file wavelength"
                                            , " `wavelength` - overwrite the data file value with this one."
                                            ]
                                           <> elemFMbDef "image_sum_max" binocularsConfig'Common'ImageSumMax c defaultConfig
                                            [ "maximum intensity value allow for the sum of all pixels in an image in order to process it."
                                            , ""
                                            , " `<not set>` - process all images."
                                            , " `max`       - process images only if the sum of all the pixels is lower than `max`"
                                            ]
                                           <> elemFMbDef "skip_first_points" binocularsConfig'Common'SkipFirstPoints c defaultConfig
                                            [ "skip the first `n` points of each scan."
                                            , ""
                                            , " `<not set>` - process all images."
                                            , " `n`         - skip the first `n` points (`n` included)"
                                            ]
                                           <> elemFMbDef "skip_last_points" binocularsConfig'Common'SkipLastPoints c defaultConfig
                                            [ "skip the last `n` points of each scan."
                                            , ""
                                            , " `<not set>` - process all images."
                                            , " `n`         - skip the last `n` points (`n` included)"
                                            ]
                                           <> elemFDef "polarization_correction" binocularsConfig'Common'PolarizationCorrection c defaultConfig
                                            [ " `true` - apply the polarization correctionthe to the intensities."
                                            , " `false` - do not apply the polarization correction."
                                            , " to avoid overwriting them."
                                            ]
                                           <> elemFMbDef "dynamic_mask" binocularsConfig'Common'DynamicMask c defaultConfig
                                            [ "mask pixels of the detector if the pixel value is above this value."
                                            , ""
                                            , " `<not set>` - process all pixels."
                                            , " `threshold` - process pixel only if its value is lower than `threshold`"
                                            ]
                                           )
                                         ]
              , iniGlobals = []
              }

    getConfig content@(ConfigContent cfg) (Args'Common mr) (Capabilities ncapmax ncoresmax)
        = do
         let minputtypedeprecated = eitherF (const Nothing) (parse' cfg "input" "type") id

         binocularsConfig'Common'NCores <- eitherF error (parse' cfg "dispatcher" "ncores")
                                          (\mb -> do
                                             let ns = [ncapmax, ncoresmax - 1] <> case mb of
                                                                                    Nothing -> let (NCores b) = binocularsConfig'Common'NCores defaultConfig
                                                                                              in [b]
                                                                                    Just b  -> case b of
                                                                                                0 -> []
                                                                                                _ -> [b]
                                             pure $ NCores (minimum ns))
         binocularsConfig'Common'Destination <- parseFDef cfg "dispatcher" "destination" (binocularsConfig'Common'Destination defaultConfig)
         binocularsConfig'Common'Overwrite <- parseFDef cfg "dispatcher" "overwrite" (binocularsConfig'Common'Overwrite defaultConfig)
         binocularsConfig'Common'InputType <- case minputtypedeprecated of
                                               Nothing -> parseFDef cfg "input" "type" (binocularsConfig'Common'InputType defaultConfig)
                                               Just deprecated -> Right $ case deprecated of
                                                                           SixsFlyMedVEiger          -> SixsFlyMedV
                                                                           SixsFlyMedVS70            -> SixsFlyMedV
                                                                           SixsFlyScanUhvGisaxsEiger -> SixsFlyUhvGisaxs
                                                                           SixsFlyScanUhvUfxc        -> SixsFlyUhv
         binocularsConfig'Common'Nexusdir <- parseMb cfg "input" "nexusdir"
         binocularsConfig'Common'Tmpl <- parseMb cfg "input" "inputtmpl"
         binocularsConfig'Common'InputRange <- eitherF error (parse' cfg "input" "inputrange")
                                              (\mb -> do
                                                 case mr <|> mb of
                                                   Nothing -> error "please provide an input range either in the config file with the \"inputrange\" key under the \"input\" section, or on the command line"
                                                   Just r -> pure r)
         binocularsConfig'Common'Detector <- parseFDef cfg "input" "detector"
                                            (case minputtypedeprecated of
                                               Nothing -> case binocularsConfig'Common'InputType  of
                                                           CristalK6C -> mkDetector HklBinocularsDetectorEnum'XpadFlatCorrected
                                                           Custom -> binocularsConfig'Common'Detector defaultConfig
                                                           DiffabsCirpad -> undefined -- mkDetector HklBinocularsDetectorEnum'Cirpad
                                                           MarsCx3Sbs -> mkDetector HklBinocularsDetectorEnum'Dectris'PilatusCdTe2M
                                                           MarsFlyscan -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                           MarsSbs -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                           SixsFlyMedH -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsFlyMedHGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsFlyMedV -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsFlyMedVGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsFlyUhv -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsFlyUhvGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsMedH -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsMedHGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsMedV -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsMedVGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsUhv -> binocularsConfig'Common'Detector defaultConfig
                                                           SixsSbsUhvGisaxs -> binocularsConfig'Common'Detector defaultConfig
                                               Just deprecated -> case deprecated of
                                                                   SixsFlyMedVEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                                   SixsFlyMedVS70 -> mkDetector HklBinocularsDetectorEnum'ImxpadS70
                                                                   SixsFlyScanUhvGisaxsEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                                   SixsFlyScanUhvUfxc -> mkDetector HklBinocularsDetectorEnum'Ufxc
                                            )
         binocularsConfig'Common'Image <- parseMb cfg "input" "images"
         binocularsConfig'Common'Centralpixel <- eitherF error (parse' cfg "input" "centralpixel")
                                                (\mc -> do
                                                   case mc of
                                                     Nothing -> pure (binocularsConfig'Common'Centralpixel defaultConfig)
                                                     Just c -> if c `inDetector` binocularsConfig'Common'Detector
                                                              then pure c
                                                              else error $ "The central pixel " <> show c <> " is not compatible with the detector")
         binocularsConfig'Common'Sdd <- parseFDef cfg "input" "sdd" (binocularsConfig'Common'Sdd defaultConfig)
         binocularsConfig'Common'Detrot <- parseFDef cfg "input" "detrot" (binocularsConfig'Common'Detrot defaultConfig)
         binocularsConfig'Common'AttenuationCoefficient <- parseMb cfg "input" "attenuation_coefficient"
         binocularsConfig'Common'AttenuationMax <- parseMb cfg "input" "attenuation_max"
         binocularsConfig'Common'AttenuationShift <- parseMb cfg "input" "attenuation_shift"
         binocularsConfig'Common'Maskmatrix <- parseMb cfg "input" "maskmatrix"
         binocularsConfig'Common'Flatmatrix <- parseMb cfg "input" "flatmatrix"
         binocularsConfig'Common'Wavelength <- parseMb cfg "input" "wavelength"
         binocularsConfig'Common'ImageSumMax <- parseMb cfg "input" "image_sum_max"
         binocularsConfig'Common'SkipFirstPoints <- parseMb cfg "input" "skip_first_points"
         binocularsConfig'Common'SkipLastPoints <- parseMb cfg "input" "skip_last_points"
         binocularsConfig'Common'PolarizationCorrection <- parseFDef cfg "input" "polarization_correction" (binocularsConfig'Common'PolarizationCorrection defaultConfig)
         binocularsConfig'Common'Geometry <- eitherF error (parseIniFile cfg iniParser'Maybe'Geometry)
                                            (\mg -> pure $ case mg of
                                                            Nothing -> case binocularsConfig'Common'InputType of
                                                                        CristalK6C        -> Geometry'Factory K6c Nothing
                                                                        Custom            -> undefined
                                                                        DiffabsCirpad     -> cirpad
                                                                        MarsCx3Sbs        -> marsCx3
                                                                        MarsFlyscan       -> Geometry'Factory Mars Nothing
                                                                        MarsSbs           -> Geometry'Factory Mars Nothing
                                                                        SixsFlyMedH       -> Geometry'Factory MedH Nothing
                                                                        SixsFlyMedHGisaxs -> sixsMedHGisaxs
                                                                        SixsFlyMedV       -> Geometry'Factory MedV Nothing
                                                                        SixsFlyMedVGisaxs -> sixsMedVGisaxs
                                                                        SixsFlyUhv        -> Geometry'Factory Uhv Nothing
                                                                        SixsFlyUhvGisaxs  -> sixsUhvGisaxs
                                                                        SixsSbsMedH       -> Geometry'Factory MedH Nothing
                                                                        SixsSbsMedHGisaxs -> sixsMedHGisaxs
                                                                        SixsSbsMedV       -> Geometry'Factory MedV Nothing
                                                                        SixsSbsMedVGisaxs -> sixsMedVGisaxs
                                                                        SixsSbsUhv        -> Geometry'Factory Uhv Nothing
                                                                        SixsSbsUhvGisaxs  -> sixsUhvGisaxs
                                                            Just g -> g
                                            )
         binocularsConfig'Common'DynamicMask <- parseMb cfg "input" "dynamic_mask"
         binocularsConfig'Common'GeometryValues <- parseIniFile cfg iniParser'GeometryValues
         binocularsConfig'Common'GeometryShifts <- parseIniFile cfg (iniParser'GeometryShifts binocularsConfig'Common'Geometry)
         let binocularsConfig'Common'_Content = content  -- TO DELETE

         pure BinocularsConfig'Common{..}
