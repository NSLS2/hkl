#include "hkl-binoculars.h"
#include <bindings.dsl.h>

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.C.Binoculars where

import           Data.Int              (Int32)
import           Data.Word             (Word16, Word32)
import           Foreign.C.Types       (CBool(..), CDouble(..), CInt(..), CSize(..), CUInt(..), CPtrdiff)
import           Foreign.C.String      (CString, withCString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array (withArrayLen)
import           Foreign.Ptr           (FunPtr, Ptr)
import           System.IO.Unsafe      (unsafePerformIO)

import           Hkl.C.Hkl
import           Hkl.Repa

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings = foldr (\v kk -> \k -> (withCString v) (\a -> kk (\as -> k (a:as)))) ($ [])

----------------
-- AxisLimits --
----------------

#opaque_t HklBinocularsAxisLimits

#ccall hkl_binoculars_axis_limits_free, \
  Ptr <HklBinocularsAxisLimits> -> IO ()

#ccall hkl_binoculars_axis_limits_new, \
  Ptr CPtrdiff -> Ptr CPtrdiff -> IO (Ptr <HklBinocularsAxisLimits>)

----------
-- Cube --
----------

#opaque_t HklBinocularsCube

data Cube sh = Cube (ForeignPtr C'HklBinocularsCube)
             | EmptyCube
             deriving Show

newCube :: Ptr C'HklBinocularsCube -> IO (Cube sh)
newCube p = Cube <$> (newForeignPtr p'hkl_binoculars_cube_free p)

instance Shape sh => Semigroup (Cube sh) where
  {-# INLINE (<>) #-}
  EmptyCube <> a = a
  a <> EmptyCube = a
  (Cube fpa) <> (Cube fpb) = unsafePerformIO $ do
    withForeignPtr fpa $ \pa ->
      withForeignPtr fpb $ \pb ->
      newCube =<< {-# SCC "c'hkl_binoculars_cube_new_merge'" #-} c'hkl_binoculars_cube_new_merge pa pb

#ccall hkl_binoculars_cube_new_merge, \
  Ptr <HklBinocularsCube> -> Ptr <HklBinocularsCube> -> IO (Ptr <HklBinocularsCube>)

instance Shape sh => Monoid (Cube sh) where
  {-# INLINE mempty #-}
  mempty = EmptyCube

cube'MergeAndSave :: String -> String -> [String] -> IO ()
cube'MergeAndSave output config fns =
  withCString output $ \c'output ->
  withCString config $ \ c'config ->
  withCStrings fns $ \cfns ->
  withArrayLen cfns $ \c'nfns c'cfns -> do
  c'hkl_binoculars_cube_merge_and_save_hdf5 c'output c'config c'cfns (toEnum c'nfns)


#ccall hkl_binoculars_cube_add_space, Ptr <HklBinocularsCube> -> Ptr <HklBinocularsSpace> -> IO ()
#ccall hkl_binoculars_cube_free, Ptr <HklBinocularsCube> -> IO ()
#ccall hkl_binoculars_cube_merge_and_save_hdf5, CString -> CString -> Ptr CString -> CSize -> IO ()
#ccall hkl_binoculars_cube_new, CSize -> Ptr (Ptr <HklBinocularsSpace>) -> IO (Ptr <HklBinocularsCube>)
#ccall hkl_binoculars_cube_new_empty, IO (Ptr <HklBinocularsCube>)
#ccall hkl_binoculars_cube_new_empty_from_cube, Ptr <HklBinocularsCube> -> IO (Ptr <HklBinocularsCube>)
#ccall hkl_binoculars_cube_new_from_file, CString -> IO (Ptr <HklBinocularsCube>)
#ccall hkl_binoculars_cube_save_hdf5, CString -> CString -> Ptr <HklBinocularsCube> -> IO CInt

--------------
-- Detector --
--------------

#integral_t HklBinocularsDetectorEnum

#num HKL_BINOCULARS_DETECTOR_IMXPAD_S140
#num HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED
#num HKL_BINOCULARS_DETECTOR_IMXPAD_S70
#num HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M
#num HKL_BINOCULARS_DETECTOR_UFXC
#num HKL_BINOCULARS_DETECTOR_MERLIN
#num HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD
#num HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD_512
#num HKL_BINOCULARS_DETECTOR_CIRPAD
#num HKL_BINOCULARS_DETECTOR_RIGAKU_XSPA_1M

data HklBinocularsDetectorEnum
  = HklBinocularsDetectorEnum'ImxpadS140
  | HklBinocularsDetectorEnum'XpadFlatCorrected
  | HklBinocularsDetectorEnum'ImxpadS70
  | HklBinocularsDetectorEnum'DectrisEiger1M
  | HklBinocularsDetectorEnum'Ufxc
  | HklBinocularsDetectorEnum'Merlin
  | HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
  | HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512
  | HklBinocularsDetectorEnum'Cirpad
  | HklBinocularsDetectorEnum'RigakuXspa1M
  deriving (Bounded, Eq, Show)

instance Enum HklBinocularsDetectorEnum where
  toEnum n
    | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140 = HklBinocularsDetectorEnum'ImxpadS140
    | n == c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED = HklBinocularsDetectorEnum'XpadFlatCorrected
    | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70 = HklBinocularsDetectorEnum'ImxpadS70
    | n == c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M = HklBinocularsDetectorEnum'DectrisEiger1M
    | n == c'HKL_BINOCULARS_DETECTOR_UFXC = HklBinocularsDetectorEnum'Ufxc
    | n == c'HKL_BINOCULARS_DETECTOR_MERLIN = HklBinocularsDetectorEnum'Merlin
    | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD = HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
    | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD_512 = HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512
    | n == c'HKL_BINOCULARS_DETECTOR_CIRPAD = HklBinocularsDetectorEnum'Cirpad
    | n == c'HKL_BINOCULARS_DETECTOR_RIGAKU_XSPA_1M = HklBinocularsDetectorEnum'RigakuXspa1M
    | otherwise = error ("Non supported Detector type" ++ show n)

  fromEnum HklBinocularsDetectorEnum'ImxpadS140 = c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140
  fromEnum HklBinocularsDetectorEnum'XpadFlatCorrected = c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED
  fromEnum HklBinocularsDetectorEnum'ImxpadS70 = c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70
  fromEnum HklBinocularsDetectorEnum'DectrisEiger1M = c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M
  fromEnum HklBinocularsDetectorEnum'Ufxc =  c'HKL_BINOCULARS_DETECTOR_UFXC
  fromEnum HklBinocularsDetectorEnum'Merlin =  c'HKL_BINOCULARS_DETECTOR_MERLIN
  fromEnum HklBinocularsDetectorEnum'MerlinMedipix3rxQuad = c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD
  fromEnum HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512 = c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD_512
  fromEnum HklBinocularsDetectorEnum'Cirpad = c'HKL_BINOCULARS_DETECTOR_CIRPAD
  fromEnum HklBinocularsDetectorEnum'RigakuXspa1M = c'HKL_BINOCULARS_DETECTOR_RIGAKU_XSPA_1M

#ccall hkl_binoculars_detector_2d_coordinates_get, <HklBinocularsDetectorEnum> -> IO (Ptr CDouble)
#ccall hkl_binoculars_detector_2d_flat_get, <HklBinocularsDetectorEnum> -> IO (Ptr CDouble)
#ccall hkl_binoculars_detector_2d_flat_load, <HklBinocularsDetectorEnum> -> CString -> IO (Ptr CDouble)
#ccall hkl_binoculars_detector_2d_img_load_into, <HklBinocularsDetectorEnum> -> CString -> Ptr Int32 -> CInt -> IO ()
#ccall hkl_binoculars_detector_2d_mask_get, <HklBinocularsDetectorEnum> -> IO (Ptr CBool)
#ccall hkl_binoculars_detector_2d_mask_load, <HklBinocularsDetectorEnum> -> CString -> IO (Ptr CBool)
#ccall hkl_binoculars_detector_2d_mask_or, <HklBinocularsDetectorEnum> -> Ptr CBool -> Ptr CBool -> IO (Ptr CBool)
#ccall hkl_binoculars_detector_2d_name_get, <HklBinocularsDetectorEnum> -> IO CString
#ccall hkl_binoculars_detector_2d_number_of_detectors, IO CInt
#ccall hkl_binoculars_detector_2d_shape_get, <HklBinocularsDetectorEnum> -> Ptr CInt -> Ptr CInt -> IO ()
#ccall hkl_binoculars_detector_2d_sixs_calibration, <HklBinocularsDetectorEnum> -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CInt -> IO ()

-----------
-- Space --
-----------

#integral_t HklBinocularsSurfaceOrientationEnum

#num HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL
#num HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL

data HklBinocularsSurfaceOrientationEnum
  = HklBinocularsSurfaceOrientationEnum'Vertical
  | HklBinocularsSurfaceOrientationEnum'Horizontal
  deriving (Bounded, Eq, Show)


instance Enum HklBinocularsSurfaceOrientationEnum where
  toEnum n
    | n == c'HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL = HklBinocularsSurfaceOrientationEnum'Vertical
    | n == c'HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL = HklBinocularsSurfaceOrientationEnum'Horizontal
    | otherwise = error "Non supported HklBinocularsSurfaceOrientationEnum value"

  fromEnum HklBinocularsSurfaceOrientationEnum'Vertical = c'HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL
  fromEnum HklBinocularsSurfaceOrientationEnum'Horizontal = c'HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL

#integral_t HklBinocularsQCustomSubProjectionEnum

#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY
#num HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ

data HklBinocularsQCustomSubProjectionEnum
  = HklBinocularsQCustomSubProjectionEnum'QxQyQz
  | HklBinocularsQCustomSubProjectionEnum'QTthTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QparQper
  | HklBinocularsQCustomSubProjectionEnum'QPhiQx
  | HklBinocularsQCustomSubProjectionEnum'QPhiQy
  | HklBinocularsQCustomSubProjectionEnum'QPhiQz
  | HklBinocularsQCustomSubProjectionEnum'QStereo
  | HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis
  | HklBinocularsQCustomSubProjectionEnum'XYZ
  | HklBinocularsQCustomSubProjectionEnum'YZTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QQparQper
  | HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis
  | HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth
  | HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp
  | HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp
  | HklBinocularsQCustomSubProjectionEnum'TthAzimuth
  | HklBinocularsQCustomSubProjectionEnum'QTimescan0
  | HklBinocularsQCustomSubProjectionEnum'QScannumber
  | HklBinocularsQCustomSubProjectionEnum'TthScannumber
  | HklBinocularsQCustomSubProjectionEnum'PhixQThetax
  | HklBinocularsQCustomSubProjectionEnum'PhiyQThetay
  | HklBinocularsQCustomSubProjectionEnum'PhizQThetaz
  deriving (Bounded, Eq, Show)

instance Enum HklBinocularsQCustomSubProjectionEnum where
  toEnum n
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ = HklBinocularsQCustomSubProjectionEnum'QxQyQz
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QTthTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER = HklBinocularsQCustomSubProjectionEnum'QparQper
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX = HklBinocularsQCustomSubProjectionEnum'QPhiQx
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY = HklBinocularsQCustomSubProjectionEnum'QPhiQy
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ = HklBinocularsQCustomSubProjectionEnum'QPhiQz
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO = HklBinocularsQCustomSubProjectionEnum'QStereo
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS = HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z = HklBinocularsQCustomSubProjectionEnum'XYZ
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'YZTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER = HklBinocularsQCustomSubProjectionEnum'QQparQper
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS = HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH = HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP = HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH = HklBinocularsQCustomSubProjectionEnum'TthAzimuth
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0 = HklBinocularsQCustomSubProjectionEnum'QTimescan0
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER = HklBinocularsQCustomSubProjectionEnum'QScannumber
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER = HklBinocularsQCustomSubProjectionEnum'TthScannumber
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX = HklBinocularsQCustomSubProjectionEnum'PhixQThetax
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY = HklBinocularsQCustomSubProjectionEnum'PhiyQThetay
    | n == c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ = HklBinocularsQCustomSubProjectionEnum'PhizQThetaz
    | otherwise = error "Non supported HklBinocularsQCustomSubProjectionEnum value"

  fromEnum HklBinocularsQCustomSubProjectionEnum'QxQyQz = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ
  fromEnum HklBinocularsQCustomSubProjectionEnum'QTthTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QparQper = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER
  fromEnum HklBinocularsQCustomSubProjectionEnum'QPhiQx = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX
  fromEnum HklBinocularsQCustomSubProjectionEnum'QPhiQy = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY
  fromEnum HklBinocularsQCustomSubProjectionEnum'QPhiQz = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ
  fromEnum HklBinocularsQCustomSubProjectionEnum'QStereo = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO
  fromEnum HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS
  fromEnum HklBinocularsQCustomSubProjectionEnum'XYZ = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z
  fromEnum HklBinocularsQCustomSubProjectionEnum'YZTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QQparQper = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER
  fromEnum HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS
  fromEnum HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH
  fromEnum HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP
  fromEnum HklBinocularsQCustomSubProjectionEnum'TthAzimuth = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH
  fromEnum HklBinocularsQCustomSubProjectionEnum'QTimescan0 = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0
  fromEnum HklBinocularsQCustomSubProjectionEnum'QScannumber = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER
  fromEnum HklBinocularsQCustomSubProjectionEnum'TthScannumber = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER
  fromEnum HklBinocularsQCustomSubProjectionEnum'PhixQThetax = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX
  fromEnum HklBinocularsQCustomSubProjectionEnum'PhiyQThetay = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY
  fromEnum HklBinocularsQCustomSubProjectionEnum'PhizQThetaz = c'HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ

#opaque_t HklBinocularsSpace

#ccall hkl_binoculars_space_new, \
  CSize -> CSize -> IO (Ptr <HklBinocularsSpace>)

#ccall hkl_binoculars_space_free, \
  Ptr <HklBinocularsSpace> -> IO ()

type C'ProjectionTypeAngles t = Ptr C'HklBinocularsSpace -- HklBinocularsSpace *self
 -> Ptr C'HklGeometry -- const HklGeometry *geometry
 -> Ptr t --  const uint16_t *image
 -> CSize -- size_t n_pixels
 -> CDouble -- double weight
 -> Ptr CDouble -- double flat
 -> Ptr Double -- const double *pixels_coordinates
 -> CSize -- int32_t pixels_coordinates_ndim
 -> Ptr CSize --  const int32_t *pixels_coordinates_dims
 -> Ptr Double --  const double *resolutions
 -> CSize -- size_t n_resolutions
 -> Ptr CBool -- const uint8_t *mask
 -> Ptr (Ptr C'HklBinocularsAxisLimits) -- const HklBinocularsAxisLimits
 -> CSize -- size_t n_limits
 -> CString -- const char *sample_axis
 -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_angles_double" \
c'hkl_binoculars_space_angles_double :: C'ProjectionTypeAngles Double

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_angles_int32_t" \
c'hkl_binoculars_space_angles_int32_t :: C'ProjectionTypeAngles Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_angles_uint16_t" \
c'hkl_binoculars_space_angles_uint16_t :: C'ProjectionTypeAngles Word16

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_angles_uint32_t" \
c'hkl_binoculars_space_angles_uint32_t :: C'ProjectionTypeAngles Word32


type C'ProjectionTypeQCustom t = Ptr C'HklBinocularsSpace -- HklBinocularsSpace *self
 -> Ptr C'HklGeometry -- const HklGeometry *geometry
 -> Ptr t --  const uint16_t *image
 -> CSize -- size_t n_pixels
 -> CDouble -- double weight
 -> Ptr CDouble -- double flat
 -> Ptr Double -- const double *pixels_coordinates
 -> CSize -- int32_t pixels_coordinates_ndim
 -> Ptr CSize --  const int32_t *pixels_coordinates_dims
 -> Ptr Double --  const double *resolutions
 -> CSize -- size_t n_resolutions
 -> Ptr CBool -- const uint8_t *mask
 -> C'HklBinocularsSurfaceOrientationEnum -- surface orientation
 -> Ptr (Ptr C'HklBinocularsAxisLimits) -- const HklBinocularsAxisLimits
 -> CSize -- size_t n_limits
 -> CDouble -- double timestamp
 -> CDouble -- double timescan0
 -> CInt -- int scannumber
 -> C'HklBinocularsQCustomSubProjectionEnum -- int subprojection
 -> CDouble -- uqx
 -> CDouble -- uqy
 -> CDouble -- uqz
 -> CString -- const char *sample_axis
 -> CBool -- int do_polarization_correction
 -> Ptr CDouble -- double *image_threshold
 -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_qcustom_double" \
c'hkl_binoculars_space_qcustom_double :: C'ProjectionTypeQCustom Double

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_qcustom_int32_t" \
c'hkl_binoculars_space_qcustom_int32_t :: C'ProjectionTypeQCustom Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_qcustom_uint16_t" \
c'hkl_binoculars_space_qcustom_uint16_t :: C'ProjectionTypeQCustom Word16

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_qcustom_uint32_t" \
c'hkl_binoculars_space_qcustom_uint32_t :: C'ProjectionTypeQCustom Word32

type C'ProjectionTypeHkl t = Ptr C'HklBinocularsSpace -- HklBinocularsSpace *self
  -> Ptr C'HklGeometry -- const HklGeometry *geometry
  -> Ptr C'HklSample -- const HklSample *sample
  -> Ptr t --  const <t> *image
  -> CSize -- size_t n_pixels
  -> CDouble -- double weight
 -> Ptr CDouble -- double flat
  -> Ptr Double -- const double *pixels_coordinates
  -> CSize -- size_t pixels_coordinates_ndim
  -> Ptr CSize --  const int32_t *pixels_coordinates_dims
  -> Ptr Double --  const double *resolutions
  -> CSize -- size_t n_resolutions
  -> Ptr CBool -- const uint8_t *mask
  -> Ptr (Ptr C'HklBinocularsAxisLimits) -- const HklBinocularsAxisLimits
  -> CSize -- size_t n_limits
  -> CBool -- int do_polarization_correction
  -> Ptr CDouble -- double *image_threshold
  -> IO ()

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_double" \
c'hkl_binoculars_space_hkl_double :: C'ProjectionTypeHkl Double

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_int32_t" \
c'hkl_binoculars_space_hkl_int32_t :: C'ProjectionTypeHkl Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_uint16_t" \
c'hkl_binoculars_space_hkl_uint16_t :: C'ProjectionTypeHkl Word16


foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_hkl_uint32_t" \
c'hkl_binoculars_space_hkl_uint32_t :: C'ProjectionTypeHkl Word32



foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_test_double" \
c'hkl_binoculars_space_test_double :: C'ProjectionTypeHkl Double

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_test_int32_t" \
c'hkl_binoculars_space_test_int32_t :: C'ProjectionTypeHkl Int32

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_test_uint16_t" \
c'hkl_binoculars_space_test_uint16_t :: C'ProjectionTypeHkl Word16

foreign import ccall unsafe "hkl-binoculars.h hkl_binoculars_space_test_uint32_t" \
c'hkl_binoculars_space_test_uint32_t :: C'ProjectionTypeHkl Word32
