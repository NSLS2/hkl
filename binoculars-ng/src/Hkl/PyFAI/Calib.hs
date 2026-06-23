{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2022, 2023, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.PyFAI.Calib
       ( ToPyFAICalibArg(..) ) where

import           Data.Text                         (unpack)
import           Numeric.Units.Dimensional.Prelude (meter, nano, (/~))

import           Hkl.Detector                      (Detector)
import           Hkl.PyFAI.Calibrant               (Calibrant)
import           Hkl.PyFAI.Detector                (toPyFAI)
import           Hkl.PyFAI.Poni                    (WaveLength)

class ToPyFAICalibArg a where
  toPyFAICalibArg ∷ a → String

instance ToPyFAICalibArg FilePath where
  toPyFAICalibArg f = f

instance ToPyFAICalibArg (Detector a sh) where
  toPyFAICalibArg d = "-D" ++ unpack (toPyFAI d)

instance ToPyFAICalibArg Calibrant where
  toPyFAICalibArg c = "-c " ++ show c

instance ToPyFAICalibArg WaveLength where
    toPyFAICalibArg w = "-w " ++ show ((w /~ nano meter) * 10)
