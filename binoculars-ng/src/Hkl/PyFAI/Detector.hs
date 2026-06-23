{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Hkl.PyFAI.Detector
       ( ToPyFAI(..)
       ) where

import           Data.Text    (Text)

import           Hkl.Detector

class ToPyFAI a where
  toPyFAI ∷ a → Text

instance ToPyFAI (Detector a sh) where
  toPyFAI Xpad32             = "Xpad_flat"
  toPyFAI PilatusCdTe2M      = "PilatusCdTe2M"
  toPyFAI ImXpadS140         = "imxpad_s140"
  toPyFAI XpadFlatCorrected  = error "Unsupported Detector"
  toPyFAI ZeroD              = error "Unsupported Detector"
  toPyFAI (Detector2D _ _ _) = undefined

instance ToPyFAI SomeDetector where
  toPyFAI (SomeDetector v) = toPyFAI v
