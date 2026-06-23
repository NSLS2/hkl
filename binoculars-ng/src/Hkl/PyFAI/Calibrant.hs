{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2022, 2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.PyFAI.Calibrant
    ( Calibrant(..) ) where

data Calibrant = CeO2 | LaB6

instance Show Calibrant where
  show CeO2 = "CeO2"
  show LaB6 = "LaB6"
