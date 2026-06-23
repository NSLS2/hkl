{-# LANGUAGE UnicodeSyntax #-}

{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2022, 2023, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.PyFAI.AzimuthalIntegrator
       ( AIMethod(..)
       ) where

data AIMethod = Numpy | Cython | SplitPixel | Lut | Csr | NoSplitCsr | FullCsr | LutOcl | CsrOcl

instance Show AIMethod where
  show Numpy      = "numpy"
  show Cython     = "cython"
  show SplitPixel = "splitpixel"
  show Lut        = "lut"
  show Csr        = "csr"
  show NoSplitCsr = "nosplit_csr"
  show FullCsr    = "full_csr"
  show LutOcl     = "lut_ocl"
  show CsrOcl     = "csr_ocl"
