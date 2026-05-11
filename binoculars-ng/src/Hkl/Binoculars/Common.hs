{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2024, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Common
  ( DataFrameSpace(..)
  , InputFn(..)
  , addSpace
  , mkCube'
  , toList
  , withCubeAccumulator
  ) where

import           Control.Exception          (bracket)
import           Data.IORef                 (IORef, newIORef, readIORef)
import           Foreign.ForeignPtr         (withForeignPtr)
import           Foreign.Marshal.Array      (withArrayLen)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.Image
import           Hkl.Orphan                 ()
import           Hkl.Repa
import           Hkl.Types

--  DataFrameSpace

data DataFrameSpace sh = DataFrameSpace Image (Space sh) Attenuation
  deriving Show

--  Create the Cube

{-# INLINE mkCube' #-}
mkCube' :: Shape sh => [DataFrameSpace sh] -> IO (Cube sh)
mkCube' dfs = do
  let spaces = [fp | (DataFrameSpace _ (Space fp) _) <- dfs]
  withForeignPtrs spaces $ \pspaces ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    newCube =<< {-# SCC "hkl_binoculars_cube_new'" #-} c'hkl_binoculars_cube_new (toEnum nSpaces') spaces'

{-# INLINE addSpace #-}
addSpace :: Shape sh => DataFrameSpace sh -> Cube sh -> IO (Cube sh)
addSpace df EmptyCube = mkCube' [df]
addSpace (DataFrameSpace _ (Space fs) _) (Cube fp) =
  withForeignPtr fs $ \spacePtr ->
  withForeignPtr fp $ \cPtr -> do
  {-# SCC "hkl_binoculars_cube_add_space" #-} c'hkl_binoculars_cube_add_space cPtr spacePtr
  return $ Cube fp

newtype InputFn =  InputFn'List [ScanFilePath]

toList :: InputFn -> [ScanFilePath]
toList (InputFn'List fs)        = fs

instance Show InputFn where
  show (InputFn'List sn) = unlines $ map show sn

withCubeAccumulator :: Shape sh => Cube sh -> (IORef (Cube sh)  -> IO ()) -> IO (Cube sh)
withCubeAccumulator c f = bracket
  (newIORef =<< newCube =<< (case c of
                               EmptyCube -> c'hkl_binoculars_cube_new_empty
                               (Cube fp) -> withForeignPtr fp $ \p ->
                                 c'hkl_binoculars_cube_new_empty_from_cube p
                            )
  )
  pure
  (\r -> f r >> readIORef r)

-- Projections
