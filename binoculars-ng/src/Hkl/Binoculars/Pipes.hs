{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
    Copyright  : Copyright (C) 2014-2024, 2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Pipes
  ( Chunk(..)
  , ChunkP(..)
  , FramesP(..)
  , accumulateP
  , clippedChunk
  , progress
  , project
  , skipMalformed
  , tryYield
  , withProgressBar
  , withSpace
  , withScanFileP
  ) where

import           Control.Monad              (forM_, forever)
import           Control.Monad.Catch        (tryJust)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.IORef                 (IORef, readIORef)
import           Data.Maybe                 (fromMaybe)
import           Data.Typeable              (Typeable)
import           Path                       (toFilePath)
import           Pipes                      (Consumer, Pipe, Proxy, await,
                                             yield)
import           Pipes.Prelude              (mapM)
import           Pipes.Safe                 (MonadSafe, SomeException, bracket,
                                             catchP, displayException)
import           System.ProgressBar         (Progress (..), ProgressBar,
                                             Style (..), defStyle, elapsedTime,
                                             incProgress, newProgressBar,
                                             renderDuration, updateProgress)

import           Prelude                    hiding (filter)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Pipes
import           Hkl.Repa
import           Hkl.Types


-- class ChunkP

clippedChunk :: Maybe Int -> Maybe Int -> ScanFilePath -> DataSourceShape -> (Chunk Int ScanFilePath, DataSourceShape)
clippedChunk mSkipFirst mSkipLast sfp sh = (c, sh)
    where
      c = case sh of
            DataSourceShape'Range (Z :. f) (Z :. t) ->
                cclip (fromMaybe 0 mSkipFirst) (fromMaybe 0 mSkipLast) (Chunk sfp f (t - 1))
            DataSourceShape'Range'DIM2 (Z :. f) (Z :. t) _ ->
                cclip (fromMaybe 0 mSkipFirst) (fromMaybe 0 mSkipLast) (Chunk sfp f (t - 1))


class DataSource d => ChunkP d where
  chunkP :: ( MonadSafe m
           , Show (d DSPath)
           , Typeable d
           ) => Maybe Int -> Maybe Int -> [d DSPath] -> Pipe ScanFilePath ((Chunk Int ScanFilePath), DataSourceShape) m ()

  chunkP mSkipFirst mSkipLast ps =
      skipMalformed $ forever $ do
                 sfp <- await
                 withScanFileP sfp $ \f' ->
                     withDataSourcesP f' ps $ \_p p' ->
                         do sh <- ds'Shape p'
                            yield $ clippedChunk mSkipFirst mSkipLast sfp sh

-- class FramesP

iterate' :: ( DataSource d, MonadSafe m,
             Is1DStreamable (d DSAcq) r,
             Is2DStreamable (d DSAcq) r
           ) => [Int] -> d DSAcq -> Proxy x' x () r m ()
iterate' js p' = do
  sh <- ds'Shape p'
  case sh of
    DataSourceShape'Range _ _ -> forM_ js (tryYield . extract1DStreamValue p')
    DataSourceShape'Range'DIM2 _ _ (Z :. ks) -> forM_ [0..ks-1] (\k -> forM_ js
                                                                (\j -> do tryYield $ extract2DStreamValue p' (ix2 j k)))

class ChunkP d => FramesP d b where
    framesP :: ( Is1DStreamable (d DSAcq) b
              , Is2DStreamable (d DSAcq) b
              , MonadSafe m
              , Show (d DSPath)
              , Typeable d
              )
              => [d DSPath] -> Pipe (ScanFilePath, [Int]) b m ()
    framesP ps =
        skipMalformed $ forever $ do
                               (fp, js) <- await
                               withScanFileP fp $ \f ->
                                  withDataSourcesP f ps $ \_p p' -> iterate' js p'


-- Project

withSpace :: (MonadSafe m, Shape sh)
            => Detector a DIM2 -> Int -> (Space sh -> m r) -> m r
withSpace d n = bracket (liftIO $ newSpace d n) pure

project :: (MonadSafe m, Shape sh)
          => Detector a DIM2
        -> Int
        -> (Space sh -> b -> IO (DataFrameSpace sh))
        -> Pipe b (DataFrameSpace sh) m ()
project d n f = withSpace d n $ \s -> Pipes.Prelude.mapM (liftIO . f s)


skipMalformed :: MonadSafe m
              => Proxy a' a b' b m r
              -> Proxy a' a b' b m r
skipMalformed p = loop
  where
    loop = catchP p $ \e -> do
        liftIO $ Prelude.print $ displayException (e :: SomeException)
        loop

-- ProgressBar

withProgressBar :: Int -> (ProgressBar () -> IO ()) -> IO ()
withProgressBar ntot f = do
  pb <- newProgressBar defStyle{ stylePostfix=elapsedTime renderDuration } 10 (Progress 0 ntot ())
  f pb
  updateProgress pb $ \p@(Progress _ t _) -> p{progressDone=t}

--  Create the Cube

accumulateP :: (MonadIO m, Shape sh)
            => IORef (Cube sh) -> Consumer (DataFrameSpace sh) m ()
accumulateP ref =
    forever $ do s <- await
                 liftIO $ addSpace s =<< readIORef ref

progress :: MonadIO m => ProgressBar s -> Consumer a m ()
progress p = forever $ do
  _ <- await
  liftIO $ p `incProgress` 1

tryYield :: MonadSafe m
         => IO r -> Proxy x' x () r m ()
tryYield io = do
  edf <- liftIO $ tryJust selectHklBinocularsException io
  case edf of
    Left _   -> return ()
    Right df -> yield df
  where
    selectHklBinocularsException :: HklBinocularsException -> Maybe HklBinocularsException
    selectHklBinocularsException = Just


withScanFileP :: (MonadSafe m)
              => ScanFilePath
              -> (ScanFile File -> Proxy a' a b' b m r)
              -> Proxy a' a b' b m r
withScanFileP (ScanFilePath fp sn) g
  = withFileP (openFile' (toFilePath fp)) $ \f -> g (ScanFile f sn)
