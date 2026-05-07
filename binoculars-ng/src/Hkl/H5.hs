{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}


{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.H5
    ( Dataset
    , File
    , H5
    , H5Path
    , HklH5Exception
    , checkNdims
    , closeDataset
    , closeFile
    , getArrayInBuffer
    , getPosition
    , getPositionNew
    , getUB
    , lenH5Dataspace
    , datasetShape
    , nxEntries
    , nxEntries'
    , openDataset'
    , openDatasetWithAttr
    , openFile'
    , openGroup'
    , setImage
    , withH5File
    -- new API
    , Hdf5
    , ToHdf5(..)
    , Hkl.H5.empty
    , hdf5
    , group
    , dataset
    , saveHdf5
    , Hdf5Path(..)
    , hdf5p
    , groupp
    , grouppat
    , datasetp
    , datasetpattr
    , withHdf5Path'
    , withHdf5Path
    )
    where

import           Bindings.HDF5.Attribute         (Attribute, closeAttribute,
                                                  doesAttributeExist,
                                                  openAttribute,
                                                  readAttributeStringASCII)
import           Bindings.HDF5.Core              (HSize (HSize),
                                                  IndexType (ByName),
                                                  IterOrder (Native), Location,
                                                  hSize, hid, indexTypeCode,
                                                  iterOrderCode)
import           Bindings.HDF5.Dataset           (Dataset, closeDataset,
                                                  createDataset,
                                                  getDatasetSpace, openDataset,
                                                  readDataset, readDatasetInto,
                                                  writeDataset)
import           Bindings.HDF5.Dataspace         (Dataspace,
                                                  SelectionOperator (Set),
                                                  closeDataspace,
                                                  createSimpleDataspace,
                                                  getSimpleDataspaceExtent,
                                                  getSimpleDataspaceExtentNDims,
                                                  getSimpleDataspaceExtentNPoints,
                                                  selectHyperslab, selectNone)
import           Bindings.HDF5.Datatype.Internal (NativeType, nativeTypeOf)
import           Bindings.HDF5.Error             (HDF5Exception)
import           Bindings.HDF5.File              (AccFlags (ReadOnly, Truncate),
                                                  File, closeFile, createFile,
                                                  openFile)
import           Bindings.HDF5.Group             (Group, closeGroup,
                                                  createGroup, openGroup)
import           Bindings.HDF5.Link              (visitLinks)
import           Bindings.HDF5.Object            (ObjectId, ObjectType (..),
                                                  closeObject, getObjectType,
                                                  openObject)
import           Bindings.HDF5.PropertyList.DAPL (DAPL)
import           Bindings.HDF5.PropertyList.GAPL (GAPL)
import           Bindings.HDF5.Raw               (H5L_info_t, HErr_t (HErr_t),
                                                  HId_t (HId_t), h5l_iterate)
import           Control.Exception               (bracket, throwIO, try)
import           Control.Monad.Extra             (fromMaybeM)
import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import           Data.ByteString.Char8           (ByteString, pack, packCString,
                                                  unpack)
import           Data.IORef                      (modifyIORef', newIORef,
                                                  readIORef)
import           Data.Vector.Storable            (Storable, Vector, freeze,
                                                  head, unsafeFromForeignPtr0)
import           Data.Vector.Storable.Mutable    (IOVector, new)
import           Data.Word                       (Word16)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CInt (CInt))
import           Foreign.Ptr                     (FunPtr, freeHaskellFunPtr)
import           Foreign.Ptr.Conventions         (In (In), InOut (InOut),
                                                  castWrappedPtr, withInOut_)
import           Foreign.StablePtr               (StablePtr, castPtrToStablePtr,
                                                  castStablePtrToPtr,
                                                  deRefStablePtr, freeStablePtr,
                                                  newStablePtr)
import           GHC.Generics                    (Generic)
import           Numeric.LinearAlgebra           (Matrix, reshape)
import           Test.QuickCheck                 (Arbitrary (..), oneof)
import           Text.Printf                     (printf)

import           Hkl.Detector
import           Hkl.Exception
import           Hkl.Orphan                      ()
import           Hkl.Repa

import           Prelude                         hiding (head)

-- {-# ANN module "HLint: ignore Use camelCase" #-}

data H5

type H5Path = String

checkNdims :: Dataset -> Int -> IO Bool
checkNdims d expected = do
  space_id <- getDatasetSpace d
  (CInt ndims) <- getSimpleDataspaceExtentNDims space_id
  return $ expected == fromEnum ndims

shapeAsCoordinateToHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
shapeAsCoordinateToHyperslab s = [(HSize (fromIntegral n), Nothing, HSize 1, Nothing) | n <- reverse . listOfShape $ s]

shapeAsRangeToHyperslab :: Shape sh => sh -> [(HSize, Maybe HSize, HSize, Maybe HSize)]
shapeAsRangeToHyperslab s =  [(0, Nothing, HSize (fromIntegral s'), Nothing) | s' <- reverse . listOfShape $ s]

createDataspaceFromShape :: Shape sh => sh -> IO Dataspace
createDataspaceFromShape sh = createSimpleDataspace [HSize (fromIntegral s) | s <- reverse . listOfShape $ sh]

castToVector :: (Shape sh, Storable e) => Array F sh e -> Vector e
castToVector arr = unsafeFromForeignPtr0 (toForeignPtr arr) (size . extent $ arr)

getArrayInBuffer :: (NativeType t, Shape sh1, Shape sh2, Show sh2) => IOVector t -> Detector a sh1 -> Dataset -> sh2 -> IO (IOVector t)
getArrayInBuffer arr det d i = withDataspace (getDatasetSpace d) $ \dataspace ->
  do let s = shape det
         h = shapeAsCoordinateToHyperslab i ++ shapeAsRangeToHyperslab s
     selectHyperslab dataspace Set h
     withDataspace (createDataspaceFromShape s) $ \memspace ->
       do readDatasetInto d (Just memspace) (Just dataspace) Nothing arr
          return arr

setImage :: Shape sh => Detector a sh -> Dataset -> Dataspace -> Int -> Array F sh Word16 -> IO ()
setImage det d dataspace n arr =  do
  selectNone dataspace
  selectHyperslab dataspace Set h
  withDataspace (createDataspaceFromShape s) $ \memspace ->
    writeDataset d (Just memspace) (Just dataspace) Nothing (castToVector arr)
    where
      s = shape det
      h = (HSize (fromIntegral n), Nothing,  HSize 1, Nothing) : shapeAsRangeToHyperslab s

saveRepa :: (Location t, NativeType e, Shape sh, Storable e) => t -> ByteString -> Array F sh e -> IO ()
saveRepa f path arr =
  withDataspace (createDataspaceFromShape (extent arr)) $ \dataspace ->
  withDataset (createDataset f path (nativeTypeOf (linearIndex arr 0)) dataspace Nothing Nothing Nothing) $ \dataset' ->
  writeDataset dataset' Nothing Nothing Nothing (castToVector arr)

getPosition' :: NativeType t => Dataset -> [(HSize, Maybe HSize, HSize, Maybe HSize)] -> IO (Vector t)
getPosition' dataset' h =
    withDataspace (getDatasetSpace dataset') $ \dataspace -> do
      selectHyperslab dataspace Set h
      withDataspace (createSimpleDataspace [HSize 1]) $ \memspace -> do
        data_out <- Data.Vector.Storable.Mutable.new 1
        readDatasetInto dataset' (Just memspace) (Just dataspace) Nothing data_out
        freeze data_out

getPositionNew :: (NativeType t, RealFloat t, Shape sh) => Dataset -> sh -> IO t
getPositionNew dataset' s = do
  v <- getPosition' dataset' (shapeAsCoordinateToHyperslab s)
  let v' = head v
  if isNaN v'
    then throwIO ContainNanValue
    else return v'

getPosition :: (NativeType t, RealFloat t) => Dataset -> Int -> IO t
getPosition dataset' n = do
  v <- getPosition' dataset' [(HSize (fromIntegral n), Nothing,  HSize 1, Nothing)]
  let v' = head v
  if isNaN v'
    then throwIO ContainNanValue
    else return v'

getUB :: Dataset -> IO (Matrix Double)
getUB dataset' = do
  v <- readDataset dataset' Nothing Nothing
  return $ reshape 3 v

--  File

withH5File' :: IO File -> (File -> IO r) -> IO r
withH5File' a = bracket a closeFile

withH5File :: FilePath -> (File -> IO r) -> IO r
withH5File fp = withH5File' (openFile' fp)

openFile' ∷ FilePath → IO File
openFile' f = do
  ef :: Either HDF5Exception File <- try $ openFile (pack f) [ReadOnly] Nothing
  case ef of
    Left _   -> throwIO $ CanNotOpenFile (pack f)
    Right f' -> pure f'

--  Group

openGroup' :: Location l =>l  -> ByteString  -> Maybe GAPL -> IO Group
openGroup' l n mgapl = do
  eg :: Either HDF5Exception Group <- try $ openGroup l n mgapl
  case eg of
    Left _  -> throwIO $ CanNotOpenGroup n
    Right g -> pure g

withGroup :: IO Group -> (Group -> IO r) -> IO r
withGroup a = bracket a closeGroup

withGroupAt :: Location l => l -> Int -> (Group -> IO r) -> IO r
withGroupAt l i f = do
  es <- nxEntries' l
  let n = es !! i
  eg :: Either HDF5Exception Group <- try $ openGroup l n Nothing
  case eg of
    Left _  -> throwIO $ CanNotOpenGroupAt n i
    Right g -> withGroup (pure g) f

--  Dataspace

-- check how to merge both methods

withDataspace :: IO Dataspace -> (Dataspace -> IO r) -> IO r
withDataspace a = bracket a closeDataspace

lenH5Dataspace :: Dataset -> IO (Maybe Int)
lenH5Dataspace d = withDataspace (getDatasetSpace d) len
  where
    len space_id = do
      (HSize n) <- getSimpleDataspaceExtentNPoints space_id
      return $ if n < 0 then Nothing else Just (fromIntegral n)

datasetShape :: Dataset -> IO ([HSize], [Maybe HSize])
datasetShape d = withDataspace (getDatasetSpace d) getSimpleDataspaceExtent

--  DataSet

openDataset' :: Location l => l -> ByteString -> Maybe DAPL -> IO Dataset
openDataset' l n mdapl = do
  ed :: Either HDF5Exception Dataset <- try $ openDataset l n mdapl
  case ed of
    Left _  -> throwIO $ CanNotOpenDataset n
    Right d -> pure d

withDataset :: IO Dataset -> (Dataset -> IO r) -> IO r
withDataset a = bracket a closeDataset

--  WIP until I have decided what is the right way to go

type H5Iterate a = HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t

foreign import ccall "wrapper" mkOp :: H5Iterate a -> IO (FunPtr (H5Iterate a))

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

withFunPtr :: H5Iterate a -> (FunPtr (H5Iterate a) -> IO b) -> IO b
withFunPtr f = bracket (mkOp f) freeHaskellFunPtr

nxEntries ∷ FilePath → IO [String]
nxEntries f = withH5File f $ \l → do
  es <- nxEntries' l
  return $ map unpack es

nxEntries' :: Location l => l -> IO [ByteString]
nxEntries' l = do
  state <- newIORef []
  _ <- withStablePtr state $ \statePtr -> do
    let opData = InOut $ castStablePtrToPtr statePtr
    let startIndex = Nothing
    let indexType = ByName
    let order = Native
    withFunPtr callback $ \iop ->
      withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
      h5l_iterate (hid l) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

  -- retrieve the final state
  readIORef state
    where
      callback :: H5Iterate a
      callback _ n _ (InOut dataptr) = do
        name <- packCString n
        stRef <- deRefStablePtr. castPtrToStablePtr . castWrappedPtr $ dataptr
        modifyIORef' stRef $ \st -> name : st
        return $ HErr_t 0

-- Attribute

withObject :: IO ObjectId -> (ObjectId -> IO r) -> IO r
withObject a = bracket a closeObject

withAttribute :: IO Attribute -> (Attribute -> IO r) -> IO r
withAttribute a = bracket a closeAttribute

openDatasetWithAttr :: Location l => l -> ByteString -> ByteString -> IO Dataset
openDatasetWithAttr loc attr value = do
  state <- newIORef (Nothing :: Maybe Dataset)
  visitLinks loc ByName Native $ \g n _ -> do
    withObject (openObject g n Nothing) $ \obj -> do
      t <-  getObjectType obj
      case t of
        DatasetObj -> do
          exist <- doesAttributeExist obj attr
          if exist
            then withAttribute (openAttribute obj attr) $ \a -> do
              c <- readAttributeStringASCII a
              if c == value
                then do
                  ds <- openDataset' g n Nothing
                  modifyIORef' state $ const (Just ds)
                  return $ HErr_t 1
                else return $ HErr_t 0
            else return $ HErr_t 0
        _          -> return $ HErr_t 0
  fromMaybeM (throwIO $ CanNotFindDatasetWithAttributContent attr value) (readIORef state)

--  Better API

data Hdf5M a
  = H5Empty
  | H5Root (Hdf5M a)
  | H5Group ByteString [Hdf5M a]
  | forall sh b. (NativeType b, Shape sh) => H5Dataset ByteString (Array F sh b)

type Hdf5 = Hdf5M ()

class ToHdf5 a where
  toHdf5 :: a -> Hdf5

hdf5 :: Hdf5 -> Hdf5
hdf5 = H5Root

empty :: Hdf5
empty = H5Empty

group :: ByteString -> [Hdf5] -> Hdf5
group = H5Group

dataset :: (NativeType b, Shape sh) => ByteString -> Array F sh b -> Hdf5
dataset = H5Dataset

saveHdf5 :: ToHdf5 a => FilePath -> a -> IO ()
saveHdf5 f a =  withH5File' (createFile (pack f) [Truncate] Nothing Nothing) $ \f' ->
  go f' (toHdf5 a)
  where
    go :: Location l => l -> Hdf5 -> IO ()
    go _ H5Empty = return ()
    go l (H5Root c) = go l c
    go l (H5Group n cs) = withGroup (createGroup l n Nothing Nothing Nothing ) $ \g -> mapM_ (go g) cs
    go l (H5Dataset n arr) = saveRepa l n arr


data Hdf5Path sh e
  = H5RootPath (Hdf5Path sh e)
  | H5GroupPath ByteString (Hdf5Path sh e)
  | H5GroupAtPath Int (Hdf5Path sh e)
  | H5DatasetPath ByteString
  | H5DatasetPathAttr (ByteString, ByteString)
    deriving (Eq, Generic, FromJSON, ToJSON)

instance Show (Hdf5Path sh e) where
    show (H5RootPath p)                   = "hdf5://" <> show p
    show (H5GroupPath name p)             = unpack name <> "/" <> show p
    show (H5GroupAtPath pos p)            = printf "@%d" pos <> "/" <> show p
    show (H5DatasetPath name)             = unpack name
    show (H5DatasetPathAttr (key, value)) = printf "%s=%s" (unpack key) (unpack value)

instance Arbitrary (Hdf5Path sh e) where
  {- HLINT ignore "Redundant <$>" -}
  arbitrary = oneof
    [ H5RootPath <$> arbitrary
    , H5GroupPath <$> pure "group" <*> arbitrary
    , H5GroupAtPath <$> arbitrary <*> arbitrary
    , H5DatasetPath <$> pure "dataset"
    , H5DatasetPathAttr <$> pure ("attibute", "value")
    ]

hdf5p :: Hdf5Path sh e -> Hdf5Path sh e
hdf5p = H5RootPath

groupp :: ByteString -> Hdf5Path sh e -> Hdf5Path sh e
groupp = H5GroupPath

grouppat :: Int -> Hdf5Path sh e -> Hdf5Path sh e
grouppat = H5GroupAtPath

datasetp :: ByteString -> Hdf5Path sh e
datasetp = H5DatasetPath

datasetpattr :: (ByteString, ByteString) -> Hdf5Path sh e
datasetpattr = H5DatasetPathAttr

withHdf5Path' :: Location l => l -> Hdf5Path sh e -> (Dataset -> IO r) -> IO r
withHdf5Path' loc (H5RootPath subpath) f = withHdf5Path' loc subpath f
withHdf5Path' loc (H5GroupPath n subpath) f = withGroup (openGroup' loc n Nothing) $ \g -> withHdf5Path' g subpath f
withHdf5Path' loc (H5GroupAtPath i subpath) f = withGroupAt loc i $ \g -> withHdf5Path' g subpath f
withHdf5Path' loc (H5DatasetPath n) f = withDataset (openDataset' loc n Nothing) f
withHdf5Path' loc (H5DatasetPathAttr (a, c)) f = withDataset (openDatasetWithAttr loc a c) f

withHdf5Path :: FilePath -> Hdf5Path sh e -> (Dataset -> IO r) -> IO r
withHdf5Path fn path f = withH5File fn $ \fn' -> withHdf5Path' fn' path f

-- TODO
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
