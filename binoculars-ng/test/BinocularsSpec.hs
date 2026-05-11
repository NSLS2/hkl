{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                      (forM_)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Logger               (LoggingT)
import           Data.Attoparsec.Text               (Parser, parseOnly)
import           Data.Either                        (isRight)
import           Data.Ini.Config                    (parseIniFile)
import           Data.Ini.Config.Bidir              (FieldValue (..))
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Text                          (unpack)
import           Data.Tree                          (Tree (..))
import           Numeric.Interval                   (singleton, (...))
import           Test.Hspec
import           Test.Hspec.Attoparsec              (shouldFailOn, shouldParse)
import           Test.Hspec.Attoparsec.Source
import           Test.Hspec.QuickCheck              (prop)
import           Test.Inspection

import           Hkl.Binoculars
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.Chunk
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.Lattice
import           Hkl.Repa

import           Paths_hkl

import           Prelude                            hiding (readFile)

pro :: LoggingT IO ()
pro = process (Just "toto") (Just (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)])))

spec :: Spec
spec = do

  -----------
  -- Chunk --
  -----------

  describe "Chunk" $ do
         let fn = "toto" :: String
         let c = [Chunk fn 0 100]
         let n = 25 :: Int
         let e = [[Chunk fn 0 25], [Chunk fn 25 50], [Chunk fn 50 75], [Chunk fn 75 100]]
         it ("chunk : " <> show c <> " into chunks of lenght " <> show n) $ do
                       chunk n c `shouldBe` e

  -----------------
  -- ConfigRange --
  -----------------

  describe "ConfigRange" $ do

    let it'range t e = it ("parse a range: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'range "120 123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120,123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120,,,123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120-135 137-453" (ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
    it'range "120-135, 137-453" (ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
    it'range "0 1--1 0-0" (ConfigRange (InputRange (singleton 0) :| [InputRange (1...(-1)), InputRange (singleton 0)]))
    it'range "210-286" (ConfigRange (InputRange (210...286) :| []))

    prop "quickcheck" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: ConfigRange)

  --------------------
  -- DataSourcePath --
  --------------------

  describe "DataSourcePath Image" $ do
         let it'datasource'path'image (t, v)
                 = do it ("emit a DataSourcePath Image: " <> unpack t) $ do
                                      fvEmit fieldvalue v `shouldBe` t
                      it ("parse a DataSourcePath Image: " <> show v) $ do
                                      fvParse fieldvalue t `shouldBe` Right (v :: DSWrap_ DSImage DSPath)

         let tests = [ ( "[{\"contents\":[{\"detector\":\"ImXpadS140\"},1],\"tag\":\"DataSourcePath'Image'Dummy\"}]"
                       , [DataSourcePath'Image'Dummy defaultDetector 1.0])
                     , ( "[{\"contents\":[{\"detector\":\"ImXpadS140\"},100],\"tag\":\"DataSourcePath'Image'Dummy\"}]"
                       , [DataSourcePath'Image'Dummy defaultDetector 100])
                     ]

         mapM_ it'datasource'path'image tests

-- data instance DataSourcePath Image
--     = DataSourcePath'Image'Dummy (Detector Hkl DIM2) (DataSourcePath Attenuation) Double
--     | DataSourcePath'Image'Hdf5 (Detector Hkl DIM2) (Hdf5Path DIM3 Int32) -- TODO Int32 is wrong
--     | DataSourcePath'Image'Img (Detector Hkl DIM2) (DataSourcePath Attenuation) Text Scannumber
--     deriving (Eq, Generic, Show, FromJSON, ToJSON)


  ------------------
  -- MaskLocation --
  ------------------

  describe "MaskLocation" $ do

    let it'maskLocation t e =
            it ("parse a MaskLocation: " <> unpack t) $ do
              t ~> fieldParser `shouldParse` e

    it'maskLocation "mask.npy" (MaskLocation "mask.npy")
    it'maskLocation "mask_{scannumber:03d}.npy" (MaskLocation'Tmpl "mask_{scannumber:03d}.npy")
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy" (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation "mask.npy"))
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy|default  " (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation'Or (MaskLocation "mask.npy") (MaskLocation "default")))

  --------------
  -- Geometry --
  --------------

  describe "Geometry" $ do
    let title t = "Parse a geometry from ini string: " <> unpack t
    let checkParse t r = parseIniFile t iniParser'Maybe'Geometry `shouldBe` r

    let it'geometry'right t me
            = it (title t)
              (case me of
                Nothing -> checkParse t (Right me)
                Just e  -> liftIO $ withGeometry e (const $ checkParse t (Right me))
              )

    let it'geometry'left t err
            = it (title t)
              (const $ checkParse t (Left err))

    it'geometry'right "" Nothing

    it'geometry'right "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       (Just
        (Geometry'Custom
         ( Node (Axis "" NoTransformation Unit'NoUnit)
           [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "chi" (Rotation 1  0 0) Unit'Angle'Degree) [Node (Axis "phi" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
           , Node (Axis "tth" (Rotation 0 (-1) 0) Unit'Angle'Degree) []
           ]
         )
         Nothing
        )
       )

    it'geometry'right "[geometry]\ngeometry_sample=omega chi phi\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\n"
       (Just
        (Geometry'Custom
         ( Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "chi" (Rotation 1  0 0) Unit'Angle'Degree) [Node (Axis "phi" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
         )
         Nothing
        )
       )

    -- error handling

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\n\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Missing field \"axis_omega\" in section \"geometry\""

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Line 5, in section \"geometry\": rotation maformed expect: 'rotation <x> <y> <z>': Failed reading: takeWhile1"

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 toto\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Line 5, in section \"geometry\": Failed reading: unknown unit 'toto' only supported are 'ua' 'degree' 'millimeter'"

  -------------
  -- RLimits --
  -------------

  describe "Limits" $ do
    prop "quickcheck Limits2" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: RLimits DIM2)

    prop "quickcheck Limits3" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: RLimits DIM3)

  --------------------
  -- Transformation --
  --------------------

  describe "Transformation" $ do
         let it'transformation'good t e
                 = it ("parse a transformation: " <> unpack t)
                   (t ~> fieldParser `shouldParse` e)

         let it'transformation'error t
                 = it ("parse a malformed transformation: " <> unpack t)
                   ((fieldParser :: Parser Transformation) `shouldFailOn` t)

         it'transformation'good "no-transformation" NoTransformation
         it'transformation'good "rotation 1 0 0" (Rotation 1 0 0)
         it'transformation'good "rotation 1.0 0.0 0.0" (Rotation 1 0 0)
         it'transformation'good "translation -1.0 1.0 1.0" (Translation (-1) 1 1)

         it'transformation'error "rotation 1 0"

                                   -- (Left "Line 5, in section \"geometry\": rotation maformed expect: rotation <x> <y> <z>: Failed reading: takeWhile1")

  ----------
  -- Unit --
  ----------

  describe "Unit" $ do
    let it'unit t e = it ("parse an unit: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'unit "ua" Unit'NoUnit
    it'unit "degree" Unit'Angle'Degree
    it'unit "millimeter" Unit'Length'MilliMeter

  ------------------------
  -- Parse full configs --
  ------------------------

  describe "read and parse binoculars configuration" $ do
    it "deprecated inputype" $ do
      forM_ [ "data/test/config_ech6eiger.txt"
            , "data/test/config_sixs_ruche_parsing.ini"
            , "data/test/config_geometry_custom.ini"
            ] $ \f -> do
        content <- readConfig . Just =<< getDataFileName f
        capabilities <- getCapabilities
        let args = Args'QCustomProjection (Just $ ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
        let cfg = getConfig content args capabilities
        isRight cfg `shouldBe` True

  describe "inspection tests" $ do
    it "display inspection information of binoculars-ng" $ do
       let results = [ $(inspectTest $ hasNoTypeClasses 'pro)
                     , $(inspectTest $ hasNoGenerics 'pro)
                     ]
       if map (\case
                Success _ -> True
                Failure _ -> False) results == [True, True]
       then True `shouldBe` True
       else do
         mapM_ (\case
                 Success m -> putStrLn m
                 Failure m -> putStrLn m) results
         False `shouldBe` True
