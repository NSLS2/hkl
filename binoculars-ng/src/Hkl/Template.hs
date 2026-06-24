{-# LANGUAGE OverloadedStrings #-}

module Hkl.Template
    ( Template(..)
    , applyTemplate
    ) where

import           Control.Monad (unless)
import           Data.Map      (Map, fromList, (!?))
import           Data.Text     (Text, break, breakOn, drop, pack, replace,
                                unpack)
import           Text.Printf   (printf)

import           Prelude       hiding (break, drop)

import           Hkl.Repa
import           Hkl.Types

type Tmpl = (Text, Text, String) -- ("{scannumber:02d"}, "scannumber", "02d")

-- Fonction pour extraire le nom et le format d'une variable
parseVarKey :: Text -> (Text, Text, String)
parseVarKey varText
    = let full = varText <> "}"
      in case Data.Text.break (== ':') varText of
           (name, fmt) -> (full, Data.Text.drop 1 name, unpack ("%" <> drop 1 fmt))
--           (name, _)   -> (full, name, "%d")  -- Format par défaut

-- Fonction pour extraire tous les segments entre { et }
findTemplateVars :: Text -> [Tmpl]
findTemplateVars t = case breakOn "{" t of
                       (_, "") -> []
                       (_, rest) -> case breakOn "}" rest of
                                          (varKey, after) -> parseVarKey varKey : findTemplateVars after

-- Fonction pour construire la map des valeurs à partir des variables trouvées
buildValuesMap :: Int -> [Int] -> [Tmpl] -> Either String (Map Text Int)
buildValuesMap sn shape tmpls = do
  let baseMap :: [(Text, Int)]
      baseMap = [ ("scannumber", sn) ]

  let indexMap :: [(Text, Int)]
      indexMap = zipWith (\i val -> (Data.Text.pack $ printf "index%d" i, val)) [0 :: Int ..] shape

  let allKeys :: [Text]
      allKeys = map fst baseMap ++ map fst indexMap

  let requiredKeys :: [Text]
      requiredKeys = Prelude.map (\(_, k, _) -> k) tmpls

  -- Vérifier que toutes les clés nécessaires sont présentes
  unless (all (`elem` allKeys) requiredKeys) $
         Left $ "Missing required variables in template" <> show allKeys <> show requiredKeys

  Right $ fromList $ baseMap ++ indexMap

-- Fonction pour remplacer les variables dans le template
applyTemplate :: Shape sh => Int -> sh -> Template -> Either String Text
applyTemplate sn sh (Template template) = do
  let shape = listOfShape sh
  let tmpls = findTemplateVars template
  if null tmpls
  then Right template
  else do
    values <- buildValuesMap sn shape tmpls
    let replacements = Prelude.map (\(full, name, fmt) -> do
                                      val <- values !? name
                                      let formatted = pack $ printf fmt val
                                      return (full, formatted)
                                   ) tmpls

    case sequence replacements of
      Just subs -> Right $ foldl (\t (old, new) -> replace old new t) template subs
      Nothing -> Left $ "Missing value for: " ++ show (1 :: Int)
