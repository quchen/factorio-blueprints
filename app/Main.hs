{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Control.Lens
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)

import JsonBlueprint



main :: IO ()
main = do
    rawInput <- BSL.getContents
    case upgradeEverything rawInput of
        Nothing -> error "Someting went wrong."
        Just result -> BSL8.putStrLn result
    -- case allEntityNames rawInput of
    --     Nothing -> error "Something went wrong."
    --     Just result -> traverse_ T.putStrLn result

-- API

allEntityNames :: BSL8.ByteString -> Maybe (Set Text)
allEntityNames = viewBlueprint (\v -> S.fromList (toListOf (entities . name) (Just v)))

upgradeEverything :: BSL8.ByteString -> Maybe BSL8.ByteString
upgradeEverything = modifyBlueprint (over (entities . name) (beltUpgrade . assemblyMachineUpgrade . inserterUpgrade))

beltUpgrade :: Text -> Text
beltUpgrade = \case
    "transport-belt"        -> "express-transport-belt"
    "fast-transport-belt"   -> "express-transport-belt"
    "splitter"              -> "express-splitter"
    "fast-splitter"         -> "express-splitter"
    "underground-belt"      -> "express-underground-belt"
    "fast-underground-belt" -> "express-underground-belt"
    other                   -> other

assemblyMachineUpgrade :: Text -> Text
assemblyMachineUpgrade = \case
    "assembling-machine-1" -> "assembling-machine-3"
    "assembling-machine-2" -> "assembling-machine-3"
    other                  -> other

inserterUpgrade :: Text -> Text
inserterUpgrade = \case
    "inserter" -> "fast-inserter"
    other      -> other

-- Optics

entities :: Traversal' (Maybe Value) (Maybe Value)
entities = key "blueprint" . key "entities" . traverseArray

name :: Traversal' (Maybe Value) Text
name = key "name" . asText . _Just
