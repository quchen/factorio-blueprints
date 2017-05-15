{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonBlueprint (
    decode,
    encode,
    modifyBlueprint,
    viewBlueprint,
) where



import qualified Codec.Compression.Zlib   as Z
import           Control.Exception
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as JsonPretty
import           Data.Bifunctor
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Lazy     as BSL
import           Data.Semigroup
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word
import           System.IO.Unsafe



decode :: BSL.ByteString -> Either Text (Word8, Json.Value)
decode rawInput = do
    (version, z64Blueprint) <- case BSL.uncons rawInput of
        Nothing -> Left "Empty input"
        Just x  -> Right x
    zBlueprint <- first T.pack (Base64.decode (BSL.toStrict z64Blueprint))
    rawJsonBlueprint <- (catchPure . BSL.toStrict . Z.decompress . BSL.fromStrict) zBlueprint
    case Json.eitherDecodeStrict' rawJsonBlueprint of
        Left err -> Left (T.pack err)
        Right r  -> Right (version, r)

-- For some reason zlib exports only a pure-but-throwing function, so
-- we hack our way around it here by resorting to black magic.
catchPure :: a -> Either Text a
catchPure x = unsafePerformIO (action `catch` handler)
  where
    action = fmap Right (evaluate x)
    handler (SomeException e) = (pure . Left . T.pack . show) e

encode :: (Word8, Json.Value) -> BSL.ByteString
encode (version, blueprintJson) = versionBs <> blueprintBs
  where
    versionBs = BSL.singleton version
    blueprintBs = (BSL.fromStrict . Base64.encode . BSL.toStrict . Z.compress . JsonPretty.encodePretty) blueprintJson

modifyBlueprint :: (Maybe Json.Value -> Maybe Json.Value) -> BSL.ByteString -> Maybe BSL.ByteString
modifyBlueprint f bs = case decode bs of
    Left _err -> Nothing
    Right (v, bpJson) -> case f (Just bpJson) of
        Nothing -> Nothing
        Just bpJson' -> Just (encode (v, bpJson'))

viewBlueprint :: (Json.Value -> a) -> BSL.ByteString -> Maybe a
viewBlueprint f bs = case decode bs of
    Left _err -> Nothing
    Right (_, bpJson) -> Just (f bpJson)
