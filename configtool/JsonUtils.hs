module JsonUtils
    ( parseJson
    , makeJson
    )
where

import Data.Aeson

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

parseJson :: FromJSON a => T.Text -> Maybe a
parseJson = decode . fromText

makeJson :: ToJSON a => a -> T.Text
makeJson = toText . encode . toJSON

fromText :: T.Text -> BSL.ByteString
fromText text = BSL.fromStrict (TEnc.encodeUtf8 text)

toText :: BSL.ByteString -> T.Text
toText bsl = TEnc.decodeUtf8 (BSL.toStrict bsl)
