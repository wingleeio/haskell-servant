module Lib.Utils (parseCount, makeCookie) where

import qualified Data.ByteString as BS
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import Data.Text.Read (decimal, signed)

parseCount :: Maybe BS.ByteString -> Integer
parseCount Nothing = 0
parseCount (Just countStr) =
  case signed decimal (TE.decodeUtf8 countStr) of
    Right (n, _) -> n
    Left _ -> 0

makeCookie :: (Show a) => Text -> a -> Text
makeCookie name value = pack $ unpack name ++ "=" ++ show value ++ "; Path=/"