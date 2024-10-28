{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import Data.Text.Read (decimal)
import Lib.Templates as Templates
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 as H
import Web.Cookie

type API =
  Header "Cookie" Text :> Get '[HTML] Html
    :<|> "increment" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)
    :<|> "decrement" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = homeApi :<|> incrementApi :<|> decrementApi
  where
    homeApi = Handler . return . home
    incrementApi = Handler . return . increment
    decrementApi = Handler . return . decrement

parseCount :: Maybe BS.ByteString -> Integer
parseCount Nothing = 0
parseCount (Just countStr) =
  case decimal (TE.decodeUtf8 countStr) of
    Right (n, _) -> n
    Left _ -> 0

makeCookie :: (Show a) => Text -> a -> Text
makeCookie name value = pack $ unpack name ++ "=" ++ show value ++ "; Path=/"

home :: Maybe Text -> Html
home Nothing = Templates.counter 0
home (Just x) =
  let cookies = parseCookies (TE.encodeUtf8 x)
      n = parseCount (lookup "haskell.count" cookies)
   in Templates.counter n

increment :: Maybe Text -> Headers '[Header "Set-Cookie" Text] Html
increment Nothing = addHeader "haskell.count=1; Path=/" (Templates.count 1)
increment (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
      n = parseCount (lookup "haskell.count" cookies)
      newCount = n + 1
  addHeader (makeCookie "haskell.count" newCount) (Templates.count newCount)

decrement :: Maybe Text -> Headers '[Header "Set-Cookie" Text] Html
decrement Nothing = addHeader "haskell.count=-1; Path=/" (Templates.count (-1))
decrement (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
      n = parseCount (lookup "haskell.count" cookies)
      newCount = n - 1
  addHeader (makeCookie "haskell.count" newCount) (Templates.count newCount)
