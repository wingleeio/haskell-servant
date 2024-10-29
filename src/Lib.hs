{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
  ( startApp,
    app,
    startDevelopmentServer
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import Data.Text.Read (decimal, signed)
import qualified Lib.Templates as Templates
import qualified Lib.State as AppState
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Web.Cookie
import System.Environment (lookupEnv)
import Development ( startDevelopmentServer )
import qualified Servant.OpenApi as SOA
import qualified Data.OpenApi as OA
import Data.Function
import Control.Lens
import Data.OpenApi (ToSchema(declareNamedSchema))

type API =
  Header "Cookie" Text :> Get '[HTML] H.Html
    :<|> "increment" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] H.Html)
    :<|> "decrement" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] H.Html)
    :<|> "documentation" :> Get '[HTML] H.Html
    :<|> "swagger.json" :> Get '[JSON] OA.OpenApi
    :<|> Raw

startApp :: IO ()
startApp = do
  env <- lookupEnv "ENV"
  let state = AppState.State { AppState.isDevelopment = env == Just "development" }
  run 8080 (app state)

app :: AppState.State -> Application
app state = serve api (hoistServer api (AppState.nt state) server)

api :: Proxy API
api = Proxy

server :: ServerT API AppState.AppM
server = homeApi :<|> incrementApi :<|> decrementApi :<|> Templates.scalar :<|> swaggerApi  :<|> serveDirectoryWebApp "public" 
  where
    homeApi = home
    incrementApi = increment
    decrementApi = decrement
    swaggerApi = pure swagger

parseCount :: Maybe BS.ByteString -> Integer
parseCount Nothing = 0
parseCount (Just countStr) =
  case signed decimal (TE.decodeUtf8 countStr) of
    Right (n, _) -> n
    Left _ -> 0

makeCookie :: (Show a) => Text -> a -> Text
makeCookie name value = pack $ unpack name ++ "=" ++ show value ++ "; Path=/"

home :: Maybe Text -> AppState.AppM H.Html
home Nothing = Templates.counter 0
home (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = parseCount (lookup "haskell.count" cookies)
  Templates.counter n

increment :: Maybe Text -> AppState.AppM(Headers '[Header "Set-Cookie" Text] H.Html)
increment Nothing = pure $ addHeader "haskell.count=1; Path=/" (Templates.count 1)
increment (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = parseCount (lookup "haskell.count" cookies)
  let newCount = n + 1
  pure $ addHeader (makeCookie "haskell.count" newCount) (Templates.count newCount)

decrement :: Maybe Text -> AppState.AppM(Headers '[Header "Set-Cookie" Text] H.Html)
decrement Nothing = pure $ addHeader "haskell.count=-1; Path=/" (Templates.count (-1))
decrement (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = parseCount (lookup "haskell.count" cookies)
  let newCount = n - 1
  pure $ addHeader (makeCookie "haskell.count" newCount) (Templates.count newCount)

swagger :: OA.OpenApi
swagger = SOA.toOpenApi api
  & OA.info.OA.title   .~ "Haskell HTMX API"
  & OA.info.OA.version .~ "1.0"
  & OA.info.OA.description ?~ "This is an API built with Haskell Servent and HTMX"
  & OA.info.OA.license ?~ ("MIT" & OA.url ?~ OA.URL "https://haskell-htmx.uwulabs.io")

instance OA.ToSchema H.Html where
    declareNamedSchema _ = do
        pure $ OA.NamedSchema Nothing $ mempty 
            & OA.type_ ?~ OA.OpenApiString
            & OA.description ?~ "HTML content"
            & OA.format ?~ "html"

instance OA.ToSchema OA.OpenApi where
    declareNamedSchema _ = pure $ OA.NamedSchema Nothing $ mempty
        & OA.type_ ?~ OA.OpenApiObject
        & OA.description ?~ "OpenAPI specification"