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

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
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
import Network.Wai.Logger (withStdoutLogger)
import qualified Lib.Utils as Utils

type API =
  Header "Cookie" Text :> Get '[HTML] H.Html
    :<|> "increment" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] H.Html)
    :<|> "decrement" :> Header "Cookie" Text :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] H.Html)
    :<|> "documentation" :> Get '[HTML] H.Html
    :<|> "swagger.json" :> Get '[JSON] OA.OpenApi
    :<|> Raw

startApp :: IO ()
startApp = do
  withStdoutLogger $ \logger -> do
    env <- lookupEnv "ENV"
    let state = AppState.State { AppState.isDevelopment = env == Just "development" }
    let settings = setPort 8080 $ setLogger logger defaultSettings
    runSettings settings (app state)

app :: AppState.State -> Application
app state = serve api (hoistServer api (AppState.nt state) server)

api :: Proxy API
api = Proxy

server :: ServerT API AppState.AppM
server = home 
  :<|> increment 
  :<|> decrement 
  :<|> Templates.scalar 
  :<|> pure swagger  
  :<|> serveDirectoryWebApp "public" 



home :: Maybe Text -> AppState.AppM H.Html
home Nothing = Templates.counter 0
home (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = Utils.parseCount (lookup "haskell.count" cookies)
  Templates.counter n

increment :: Maybe Text -> AppState.AppM(Headers '[Header "Set-Cookie" Text] H.Html)
increment Nothing = pure $ addHeader "haskell.count=1; Path=/" (Templates.count 1)
increment (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = Utils.parseCount (lookup "haskell.count" cookies)
  let newCount = n + 1
  pure $ addHeader (Utils.makeCookie "haskell.count" newCount) (Templates.count newCount)

decrement :: Maybe Text -> AppState.AppM(Headers '[Header "Set-Cookie" Text] H.Html)
decrement Nothing = pure $ addHeader "haskell.count=-1; Path=/" (Templates.count (-1))
decrement (Just x) = do
  let cookies = parseCookies (TE.encodeUtf8 x)
  let n = Utils.parseCount (lookup "haskell.count" cookies)
  let newCount = n - 1
  pure $ addHeader (Utils.makeCookie "haskell.count" newCount) (Templates.count newCount)

swagger :: OA.OpenApi
swagger = SOA.toOpenApi api
  & OA.info.OA.title   .~ "Haskell HTMX API"
  & OA.info.OA.version .~ "1.0"
  & OA.info.OA.description ?~ "This is an API built with Haskell Servent and HTMX"
  & OA.info.OA.license ?~ ("MIT" & OA.url ?~ OA.URL "https://haskell-htmx.uwulabs.io")

instance OA.ToSchema H.Html where
    declareNamedSchema _ = pure $ OA.NamedSchema Nothing $ mempty 
            & OA.type_ ?~ OA.OpenApiString
            & OA.description ?~ "HTML content"
            & OA.format ?~ "html"

instance OA.ToSchema OA.OpenApi where
    declareNamedSchema _ = pure $ OA.NamedSchema Nothing $ mempty
        & OA.type_ ?~ OA.OpenApiObject
        & OA.description ?~ "OpenAPI specification"