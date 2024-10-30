{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Counter (counter, CounterAction) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApiType (OpenApiString), ToParamSchema)
import Data.OpenApi.Lens
import Data.OpenApi.ParamSchema (ToParamSchema (toParamSchema))
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Lib.Attributes as Hx
import Lib.State (AppM, State (isDevelopment))
import qualified Lib.Utils as Utils
import Servant (FromHttpApiData (parseUrlPiece), Header, Headers, ToHttpApiData (toUrlPiece), addHeader, noHeader)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Cookie (parseCookies)

data CounterAction = Count | Increment | Decrement
    deriving (Show, Generic, Eq)

instance ToJSON CounterAction
instance FromJSON CounterAction

instance FromHttpApiData CounterAction where
    parseUrlPiece "count" = Right Count
    parseUrlPiece "increment" = Right Increment
    parseUrlPiece "decrement" = Right Decrement
    parseUrlPiece _ = Left "Invalid action"

instance ToHttpApiData CounterAction where
    toUrlPiece Count = "count"
    toUrlPiece Increment = "increment"
    toUrlPiece Decrement = "decrement"

instance ToParamSchema CounterAction where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["count", "increment", "decrement"]
            & description ?~ "Action to perform on the counter"

counter
    :: Maybe CounterAction
    -> Maybe Text
    -> AppM (Headers '[Header "Set-Cookie" Text, Header "HX-Trigger" Text] H.Html)
counter action cookie = case action of
    Just Count -> handleCountAction
    Just Increment -> handleIncrementAction (count + 1)
    Just Decrement -> handleDecrementAction (count - 1)
    Nothing -> renderPage
  where
    count = do
        let cookies = parseCookies (TE.encodeUtf8 $ fromMaybe "" cookie)
        Utils.parseCount (lookup "haskell.count" cookies)
    handleCountAction =
        pure
            . noHeader
            . noHeader
            $ H.toHtml (show count)
    handleIncrementAction newCount =
        pure
            . addHeader (pack $ "haskell.count=" ++ show newCount ++ "; Path=/")
            . addHeader "count-updated"
            $ H.toHtml (show newCount)
    handleDecrementAction newCount =
        pure
            . addHeader (pack $ "haskell.count=" ++ show newCount ++ "; Path=/")
            . addHeader "count-updated"
            $ H.toHtml (show newCount)
    renderPage = do
        html <- page count
        pure
            . noHeader
            . noHeader
            $ html

page :: Integer -> AppM H.Html
page count = do
    isDev <- asks isDevelopment
    pure $ H.docTypeHtml $ do
        H.head $ do
            H.title "Hello World"
            H.link ! A.rel "stylesheet" ! A.href "/styles.css"
            when isDev $ H.script ! A.src "http://localhost:8080/development.js" $ ""
        H.script
            ! A.src "https://unpkg.com/htmx.org@2.0.3"
            ! H.customAttribute "integrity" "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq"
            ! H.customAttribute "crossorigin" "anonymous"
            $ ""
        H.body $ do
            H.div ! A.class_ "flex gap-2 p-4" $ do
                H.button
                    ! Hx.post "/counter?htmx=decrement"
                    ! Hx.target "#count"
                    ! A.class_ "inline-flex items-center justify-center px-4 py-2 text-sm font-medium tracking-wide transition-colors duration-200 bg-white border rounded-md text-neutral-500 hover:text-neutral-700 border-neutral-200/70 hover:bg-neutral-100 active:bg-white focus:bg-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-neutral-200/60 focus:shadow-outline"
                    $ "-"
                H.div ! A.class_ "flex items-center justify-center min-w-12 rounded-md bg-slate-100" $ do
                    H.span ! A.id "count" $ H.toHtml (show count)
                H.button
                    ! Hx.post "/counter?htmx=increment"
                    ! Hx.target "#count"
                    ! A.class_ "inline-flex items-center justify-center px-4 py-2 text-sm font-medium tracking-wide transition-colors duration-200 bg-white border rounded-md text-neutral-500 hover:text-neutral-700 border-neutral-200/70 hover:bg-neutral-100 active:bg-white focus:bg-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-neutral-200/60 focus:shadow-outline"
                    $ "+"