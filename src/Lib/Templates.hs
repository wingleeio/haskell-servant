{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Templates
  ( counter,
    count,
    scalar
  )
where

import qualified Lib.Attributes as Hx
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Lib.State as AppState
import Control.Monad.Trans.Reader (asks)
import Control.Monad (when)

meta :: AppState.AppM H.Html
meta = do
  isDev <- asks AppState.isDevelopment
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

counter :: Integer -> AppState.AppM H.Html
counter n = do 
  metaHtml <- meta
  pure $ H.docTypeHtml $ do
    metaHtml
    H.body ! A.class_ "flex flex-col p-4 gap-4" $ do
      H.h1 ! A.class_ "testing" $ "Hello World"
      H.p "Welcome to Haskell Servant HTMX Example!"
      H.p "Hot reloading with Haskell and HTMX is now working!"
      H.div ! A.class_ "flex gap-2" $ do
        H.button
          ! Hx.post "/decrement"
          ! Hx.target "#count"
          ! Hx.swap "outerHTML"
          ! A.class_ "inline-flex items-center justify-center px-4 py-2 text-sm font-medium tracking-wide transition-colors duration-200 bg-white border rounded-md text-neutral-500 hover:text-neutral-700 border-neutral-200/70 hover:bg-neutral-100 active:bg-white focus:bg-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-neutral-200/60 focus:shadow-outline"
          $ "-"
        H.div ! A.class_ "flex items-center justify-center min-w-12 rounded-md bg-slate-100" $ do
          count n
        H.button
          ! Hx.post "/increment"
          ! Hx.target "#count"
          ! Hx.swap "outerHTML"
          ! A.class_ "inline-flex items-center justify-center px-4 py-2 text-sm font-medium tracking-wide transition-colors duration-200 bg-white border rounded-md text-neutral-500 hover:text-neutral-700 border-neutral-200/70 hover:bg-neutral-100 active:bg-white focus:bg-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-neutral-200/60 focus:shadow-outline"
          $ "+"
      
scalar :: AppState.AppM H.Html
scalar = do
  metaHtml <- meta
  pure $ H.docTypeHtml $ do
    metaHtml
    H.body $ do
      H.script ! A.id "api-reference" ! H.customAttribute "data-url" "/swagger.json" $ ""
      H.script ! A.src "https://cdn.jsdelivr.net/npm/@scalar/api-reference" $ ""

count :: Integer -> H.Html
count n = H.span ! A.id "count" $ H.toHtml (show n)
