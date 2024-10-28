{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Templates
  ( counter,
    count,
  )
where

import Lib.Attributes as Hx
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

counter :: Integer -> Html
counter n = docTypeHtml $ do
  H.head $ do
    H.title "Hello World"
    H.script
      ! A.src "https://unpkg.com/htmx.org@2.0.3"
      ! customAttribute "integrity" "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq"
      ! customAttribute "crossorigin" "anonymous"
      $ ""
  H.body $ do
    H.h1 ! A.class_ "testing" $ "Hello World"
    H.p "Welcome to the world of Haskell Web Development"
    H.div $ do
      H.button ! Hx.post "/decrement" ! Hx.target "#count" ! Hx.swap "outerHTML" $ "-"
      H.div $ do
        count n
      H.button ! Hx.post "/increment" ! Hx.target "#count" ! Hx.swap "outerHTML" $ "+"

count :: Integer -> Html
count n = H.span ! A.id "count" $ toHtml (show n)
