{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Inertia (response, Response, initInertia) where

import Control.Lens ((?~))
import Control.Monad.Trans.Reader (asks)
import Data.Aeson (encode)
import Data.Aeson.Types
import Data.Function
import Data.Maybe (fromMaybe)
import qualified Data.OpenApi as OA
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Lib.State as AppState
import Lib.Templates (inertiaBaseTemplate, viteScript)
import Network.Wai (rawPathInfo)
import System.Directory (doesFileExist)
import Text.Blaze (ToMarkup)
import qualified Text.Blaze.Html5 as H

data Response
  = HtmlResponse H.Html
  | JsonResponse Value
  deriving (Generic)

instance ToJSON Response where
  toJSON (JsonResponse page) = page
  toJSON (HtmlResponse _) = error "Cannot render HTML as JSON"

instance ToMarkup Response where
  toMarkup (HtmlResponse html) = html
  toMarkup (JsonResponse page) = H.string $ show $ encode page

instance OA.ToSchema Response where
  declareNamedSchema _ =
    pure $
      OA.NamedSchema Nothing $
        mempty
          & OA.type_ ?~ OA.OpenApiString
          & OA.description ?~ "Inertia-compatible response"
          & OA.format ?~ "html/json"

response :: (ToJSON a) => Maybe Text -> Maybe Text -> Text -> a -> AppState.AppM Response
response header maybeVersion componentName serverProps = do
  req <- asks AppState.request
  template <- asks AppState.template

  let path = TE.decodeUtf8 $ rawPathInfo req

  let page =
        object
          [ "component" .= componentName
          , "props" .= serverProps
          , "url" .= path
          , "version" .= fromMaybe "0" maybeVersion
          ]

  case header of
    Just _ -> pure $ JsonResponse page
    _ -> pure $ HtmlResponse $ template page

initInertia :: IO (Value -> H.Html)
initInertia = do
  let viteHotFile = "./public/hot"

  viteDevelopmentServerIsRunning <- doesFileExist viteHotFile

  if viteDevelopmentServerIsRunning
    then do
      content <- readFile viteHotFile
      let script = viteScript $ content ++ "/resources/js/app.jsx"
      pure $ inertiaBaseTemplate script
    else do
      let viteManifestFile = "./public/build/.vite/manifest.json"
      _ <- readFile viteManifestFile
      pure $ error "Vite manifest file not found"
