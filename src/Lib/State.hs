module Lib.State (
  State (..),
  AppM,
  nt,
)
where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (Value)
import Network.Wai (Request)
import Servant (Handler)
import qualified Text.Blaze.Html5 as H

data State = State
  { isDevelopment :: Bool
  , request :: Request
  , template :: Value -> H.Html
  }

type AppM = ReaderT State Handler

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state
