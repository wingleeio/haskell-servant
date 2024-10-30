module Lib.State
  ( State (..),
    AppM,
    nt,
  )
where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.Wai (Request)
import Servant (Handler)

data State = State
  { isDevelopment :: Bool,
    request :: Request
  }

type AppM = ReaderT State Handler

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state