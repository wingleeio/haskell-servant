module Lib.State (
    State(..),
    AppM,
    nt
)where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Servant (Handler)

newtype State = State 
  { isDevelopment :: Bool
  }

type AppM = ReaderT State Handler

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state