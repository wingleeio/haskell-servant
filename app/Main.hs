module Main (main, development) where

import Lib (startApp, startDevelopmentServer)

main :: IO ()
main = startApp

development :: IO ()
development = startDevelopmentServer