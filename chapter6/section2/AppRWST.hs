{-# LANGUAGE NamedFieldPuns #-}

module AppRWST where

import Control.Monad.IO.Class

import App

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config state = evalRWST app (initialEnv config) st
