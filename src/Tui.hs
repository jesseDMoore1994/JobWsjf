{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tui (tuiDefault, tuiWithJob) where

import Brick(defaultMain)
import Control.Monad (void)
import qualified Data.Text as T

import JobTable (readJobTable, createJobTableName)
import View (defaultView, viewWithTable)
import App (app)

tuiDefault :: IO ()
tuiDefault = void (defaultMain app defaultView)

tuiWithJob :: FilePath -> IO ()
tuiWithJob f = do
    v <- view
    void (defaultMain app v)
  where
    jobTableName = createJobTableName (T.pack f)
    view = (readJobTable jobTableName) >>= \case
        Just j  -> return $ viewWithTable j
        Nothing -> return $ defaultView
