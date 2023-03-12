module MyLib (tui) where

import Brick(defaultMain)
import Control.Monad (void)

import View (defaultView)
import App (app)

tui :: IO ()
tui = void (defaultMain app defaultView)
