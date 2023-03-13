module App where

import Brick
  ( App(..)
  , attrMap
  , on
  , fg
  , neverShowCursor
  )
import Brick.Widgets.List as L
import Graphics.Vty (black, defAttr, green, red)

import Attrs (jobTablePanelAttrName, jobTablePanelItemAttrName)
import Draw (draw)
import EventHandles (handleEvent)
import ResourceNames
import View (View)

app :: App View e ResourceNames
app =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (jobTablePanelAttrName, fg red)
          , (jobTablePanelItemAttrName, red `on` black)
          , (L.listSelectedAttr, black `on` red)
          , (L.listAttr, red `on` black)
          ]
    }
