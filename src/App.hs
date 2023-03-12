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
          , (jobTablePanelItemAttrName, black `on` green)
          , (L.listSelectedAttr, black `on` green)
          , (L.listAttr, green `on` black)
          ]
    }
