module Attrs 
    ( jobTablePanelAttrName
    , jobTablePanelItemAttrName
    ) where

import Brick (AttrName, attrName)

-- Attributes
jobTablePanelAttrName :: AttrName
jobTablePanelAttrName = attrName "jobTablePanel"

jobTablePanelItemAttrName :: AttrName
jobTablePanelItemAttrName = attrName "jobTablePanelItem"
