module Draw (draw) where

import Brick
  ( Widget
  , padAll
  , str
  , withBorderStyle
  )
import Brick.Forms
  ( Form
  , renderForm
  )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core (withAttr)
import Brick.Widgets.List as L
import Data.Maybe (catMaybes)
import qualified Data.Vector as Vec
import Lens.Micro ((^.))

import Attrs (jobTablePanelItemAttrName)
import Forms 
    ( FileFormInfo
    , fileFormIsVisible
    , fileFormOperation
    , JobFormInfo
    , jobFormIsVisible
    )
import JobTable (JobTable, name, jobIdx, jobs)
import ResourceNames
import View 
    ( View
    , currentFileForm
    , currentJobForm
    , currentTable
    )

drawFileFormInfo :: Form FileFormInfo e ResourceNames -> Widget ResourceNames
drawFileFormInfo j =
  withBorderStyle unicodeBold $
  borderWithLabel (str $ (show $ fileFormOperation j) ++ " a Job Table!") $
  hCenter $ padAll 1 $ withAttr jobTablePanelItemAttrName $ 
  renderForm j

drawJobFormInfo :: Form JobFormInfo e ResourceNames -> Widget ResourceNames
drawJobFormInfo j =
  withBorderStyle unicodeBold $
  borderWithLabel (str "create new job!") $
  hCenter $ padAll 1 $ withAttr jobTablePanelItemAttrName $ 
  renderForm j

drawJobTable :: JobTable -> Widget ResourceNames
drawJobTable j =
  withBorderStyle unicodeBold $
  borderWithLabel (str $ filter (/= '"') $ show (j ^. name)) $
  hCenter $ padAll 1 $ L.renderList listDrawElement True asBrickList
  where
    asBrickList = L.listMoveTo (j ^. jobIdx) 
        $ L.list ListItem (Vec.fromList (j ^. jobs)) 1
    listDrawElement _ a = hCenter $ str $ show a

draw :: View -> [Widget ResourceNames]
draw v = catMaybes [fileFormInfo, jobFormInfo, jobTable]
    where
    fileFormInfo = 
        if fileFormIsVisible (v ^. currentFileForm) then 
            Just $ center . padAll 1 $ drawFileFormInfo $ v ^. currentFileForm
        else 
            Nothing
    jobFormInfo = 
        if jobFormIsVisible (v ^. currentJobForm) then 
            Just $ center . padAll 1 $ drawJobFormInfo $ v ^. currentJobForm
        else 
            Nothing
    jobTable = Just $ center . padAll 1 $ drawJobTable $ v ^. currentTable
