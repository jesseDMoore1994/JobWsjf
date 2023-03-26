{-# LANGUAGE TemplateHaskell #-}
module View 
    ( View
    , currentJobForm
    , currentFileForm
    , currentTable
    , defaultView
    , viewWithTable
    , clearJobForm
    , createJobAndAddItToTable
    , loadJobTable
    , storeJobTable
    , clearFileForm
    , setJobTableNameInView
    , noFormsOpenInView
    ) where

import Brick.Forms (Form)
import Data.Kind (Type)
import Lens.Micro ((&), (^.), (.~))
import Lens.Micro.TH (makeLenses)

import JobTable 
    ( JobTable
    , JobTableName(..)
    , defaultTable
    , addJobToJobTable
    , readJobTable
    , writeJobTable
    , setJobTableName
    )
import ResourceNames
import Forms 
    ( FileFormInfo
    , mkFileEntryForm
    , defaultFileFormInfo
    , extractFilenameFromForm
    , fileFormIsVisible
    , JobFormInfo
    , mkJobEntryForm
    , defaultJobFormInfo
    , extractJobFromForm
    , jobFormIsVisible
    )

data View = 
  View
    { _currentTable :: JobTable
    , _currentJobForm :: Form JobFormInfo Type ResourceNames
    , _currentFileForm :: Form FileFormInfo Type ResourceNames
    }

makeLenses ''View

defaultView :: View
defaultView = 
    View
      { _currentTable = defaultTable
      , _currentJobForm = mkJobEntryForm defaultJobFormInfo
      , _currentFileForm = mkFileEntryForm defaultFileFormInfo
      }

viewWithTable :: JobTable -> View
viewWithTable t =
    View
      { _currentTable = t
      , _currentJobForm = mkJobEntryForm defaultJobFormInfo
      , _currentFileForm = mkFileEntryForm defaultFileFormInfo
      }

clearJobForm :: View -> View
clearJobForm view = view & currentJobForm .~ mkJobEntryForm defaultJobFormInfo

createJobAndAddItToTable :: View -> View
createJobAndAddItToTable view = view & currentTable .~ newJobTable 
    where
        newJob = extractJobFromForm $ view ^. currentJobForm 
        newJobTable = addJobToJobTable (view ^. currentTable) newJob

loadJobTable :: View -> IO View
loadJobTable view = newViewIO
    where
        filename = extractFilenameFromForm $ view ^. currentFileForm 
        newJobTableIO = do
            x <- readJobTable (JobTableName filename)
            case x of
                Just t -> return t
                Nothing -> return $ view ^. currentTable
        newViewIO = do
            x <- newJobTableIO
            return $ view & currentTable .~ x

storeJobTable :: View -> IO ()
storeJobTable view = writeJobTable (view ^. currentTable)

clearFileForm :: View -> View
clearFileForm view = view & currentFileForm .~ mkFileEntryForm defaultFileFormInfo

setJobTableNameInView :: View -> View
setJobTableNameInView view = view & currentTable .~ newTable
    where
        filename = extractFilenameFromForm $ view ^. currentFileForm 
        newTable = setJobTableName (view ^. currentTable) (JobTableName filename)

noFormsOpenInView :: View -> Bool
noFormsOpenInView view = not $ 
    jobFormIsVisible (view ^. currentJobForm) 
    || fileFormIsVisible (view ^. currentFileForm)
