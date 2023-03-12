module EventHandles (handleEvent) where

import Brick
  ( BrickEvent(..)
  , EventM
  , get
  , gets
  , halt
  , put
  , zoom
  )
import Brick.Forms
  ( Form
  , formState
  , handleFormEvent
  , updateFormState
  )
import qualified Graphics.Vty as V
import Data.Kind (Type)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Lens.Micro ((&), (^.), (.~))

import Forms 
    ( FileFormInfo
    , fileFormInfoValid
    , fileFormInfoVisible
    , defaultFileFormInfo
    , fileFormIsValid
    , fileFormIsVisible
    , toggleFileFormVisibility
    , setFileFormOperation
    , fileFormOperation
    , FileOperation(..)
    , JobFormInfo
    , jobFormInfoValid
    , jobFormInfoVisible
    , defaultJobFormInfo
    , jobFormIsValid
    , jobFormIsVisible
    , toggleJobFormVisibility
    )
import JobTable 
    ( removeSelectedJob
    , moveSelectedJob
    , Direction(..)
    )
import ResourceNames
import View 
    ( View
    , currentFileForm
    , clearFileForm
    , currentJobForm
    , clearJobForm
    , createJobAndAddItToTable
    , setJobTableNameInView
    , currentTable
    , loadJobTable
    , storeJobTable
    , noFormsOpenInView
    )

handleJobFormEvent :: BrickEvent ResourceNames e 
                   -> EventM ResourceNames (Form JobFormInfo Type ResourceNames) ()
handleJobFormEvent (VtyEvent ev) = do
    case ev of
        V.EvKey V.KEsc [] -> get >>= put . updateFormState defaultJobFormInfo
        V.EvKey V.KEnter [] -> do
            x <- get
            xs <- gets formState
            put $ updateFormState 
                (xs & jobFormInfoValid .~ True & jobFormInfoVisible .~ False)
                x
        _ -> do
            handleFormEvent (VtyEvent ev)
handleJobFormEvent _ = return ()

handleFileFormEvent :: BrickEvent ResourceNames e 
                   -> EventM ResourceNames (Form FileFormInfo Type ResourceNames) ()
handleFileFormEvent (VtyEvent ev) = do
    case ev of
        V.EvKey V.KEsc [] -> get >>= put . updateFormState defaultFileFormInfo
        V.EvKey V.KEnter [] -> do
            x <- get
            xs <- gets formState
            put $ updateFormState 
                (xs & fileFormInfoValid .~ True & fileFormInfoVisible .~ False)
                x
        _ -> do
            handleFormEvent (VtyEvent ev)
handleFileFormEvent _ = return ()

handleTopLevelEvent :: View -> V.Event -> EventM n View ()
handleTopLevelEvent v ev = do
    case ev of
        V.EvKey V.KEsc [] -> halt

        V.EvKey (V.KChar 'n') [] -> do
            put $ v & currentJobForm .~ toggleJobFormVisibility 
                (v ^. currentJobForm)
            return ()

        V.EvKey (V.KChar 'o') [] -> do
            put $ v & currentFileForm .~ (setFileFormOperation Load . toggleFileFormVisibility)
                (v ^. currentFileForm)
            return ()
        V.EvKey (V.KChar 's') [] -> do
            put $ v & currentFileForm .~ (setFileFormOperation Store . toggleFileFormVisibility)
                (v ^. currentFileForm)
            return ()

        V.EvKey (V.KChar 'd') [] -> do
            put $ v & currentTable .~ removeSelectedJob
                (v ^. currentTable)
            return ()

        V.EvKey (V.KChar 'k') [] -> do
            put $ v & currentTable .~ moveSelectedJob
                (v ^. currentTable) Up
            return ()
        V.EvKey V.KUp [] -> do
            put $ v & currentTable .~ moveSelectedJob
                (v ^. currentTable) Up
            return ()

        V.EvKey (V.KChar 'j') [] -> do
            put $ v & currentTable .~ moveSelectedJob
                (v ^. currentTable) Down
            return ()
        V.EvKey V.KDown [] -> do
            put $ v & currentTable .~ moveSelectedJob
                (v ^. currentTable) Down
            return ()

        _ -> return ()

handleEvent :: BrickEvent ResourceNames e -> EventM ResourceNames View ()
handleEvent e@(VtyEvent ev) = do
    view <- get
    when (jobFormIsVisible (view ^. currentJobForm)) jobFormSubroutine
    when (fileFormIsVisible (view ^. currentFileForm)) fileFormSubroutine
    when (noFormsOpenInView view) $ handleTopLevelEvent view ev
    where
        jobFormSubroutine = do
          zoom currentJobForm (handleJobFormEvent e)
          postFormView <- get
          if jobFormIsValid (postFormView ^. currentJobForm) then 
              put $ (clearJobForm . createJobAndAddItToTable) postFormView
          else 
              put postFormView
        fileFormSubroutine = do
          zoom currentFileForm (handleFileFormEvent e)
          postFormView <- get
          if fileFormIsValid (postFormView ^. currentFileForm) then 
              if fileFormOperation (postFormView ^. currentFileForm) == Load then
                  liftIO (loadJobTable postFormView) >>= put . clearFileForm
              else do
                  let newTableView = setJobTableNameInView postFormView
                  liftIO (storeJobTable newTableView)
                  put $ clearFileForm newTableView 
          else 
              put postFormView
    
handleEvent _ = return ()
