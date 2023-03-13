{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Forms 
    ( FileFormInfo
    , fileFormInfoVisible
    , fileFormInfoValid
    , fileFormInfoOperation
    , fileFormInfoFilename
    , mkFileEntryForm
    , defaultFileFormInfo
    , extractFilenameFromForm
    , fileFormIsVisible
    , fileFormIsValid
    , fileFormOperation
    , toggleFileFormVisibility
    , setFileFormOperation
    , FileOperation(..)
    , JobFormInfo
    , jobFormInfoVisible
    , jobFormInfoValid
    , jobFormInfoJobName
    , jobFormInfoBusinessValue
    , jobFormInfoTimeCriticality
    , jobFormInfoRROE
    , jobFormInfoJobSize
    , mkJobEntryForm
    , defaultJobFormInfo
    , extractJobFromForm
    , jobFormIsVisible
    , jobFormIsValid
    , toggleJobFormVisibility
    ) where

import Brick
  ( (<+>)
  , fill
  , hLimit
  , padBottom
  , str
  , vLimit
  )
import Brick.Forms
  ( Form
  , newForm
  , editShowableField
  , editTextField
  , formState
  , (@@=)
  , updateFormState
  )
import Brick.Widgets.Core ( Padding( Pad ) )
import Lens.Micro ((&), (^.), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import qualified Data.Text as T
import Data.Functor ((<&>))
import ResourceNames
import Job 
    ( Job
    , createJob
    )

-- Job Forms
data JobFormInfo =
  JobFormInfo
    { _jobFormInfoJobName :: T.Text
    , _jobFormInfoBusinessValue :: Int
    , _jobFormInfoTimeCriticality :: Int
    , _jobFormInfoRROE :: Int
    , _jobFormInfoJobSize :: Int
    , _jobFormInfoVisible :: Bool
    , _jobFormInfoValid :: Bool
    } deriving (Show)

makeLenses ''JobFormInfo

defaultJobFormInfo :: JobFormInfo 
defaultJobFormInfo = 
  JobFormInfo
    { _jobFormInfoJobName = ""
    , _jobFormInfoBusinessValue = 0
    , _jobFormInfoTimeCriticality = 0
    , _jobFormInfoRROE = 0
    , _jobFormInfoJobSize = 0
    , _jobFormInfoVisible = False
    , _jobFormInfoValid = False
    }

jobFormIsVisible :: Form JobFormInfo e ResourceNames -> Bool
jobFormIsVisible = formState <&> (^. jobFormInfoVisible)

jobFormIsValid :: Form JobFormInfo e ResourceNames -> Bool
jobFormIsValid = formState <&> (^. jobFormInfoValid)

mkJobEntryForm :: JobFormInfo -> Form JobFormInfo e ResourceNames
mkJobEntryForm =
    let label s w = padBottom (Pad 1) $
                    vLimit 1 (hLimit 20 $ str s <+> fill ' ') <+> w
    in newForm [ label "Job Name" @@=
                   editTextField jobFormInfoJobName JobNameField (Just 1)
               , label "Business Value" @@=
                   editShowableField jobFormInfoBusinessValue
                    BusinessValueField
               , label "Time Criticality" @@=
                   editShowableField jobFormInfoTimeCriticality
                    TimeCriticalityField
               , label "RROE" @@=
                   editShowableField jobFormInfoRROE RROEField
               , label "Job Size" @@=
                   editShowableField jobFormInfoJobSize JobSizeField
               ]

extractJobFromForm :: Form JobFormInfo e ResourceNames -> Job
extractJobFromForm = 
  formState >>= (\x -> return $ createJob 
      (x ^. jobFormInfoJobName)
      (x ^. jobFormInfoBusinessValue)
      (x ^. jobFormInfoTimeCriticality)
      (x ^. jobFormInfoRROE)
      (x ^. jobFormInfoJobSize)
  )

toggleJobFormVisibility :: Form JobFormInfo e ResourceNames 
                     -> Form JobFormInfo e ResourceNames
toggleJobFormVisibility = 
    formState >>= (\x -> updateFormState (x & jobFormInfoVisible %~ not))

-- File Forms
data FileOperation = Load | Store deriving (Eq, Enum, Show)
data FileFormInfo =
  FileFormInfo
    { _FileFormInfoFilename :: T.Text
    , _FileFormInfoOperation :: FileOperation
    , _FileFormInfoVisible :: Bool
    , _FileFormInfoValid :: Bool
    } deriving (Show)

makeLenses ''FileFormInfo

defaultFileFormInfo :: FileFormInfo 
defaultFileFormInfo = 
  FileFormInfo
    { _FileFormInfoFilename = ""
    , _FileFormInfoOperation = Load
    , _FileFormInfoVisible = False
    , _FileFormInfoValid = False
    }

fileFormIsVisible :: Form FileFormInfo e ResourceNames -> Bool
fileFormIsVisible = formState <&> (^. fileFormInfoVisible)

fileFormIsValid :: Form FileFormInfo e ResourceNames -> Bool
fileFormIsValid = formState <&> (^. fileFormInfoValid)

fileFormOperation :: Form FileFormInfo e ResourceNames -> FileOperation
fileFormOperation = formState <&> (^. fileFormInfoOperation)

mkFileEntryForm :: FileFormInfo -> Form FileFormInfo e ResourceNames
mkFileEntryForm =
    let label s w = padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "File Name" @@=
                   editTextField fileFormInfoFilename FilenameField (Just 1)
               ]

extractFilenameFromForm :: Form FileFormInfo e ResourceNames -> T.Text
extractFilenameFromForm = 
  formState >>= (\x -> return $ x ^. fileFormInfoFilename)

toggleFileFormVisibility :: Form FileFormInfo e ResourceNames 
                     -> Form FileFormInfo e ResourceNames
toggleFileFormVisibility = 
    formState >>= (\x -> updateFormState (x & fileFormInfoVisible %~ not))

setFileFormOperation :: FileOperation -> Form FileFormInfo e ResourceNames 
                     -> Form FileFormInfo e ResourceNames
setFileFormOperation oper = 
    formState >>= (\x -> updateFormState (x & fileFormInfoOperation .~ oper))
