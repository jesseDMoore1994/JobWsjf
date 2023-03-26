{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module JobTable where

import Lens.Micro ((&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Functor ((<&>))
import Data.List
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as I
import qualified Data.Text.Lazy.Encoding as TLE

import Job (Job)

data JobTable = 
  JobTable
    { _name :: JobTableName
    , _jobs :: [Job]
    , _jobIdx :: Int
    } deriving (Generic, Show)

defaultTable :: JobTable
defaultTable =
    JobTable
      { _name = JobTableName "scratchpad.json"
      , _jobs = [] :: [Job] 
      , _jobIdx = 0
      }

instance FromJSON JobTable
instance ToJSON JobTable

newtype JobTableName =
  JobTableName T.Text
  deriving (Eq, Generic)

instance Show JobTableName where
    show (JobTableName j) = show j 

createJobTableName :: T.Text -> JobTableName
createJobTableName = JobTableName

instance FromJSON JobTableName
instance ToJSON JobTableName

makeLenses ''JobTable

writeJobTable :: JobTable -> IO ()
writeJobTable t = I.writeFile filename tableData
    where
      nameAsStr = filter (/= '"') $ show $ t ^. name
      filename = nameAsStr
      tableData = encodeToLazyText t

readJobTable :: JobTableName -> IO (Maybe JobTable)
readJobTable t = readFile filename <&>
    decode . TLE.encodeUtf8 . TL.pack
    where
      nameAsStr = filter (/= '"') $ show t
      filename = nameAsStr

addJobToJobTable :: JobTable -> Job -> JobTable
addJobToJobTable t j =
    JobTable
      { _name = t ^. name
      , _jobs = (sort . Data.List.reverse) $ (++) (t ^. jobs) [j]
      , _jobIdx = t ^. jobIdx
      }

removeJobFromJobTable :: JobTable -> Job -> JobTable
removeJobFromJobTable t j =
    JobTable
      { _name = t ^. name
      , _jobs = newJobs
      , _jobIdx = newJobIdx
      }
    where
      newJobs = filter ( /= j ) $ (sort . Data.List.reverse) (t ^. jobs)
      newJobIdx = if (t ^. jobIdx) > length newJobs - 1 then 
                    length newJobs - 1
                  else 
                    t ^. jobIdx

removeSelectedJob :: JobTable -> JobTable
removeSelectedJob t = removeJobFromJobTable t $ (t ^. jobs) !! (t ^. jobIdx)

data Direction = Up | Down deriving (Eq, Enum)
moveSelectedJob :: JobTable -> Direction -> JobTable
moveSelectedJob t d =
    JobTable
      { _name = t ^. name
      , _jobs = t ^. jobs
      , _jobIdx = nextIdx f (t ^. jobIdx)
      }
    where
      f = if d == Up then (+) (-1) else (+) 1
      maxIdx = length (t ^. jobs) - 1
      nextIdx move idx
        | move idx >= maxIdx = maxIdx
        | move idx <= 0 = 0
        | otherwise = move idx


setJobTableName :: JobTable -> JobTableName -> JobTable
setJobTableName j n = j & name .~ n

showJobTable :: JobTable -> String
showJobTable j = jobTable ++ jobList
    where
      jobTableName = j ^. name
      jobTable = "Job Table: " ++ (show jobTableName) ++ "\n"
      jobList = "Jobs: \n" ++ (unlines $ map (("    " ++) . show) (j ^. jobs))
