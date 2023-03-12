{-# LANGUAGE DeriveGeneric #-}
module Job (createJob, Job) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

-- Job Definitions
data Job = 
  Job
    { _jobName :: JobName
    , _businessValue :: BusinessValue
    , _timeCriticality :: TimeCriticality
    , _rroe :: RROE
    , _costOfDelay :: CostOfDelay
    , _jobSize :: JobSize
    , _wsjf :: WSJF
    } deriving (Generic)

instance Eq Job where
    (==) a b = (==) (_jobName a) (_jobName b)

instance Ord Job where
    compare a b = compare (_wsjf a) (_wsjf b)

instance Show Job where
    show j = show (_jobName j) ++ " " ++ show (_wsjf j)

instance FromJSON Job
instance ToJSON Job

newtype JobName =
  JobName T.Text
  deriving (Eq, Generic, Show)

instance FromJSON JobName
instance ToJSON JobName

newtype BusinessValue =
  BusinessValue Int
  deriving (Eq, Generic, Show)

instance FromJSON BusinessValue
instance ToJSON BusinessValue

newtype TimeCriticality =
  TimeCriticality Int
  deriving (Eq, Generic, Show)

instance FromJSON TimeCriticality
instance ToJSON TimeCriticality

newtype RROE =
  RROE Int
  deriving (Eq, Generic, Show)

instance FromJSON RROE
instance ToJSON RROE

newtype CostOfDelay =
  CostOfDelay Int
  deriving (Eq, Generic, Show)

instance FromJSON CostOfDelay
instance ToJSON CostOfDelay

newtype JobSize =
  JobSize Int
  deriving (Eq, Generic, Show)

instance FromJSON JobSize
instance ToJSON JobSize

newtype WSJF =
  WSJF Double
  deriving (Eq, Generic, Ord, Show)

instance FromJSON WSJF
instance ToJSON WSJF

createJob :: 
     T.Text 
  -> Int
  -> Int
  -> Int 
  -> Int
  -> Job
createJob ad' bv' tc' rroe' js' =
    Job
      { _jobName = JobName ad'
      , _businessValue = BusinessValue bv'
      , _timeCriticality = TimeCriticality tc'
      , _rroe = RROE rroe'
      , _costOfDelay = CostOfDelay cd' 
      , _jobSize = JobSize js'
      , _wsjf = WSJF wsjf'
      }
    where
      cd' = sum [bv', tc', rroe']
      wsjf' = fromIntegral cd' / fromIntegral js'
