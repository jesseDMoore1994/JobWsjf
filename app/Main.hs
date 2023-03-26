{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Tui as T
import Cli as C
import System.Console.CmdArgs

data JobWsjf =
    Tui
    { jobTableFile :: Maybe FilePath
    }
    | Add
    { jobName :: String
    , businessValue :: Int
    , timeCriticality :: Int
    , riskReduction :: Int
    , jobSize :: Int
    , jobTableFile :: Maybe FilePath
    }
    | Del
    { jobName :: String
    , jobTableFile :: Maybe FilePath
    }
    | Print
    { jobTableFile :: Maybe FilePath
    }
    deriving (Show, Data, Eq, Typeable)

jobTableArg :: Data val => val -> val
jobTableArg x = x
        &= help "Optional job table file (default to ./scratchpad.json)"
        &= typ "JSON_FILE"
        &= opt (Just "scratchpad.json")

tui :: JobWsjf
tui = Tui
    { jobTableFile = jobTableArg def
    } &= help "Launch a terminal user interface"

add :: JobWsjf
add = Add
    { jobName = def
        &= typ "JOB_NAME"
        &= argPos 0
    , businessValue = def
        &= typ "BUSINESS_VALUE"
        &= argPos 1
    , timeCriticality = def
        &= typ "TIME_CRITICALITY"
        &= argPos 2
    , riskReduction = def
        &= typ "RISK_REDUCTION"
        &= argPos 3
    , jobSize = def
        &= typ "JOB_SIZE"
        &= argPos 4
    , jobTableFile = jobTableArg def
    } &= help "Add a job non-interactively"

del :: JobWsjf
del = Del
    { jobName = def
        &= typ "JOB_NAME"
        &= argPos 0
    , jobTableFile = jobTableArg def
    } &= help "Delete a job non-interactively"

print :: JobWsjf
print = Print
    { jobTableFile = jobTableArg def
    } &= help "Print a job table non-interactively"

startTui :: Maybe FilePath -> IO ()
startTui tableFile = do
    case tableFile of
        Just f  -> tuiWithJob f
        Nothing -> tuiDefault

main :: IO ()
main = do
    parsed <- cmdArgs (modes [add, del, Main.print, Main.tui])
    case parsed of
        Add {
            jobTableFile=tableFile
          , jobName=jn
          , businessValue=bv
          , timeCriticality=tc
          , riskReduction=rroe
          , jobSize=js
        } -> C.addJob tableFile jn bv tc rroe js
        Del {
            jobTableFile=tableFile
          , jobName=jn
        } -> C.delJob tableFile jn
        Print {
            jobTableFile=tableFile
        } -> C.printJobTable tableFile
        Tui {jobTableFile=tableFile} -> startTui tableFile
