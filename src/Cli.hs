{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cli (addJob, delJob, printJobTable) where

import qualified Data.Text as T

import Job (createJob)
import JobTable
    ( addJobToJobTable
    , createJobTableName
    , defaultTable
    , readJobTable
    , removeJobFromJobTable
    , writeJobTable
    , showJobTable
    )

addJob :: Maybe FilePath -> String -> Int -> Int -> Int -> Int -> IO ()
addJob jt jn bv tc rroe js = do
    jobTable <- case jt of
        Just x  -> readJobTable (createJobTableName $ T.pack x)
        Nothing -> return $ Just defaultTable

    case jobTable of
        Just x -> writeJobTable $ addJobToJobTable x (createJob (T.pack jn) bv tc rroe js)
        Nothing -> return ()

delJob :: Maybe FilePath -> String -> IO ()
delJob jt jn = do
    jobTable <- case jt of
        Just x  -> readJobTable (createJobTableName $ T.pack x)
        Nothing -> return $ Nothing

    case jobTable of
        Just x -> writeJobTable $ removeJobFromJobTable x (createJob (T.pack jn) 0 0 0 0)
        Nothing -> return ()

printJobTable :: Maybe FilePath -> IO ()
printJobTable jt = do
    jobTable <- case jt of
        Just x  -> readJobTable (createJobTableName $ T.pack x)
        Nothing -> return $ Nothing

    case jobTable of
        Just x -> putStr $ showJobTable x
        Nothing -> return ()
