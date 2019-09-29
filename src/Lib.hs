
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import ClassyPrelude
import Turtle hiding (FilePath)
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Arrow ((***))
import System.Directory (doesPathExist, doesDirectoryExist)
import Data.Time.Clock (getCurrentTime)
import System.Process (callCommand)

data Files = Files 
  { filesList :: [FilePath] 
  , fileMap :: (M.Map FilePath FilePath)
  }
  deriving (Eq, Show)

type PathPair = (FilePath, FilePath)

-- TODO: find a graceful way to automatically resolve merge conflicts


pushpullMain :: IO ()
pushpullMain = do
  mappings <- makeMappings
  copyNewFiles mappings
  callCommand "git add ."
  gitCommitWithTime
  callCommand "git pull origin master"
  callCommand "git push"
  -- TODO: run install procedure
  return ()


gitCommitWithTime = do
  time <- getCurrentTime
  callCommand $ "git commit -m \"automated commit made by pushpull at " <> show time <> "\""

copyNewFiles :: Files -> IO ()
copyNewFiles (Files fs fm) = mconcat $ copy <$> fs
  where
    copy file = do
      let mextFile = M.lookup file fm
      case mextFile of
        Nothing -> return ()
        Just extFile -> copyFileOrDir file extFile


copyFileOrDir :: FilePath -> FilePath -> IO ()
copyFileOrDir dest origin = do
  isDir <- doesDirectoryExist origin 
  if isDir 
    then cpdir origin dest
    else cpfile origin dest
  where 
    cpdir = cptree `on` toTurtleFilePath
    cpfile = cp `on` toTurtleFilePath

makeMappings :: IO (Files)
makeMappings = do 

  mappingPathText <- lines <$> readTextFile "./mappings.txt" 
  let
    dlist = catMaybes $ parseFileLines <$> mappingPathText-- [(Text,Text)
  flist <- fmap catMaybes $ traverse checkNecessaryChanges dlist
  pure $ Files (fst <$> flist) (M.fromList flist)


  -- check each pair and see if they both exist, see if the external one is newer
checkNecessaryChanges :: PathPair -> IO (Maybe PathPair)
checkNecessaryChanges ps@(p1, p2) = do
  pathsExist <- and <$> traverse doesPathExist [p1, p2]
  if pathsExist
    then do
      newer <- checkNewer ps
      if newer then return (Just ps) else return Nothing
    else
      return Nothing

checkNewer :: PathPair -> IO Bool
checkNewer ps@(p1, p2) = do
  d1 <- getdate p1
  d2 <- getdate p2
  return $ d1 < d2
    where
      -- this is because turtle's FilePath type is not the normal String
      getdate = datefile . toTurtleFilePath 

toTurtleFilePath = fromText . T.pack 

parseFileLines :: Text -> Maybe PathPair 
parseFileLines t
  | not $ ':' `elem` t  = Nothing
  | otherwise = Just . mkPathPair $ T.breakOn ":" t

mkPathPair = mkFilePath *** (mkFilePath . stripLeadingColon)

stripLeadingColon = dropWhile (== ':')
mkFilePath = T.unpack . T.strip

