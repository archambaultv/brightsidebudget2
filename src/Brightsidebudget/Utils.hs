{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Utils
    ( 
        loadFile,
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, throwError)

loadFile :: FilePath -> ExceptT Text IO ByteString
loadFile filePath = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then liftIO $ BL.readFile filePath
    else throwError $ T.concat(["The file ", T.pack filePath, " does not exist"])