{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Utils
    ( 
        loadFile,
        loadFileEnc,
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Text.IConv as IConv
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, throwError)

loadFile :: FilePath -> ExceptT Text IO ByteString
loadFile filePath = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then liftIO $ BL.readFile filePath
    else throwError $ T.concat(["The file ", T.pack filePath, " does not exist"])

loadFileEnc :: FilePath -> String -> ExceptT Text IO ByteString
loadFileEnc filePath enc = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then liftIO $ do
      rawContent <- BL.readFile filePath
      pure $ IConv.convert enc "UTF-8" rawContent
    else throwError $ T.concat(["The file ", T.pack filePath, " does not exist"])
