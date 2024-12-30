{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Account
(
    validateQname,
    qnameToText,
    textToQname,
    basename,
    parent,
    isParentOf,
    isChildOf,
    shortNameOf,
    fromCsvAccount,
    toCsvAccount,
    validateAccount,
    validateAccounts,
    loadAccounts,
    saveAccounts
)
where

import Data.Text (Text)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Csv (decodeByName, encodeDefaultOrderedByName)
import Brightsidebudget.Data (QName, CsvAccount(..), Account(..))
import Brightsidebudget.Utils (loadFile)

validateQname :: QName -> Either Text ()
validateQname [] = Left "QName cannot be empty"
validateQname qn =
    let empty = T.null . T.strip
        has_empty = any empty qn
        has_colon = any (T.isInfixOf ":") qn
    in case (has_empty, has_colon) of
        (True, _) -> Left "QName cannot contain empty strings"
        (_, True) -> Left "QName cannot contain colons"
        _ -> Right ()

qnameToText :: QName -> Text
qnameToText = T.intercalate ":"

textToQname :: Text -> QName
textToQname = T.splitOn ":"

basename :: QName -> Text
basename = last

parent :: QName -> QName
parent = init

isParentOf :: QName -> QName -> Bool
isParentOf p c = isPrefixOf p c && length p < length c

isChildOf :: QName -> QName -> Bool
isChildOf = flip isParentOf

shortNameOf :: QName -> [QName] -> Either Text QName
shortNameOf qn qns =
    let xs = filter (isSuffixOf qn) qns
    in case xs of
        [] -> Left $ T.pack $ "no matching QName for " ++ show (qnameToText qn)
        [x] -> Right x
        _ -> Left $ T.pack $ "multiple matching QNames" ++ show (map qnameToText xs)

fromCsvAccount :: CsvAccount -> Account
fromCsvAccount (CsvAccount {csvaName = name, csvaNumber = num}) =
    Account {aName = textToQname name, aNumber = num}

toCsvAccount :: Account -> CsvAccount
toCsvAccount (Account {aName = name, aNumber = num}) =
    CsvAccount {csvaName = qnameToText name, csvaNumber = num}

validateAccount :: Account -> Either Text ()
validateAccount (Account {aName = name, aNumber = num}) = do
    validateQname name
    if num < 0
    then Left "Account number must be non-negative"
    else Right ()

validateAccounts :: [Account] -> Either Text ()
validateAccounts accs = do
    traverse_ validateAccount accs
    let xs = HS.fromList $ map aName accs
    if length xs == length accs
    then Right ()
    else Left "Duplicate account names"

loadCsvAccounts :: FilePath -> ExceptT Text IO [CsvAccount]
loadCsvAccounts filePath = do
    csvData <- loadFile filePath
    case decodeByName csvData of
        Left err -> throwError $ T.pack err
        Right (_, v) -> pure $ V.toList v

loadAccounts :: FilePath -> ExceptT Text IO [Account]
loadAccounts filePath = do
    csvAccounts <- loadCsvAccounts filePath
    let accs = map fromCsvAccount csvAccounts
    pure accs

saveAccounts :: FilePath -> [Account] -> IO ()
saveAccounts filePath accs = do
    let csvAccs = map toCsvAccount accs
    let csvData = encodeDefaultOrderedByName csvAccs
    BL.writeFile filePath csvData