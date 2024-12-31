{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Account
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
    saveAccounts,
    toShortNames
)
where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.List (isSuffixOf)
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Csv (decodeByName, encodeDefaultOrderedByName)
import Brightsidebudget.Journal.Data (QName, CsvAccount(..), Account(..))
import Brightsidebudget.Utils (loadFile)

validateQname :: QName -> Either Text ()
validateQname qn =
    let empty = T.null . T.strip
        has_empty = any empty qn
        has_colon = any (T.isInfixOf ":") qn
    in case (has_empty, has_colon) of
        (True, _) -> Left "QName cannot contain empty strings"
        (_, True) -> Left "QName cannot contain colons"
        _ -> Right ()

qnameToText :: QName -> Text
qnameToText = T.intercalate ":" . NE.toList

textToQname :: Text -> QName
textToQname = NE.fromList . T.splitOn ":"

basename :: QName -> Text
basename = NE.last

parent :: QName -> Maybe QName
parent q = 
    case NE.init q of
        [] -> Nothing
        xs -> Just $ NE.fromList xs

isParentOf :: QName -> QName -> Bool
isParentOf p c = NE.isPrefixOf (NE.toList p) c && NE.length p < NE.length c

isChildOf :: QName -> QName -> Bool
isChildOf = flip isParentOf

-- | Find the QName in the list whose suffix matches the given QName
shortNameOf :: QName -> [QName] -> Either Text QName
shortNameOf qn qns =
    let xs = filter (isSuffixOf (NE.toList qn)) (map NE.toList qns)
    in case xs of
        [] -> Left $ T.pack $ "no matching QName for " ++ show (qnameToText qn)
        [x] -> Right (NE.fromList x)
        _ -> 
            let xs' = map NE.fromList xs
            in Left $ T.pack $ "multiple matching QNames" ++ show (map qnameToText xs')

-- | Find the shortest suffix of each QName in the list that is unique
toShortNames :: [QName] -> [QName]
toShortNames qns = map findShortest qns
    -- Naive implementation, but we don't expect many QNames
    where
        findShortest :: QName -> QName
        findShortest qn = 
            case NE.filter isUniqueSuffix $ suffixes qn of
                [] -> error "no unique suffix found"
                (x : _) -> x

        isUniqueSuffix :: QName -> Bool
        isUniqueSuffix candidate =
            length (filter (myIsSuffixOf candidate) qns) == 1

        myIsSuffixOf :: QName -> QName -> Bool
        myIsSuffixOf x y = NE.toList x `isSuffixOf` NE.toList y

        suffixes :: QName -> NonEmpty QName
        suffixes = NE.reverse . NE.tails1

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