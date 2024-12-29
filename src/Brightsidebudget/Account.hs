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
    csvAccountToAccount,
    accountToCsvAccount,
    validateAccount
)
where

import Data.Text (Text)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Brightsidebudget.Data (QName, CsvAccount(..), Account(..))

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

csvAccountToAccount :: CsvAccount -> Account
csvAccountToAccount (CsvAccount {csvaName = name, csvaNumber = num}) =
    Account {aName = textToQname name, aNumber = num}

accountToCsvAccount :: Account -> CsvAccount
accountToCsvAccount (Account {aName = name, aNumber = num}) =
    CsvAccount {csvaName = qnameToText name, csvaNumber = num}

validateAccount :: Account -> Either Text ()
validateAccount (Account {aName = name, aNumber = num}) = do
    validateQname name
    if num < 0
    then Left "Account number must be non-negative"
    else Right ()