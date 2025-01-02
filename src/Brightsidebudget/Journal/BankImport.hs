{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.BankImport (
    importBankPosting,
    createNewTxns,
)
where

import Control.Monad.Except (ExceptT, liftEither)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Brightsidebudget.Utils (loadFileEnc)
import Brightsidebudget.Journal.Data (BankCsv(..), Posting(..), Amount, QName, Txn(..), Journal(..))
import Brightsidebudget.Journal.Calendar (dateAsDay)
import Brightsidebudget.Journal.Amount (doubleToAmount)

headerNames :: BankCsv -> [Text]
headerNames bankCsv = 
    let amntName = case bcsvAmountCol bankCsv of
            Left col -> [col]
            Right (inCol, outCol) -> [inCol, outCol]
        stmtDateName = case bcsvStmtDateCol bankCsv of
            Just stmtDate -> [stmtDate]
            Nothing -> []
    in bcsvDateCol bankCsv : amntName ++ bcsvStmtDescCols bankCsv ++ stmtDateName

importBankPosting :: BankCsv -> ExceptT Text IO [(Day, Posting)]
importBankPosting bankCsv = do
    fileContent <- loadFileEnc (bcsvFile bankCsv) (bcsvEncoding bankCsv)
    let goodContent = removeUnquotedDelimiter fileContent (bcsvRemoveDelimiterFrom bankCsv) (bcsvDelimiter bankCsv)
    let myOptions = Csv.defaultDecodeOptions {
            Csv.decDelimiter = fromIntegral (ord (bcsvDelimiter bankCsv))
            }
    let withHeader = if bcsvSkipFirstRow bankCsv then Csv.HasHeader else Csv.NoHeader
    records <- case Csv.decodeWith myOptions withHeader goodContent of
        Left err -> liftEither $ Left $ T.pack err
        Right xs -> pure xs
    let header = V.head records
    let body = V.tail records
    let myHeader = headerNames bankCsv
    indices <- liftEither $ getColumnIndices myHeader header
    let idxMap = HM.fromList $ zip myHeader indices
    liftEither $ traverse (createPosting bankCsv idxMap) (V.toList body)

removeUnquotedDelimiter :: BL.ByteString -> [Text] -> Char -> BL.ByteString
removeUnquotedDelimiter content [] _ = content
removeUnquotedDelimiter content badText delimter = 
    let textContent = TE.decodeUtf8 (BL.toStrict content)
        modifiedContent = foldl (\acc delimiter -> T.replace delimiter (T.filter (/= delimter) delimiter) acc) textContent badText
    in BL.fromStrict (TE.encodeUtf8 modifiedContent)

getColumnIndices :: [Text] -> Vector Text -> Either Text [Int]
getColumnIndices columnNames header =
    let headerMap = HM.fromList $ zip (V.toList header) [0..]
        findName :: Text -> Either Text Int
        findName name = case HM.lookup name headerMap of
            Just i -> Right i
            Nothing -> Left $ "Column " <> name <> " not found"
    in traverse findName columnNames

createPosting :: BankCsv -> HM.HashMap Text Int -> Vector Text -> Either Text (Day, Posting)
createPosting bankCsv idxMap row = do
    date <- getField (bcsvDateCol bankCsv) >>= dateAsDay
    amount <- parseAmount
    stmtDesc <- parseStmtDesc
    stmtDate <- parseStmtDate
    pure (date, Posting (bcsvQname bankCsv) amount "" stmtDesc stmtDate)

    where
        getField :: Text -> Either Text Text
        getField name = maybe (Left $ "Missing field: " <> name) Right $ row V.!? (idxMap HM.! name)

        parseAmount :: Either Text Amount
        parseAmount = 
            case bcsvAmountCol bankCsv of
                Left col -> (doubleToAmount . read . T.unpack) <$> getField col
                Right (inCol, outCol) -> do
                    inAmt <- (doubleToAmount . read . T.unpack) <$> getField inCol
                    outAmt <- (doubleToAmount . read . T.unpack) <$> getField outCol
                    return (inAmt - outAmt)

        parseStmtDesc :: Either Text Text
        parseStmtDesc =
            let descs = traverse (\col -> getField col) (bcsvStmtDescCols bankCsv)
            in T.intercalate " | " <$> descs
        
        parseStmtDate :: Either Text (Maybe Day)
        parseStmtDate = 
            case bcsvStmtDateCol bankCsv of
                Just stmtDate -> getField stmtDate >>= dateAsDay >>= (pure . Just)
                Nothing -> Right Nothing


createNewTxns :: Journal -> ((Day, Posting) -> Maybe Txn) -> [(Day, Posting)] -> Day -> [Txn]
createNewTxns j classifier ps onlyAfter = 
    let dedupDict = buildDedupDict j
        newPs = filter (\(d, _) -> d > onlyAfter) ps
        (newPs', _) = foldr (\p (acc, dict) -> 
            let key = (fst p, pAccount (snd p), pAmount (snd p), pStmtDesc (snd p))
            in case M.lookup key dict of
                Just count -> if count == 1
                              then (acc, M.delete key dict)
                              else (acc, M.insert key (count - 1) dict)
                Nothing -> (p : acc, dict)
            ) ([], dedupDict) newPs
        acceptedTxns = mapMaybe (\p -> classifier p) newPs'
        maxTxnId = maximum (map txnId (jTxns j))
        newTxns = zipWith (\i t -> t {txnId = i}) [maxTxnId + 1..] acceptedTxns
    in newTxns

buildDedupDict :: Journal -> M.Map (Day, QName, Amount, Text) Int
buildDedupDict j =
    let ps = concatMap (\t -> map (\p -> (txnDate t, p)) (txnPostings t)) (jTxns j)
        dedupDict = foldl (\acc (dt, p) -> M.insertWith (+) (dt, pAccount p, pAmount p, pStmtDesc p) 1 acc) M.empty ps
    in dedupDict