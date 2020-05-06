{-# LANGUAGE TemplateHaskell #-}
module PerAccountTransactions where

import Concordium.GlobalState.SQL.AccountTransactionIndex
import Concordium.GlobalState.SQL
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.Execution

import Control.Monad.Logger
import Control.Monad.Reader

import qualified Data.Aeson as AE
import Data.Aeson.TH
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.Persist.Pagination
import Data.Conduit
import Data.Conduit.Combinators as Conduit

type PageResult m = ReaderT SqlBackend m (Maybe (Page Entry (Key Entry)))

type AccountStream m = ConduitT () (Entity Entry) (ReaderT SqlBackend m) ()

pageAccount :: MonadIO m => AccountAddress -> PageResult m
pageAccount accountAddr = getPage [EntryAccount ==. ByteStringSerialized accountAddr]
                                  EntryId
                                  (PageSize 10)
                                  Descend
                                  (Range Nothing Nothing)

streamRawAccounts :: MonadIO m => AccountAddress -> AccountStream m
streamRawAccounts accountAddr =
  streamEntities [EntryAccount ==. ByteStringSerialized accountAddr]
                 EntryId
                 (PageSize 10)
                 Descend
                 (Range Nothing Nothing)

data PrettySummary =
  SpecialTransaction !SpecialTransactionOutcome
  | BlockTransaction !TransactionSummary
  deriving(Eq, Show)

$(deriveJSON defaultOptions{sumEncoding=TaggedObject{
                               tagFieldName = "kind",
                               contentsFieldName = "details"}
                           }
   ''PrettySummary
  )

data PrettyEntry = PrettyEntry{
  peAccount :: AccountAddress,
  peBlockHash :: BlockHash,
  peBlockHeight :: BlockHeight,
  peBlockTime :: Timestamp,
  peTransactionSummary :: PrettySummary
  } deriving(Eq, Show)

makePretty :: Entity Entry -> Either String PrettyEntry
makePretty eentry = do
  let Entry{..} = entityVal eentry
  let peAccount = unBSS entryAccount
  let peBlockHash = unBSS entryBlock
  let peBlockHeight = fromIntegral entryBlockHeight
  let peBlockTime = entryBlockTime
  case entryHash of
    Nothing -> do
      peTransactionSummary <- SpecialTransaction <$> AE.eitherDecodeStrict entrySummary
      return $ PrettyEntry{..}
    Just _txHash -> do
      peTransactionSummary <- BlockTransaction <$> AE.eitherDecodeStrict entrySummary
      return $ PrettyEntry{..}

streamAccounts :: (MonadIO m)
                  => AccountAddress -- ^Account address to query.
                  -> ConduitM () (Either String PrettyEntry) (ReaderT SqlBackend m) ()
streamAccounts addr = streamRawAccounts addr .| Conduit.map makePretty


-- |Stream transactions affecting a given account, starting from the most recent one.
-- Starts a new database connection. For more flexiblity use 'streamAccounts' directly.
processAccounts :: ConnectionString
                -- ^How to connect to the database.
                -> AccountAddress
                -- ^Which account to query.
                -> ConduitT (Either String PrettyEntry) Void (ReaderT SqlBackend (NoLoggingT IO)) a
                -- ^How to process the stream of outcomes.
                -> IO a
processAccounts connString addr c =
  runNoLoggingT $ withPostgresqlPool connString 5 $ runSqlPool (connect (streamAccounts addr) c)
