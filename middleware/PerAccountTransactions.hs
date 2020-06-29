{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

type AccountStream m = ConduitT () (Entity Entry, Entity Summary) (ReaderT SqlBackend m) ()

pageAccount :: MonadIO m => AccountAddress -> PageResult m
pageAccount accountAddr = getPage [EntryAccount ==. ByteStringSerialized accountAddr]
                                  EntryId
                                  (PageSize 10)
                                  Descend
                                  (Range Nothing Nothing)

streamRawAccounts :: MonadIO m => AccountAddress -> AccountStream m
streamRawAccounts accountAddr =
  let
    -- entries will hold the stream of `Entry`s that correspond
    -- to the requested address.
    entries =
      streamEntities [EntryAccount ==. ByteStringSerialized accountAddr]
                     EntryId
                     (PageSize 10)
                     Descend
                     (Range Nothing Nothing)
    -- zipWithSummary gets the associated `Summary` for each `Entry` and zips it
    -- together with the `Entry`. We never delete elements from the database and the
    -- `entrySummary` field has a foreign key constraint and *MUST* exist on the
    -- `Summaries` table and be unique, so we can use `Prelude.head` safely.
    zipWithSummary v@(Entity _ Entry{..}) = ((v,) . Prelude.head <$> selectList [SummaryId ==. entrySummary] [])
  in
    -- we are chaining the stream of entries with the zipping with the associated summary
    -- to generate in the end a `Conduit () (Entity Entry, Entity Summary) ...`
    entries .| Conduit.mapM zipWithSummary

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

makePretty :: (Entity Entry, Entity Summary) -> Either String PrettyEntry
makePretty (Entity _ Entry{..}, Entity _ Summary{..}) = do
  let peAccount = unBSS entryAccount
      peBlockHash = unBSS summaryBlock
      peBlockHeight = summaryHeight
      peBlockTime = summaryTimestamp
  peTransactionSummary <- case AE.fromJSON summarySummary :: AE.Result TransactionSummary of
    AE.Error _ -> case AE.fromJSON summarySummary :: AE.Result SpecialTransactionOutcome of
      AE.Error e ->  Left e
      AE.Success v -> Right $ SpecialTransaction v
    AE.Success v -> Right $ BlockTransaction v
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
