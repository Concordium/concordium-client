{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.DummyDeploy where

import Data.String
import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types as Types
import           Control.Monad.Reader

import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import           Concordium.Crypto.SHA256            (hash)
import qualified Concordium.Crypto.SignatureScheme   as Sig
import           System.Random

import           Acorn.Core                          as Core

mateuszKP :: Sig.KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszKP' :: Sig.KeyPair
mateuszKP' = fst (randomKeyPair (mkStdGen 1))

blockPointer :: BlockHash
blockPointer = hash ""

deployModule ::
  Backend
  -> Maybe Nonce
  -> Energy
  -> Core.Module Core.UA
  -> IO Transaction
deployModule = deployModuleWithKey mateuszKP

deployModule' ::
  Backend
  -> Maybe Nonce
  -> Energy
  -> Core.Module Core.UA
  -> IO Transaction
deployModule' = deployModuleWithKey mateuszKP'

deployModuleWithKey ::
  Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Module UA
  -> IO Transaction
deployModuleWithKey kp back mnonce amount amodule = do
  runInClient back comp 
  where
    tx nonce =
      Types.signTransaction
        kp
        (txHeader nonce)
        (Types.encodePayload (Types.DeployModule amodule))
    txHeader nonce =
      Types.makeTransactionHeader
        Sig.Ed25519
        (Sig.verifyKey kp)
        nonce
        amount
        blockPointer

    comp = case mnonce of
      Nothing -> do
        cinfo <- getConsensusStatus
        case cinfo of
          Left err -> fail err
          Right [] -> fail "Should not happen."
          Right (v:_) -> do
            let blockHash = readBestBlock v
            ainfo <- getAccountInfo blockHash (fromString (show (Types.accountAddress (Sig.verifyKey kp) Sig.Ed25519)))
            case ainfo of
              Left err -> fail err
              Right [] -> fail "Should not happen."
              Right (aval:_) -> do
                let nonce = readAccountNonce aval
                let transaction = tx nonce
                sendTransactionToBaker (tx nonce) 100
                return transaction
      Just nonce -> do
        let transaction = tx nonce
        sendTransactionToBaker transaction 100
        return transaction
