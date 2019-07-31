{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types
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
     MonadIO m
  => Backend
  -> Nonce
  -> Energy
  -> Core.Module Core.UA
  -> m Transaction
deployModule = deployModuleWithKey mateuszKP

deployModule' ::
     MonadIO m
  => Backend
  -> Nonce
  -> Energy
  -> Core.Module Core.UA
  -> m Transaction
deployModule' = deployModuleWithKey mateuszKP'

deployModuleWithKey ::
     MonadIO m
  => Sig.KeyPair
  -> Backend
  -> Nonce
  -> Energy
  -> Module UA
  -> m Transaction
deployModuleWithKey kp back nonce amount amodule = do
  runInClient back (sendTransactionToBaker tx 100)
  return tx
  where
    tx =
      Types.signTransaction
        kp
        txHeader
        (Types.encodePayload (Types.DeployModule amodule))
    txHeader =
      Types.makeTransactionHeader
        Sig.Ed25519
        (Sig.verifyKey kp)
        nonce
        amount
        blockPointer
