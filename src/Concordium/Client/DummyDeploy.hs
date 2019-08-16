{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types                    as Types

import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import           Concordium.Crypto.SHA256            (hash)
import qualified Concordium.Crypto.SignatureScheme   as Sig
import           System.Random

import           Acorn.Core                          as Core

import qualified Data.ByteString                     as BS
import qualified Data.Serialize.Put                  as P
import           Data.Maybe
import           Prelude hiding(mod)

import qualified Concordium.ID.Account               as IDA

mateuszKP :: Sig.KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszKP' :: Sig.KeyPair
mateuszKP' = fst (randomKeyPair (mkStdGen 1))

blockPointer :: BlockHash
blockPointer = hash ""

deployModule ::
     Backend -> Maybe Nonce -> Energy -> [Module UA] -> IO [Transaction]
deployModule = deployModuleWithKey mateuszKP

deployModule' ::
     Backend -> Maybe Nonce -> Energy -> [Module UA] -> IO [Transaction]
deployModule' = deployModuleWithKey mateuszKP'

deployModuleWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> [Module UA]
  -> IO [Transaction]
deployModuleWithKey kp back mnonce amount amodules = runInClient back comp
  where
    tx nonce mod =
      Types.signTransaction
        kp
        (txHeader nonce)
        (Types.encodePayload (Types.DeployModule mod))
    txHeader nonce =
      Types.makeTransactionHeader
        Sig.Ed25519
        (Sig.verifyKey kp)
        nonce
        amount
        blockPointer

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transactions = zipWith tx [nonce..] amodules
      mapM_ (flip sendTransactionToBaker 100) transactions
      return transactions


initContractWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> Core.ModuleRef
  -> Core.TyName
  -> Core.Expr Core.UA Core.ModuleName
  -> IO Transaction
initContractWithKey kp back mnonce energy amount homeModule contractName contractFlags = runInClient back comp
  where
    tx nonce =
      Types.signTransaction
        kp
        (txHeader nonce)
        (Types.encodePayload initContract)

    txHeader nonce =
      Types.makeTransactionHeader
        Sig.Ed25519
        (Sig.verifyKey kp)
        nonce
        energy
        blockPointer

    initContract =
      Types.InitContract
        amount
        homeModule
        contractName
        contractFlags
        (BS.length $ P.runPut $ Core.putExpr contractFlags)

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transaction = tx nonce
      sendTransactionToBaker transaction 100
      return transaction
