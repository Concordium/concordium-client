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

import           Data.Maybe
import           Data.Aeson(Value)
import           Prelude hiding(mod)

import qualified Concordium.ID.Account               as IDA

mateuszKP :: Sig.KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszKP' :: Sig.KeyPair
mateuszKP' = fst (randomKeyPair (mkStdGen 1))

blockPointer :: BlockHash
blockPointer = hash ""

deployModule ::
     Backend -> Maybe Nonce -> Energy -> [Module UA] -> IO [(BareTransaction, Either String [Value])]
deployModule = deployModuleWithKey mateuszKP

deployModule' ::
     Backend -> Maybe Nonce -> Energy -> [Module UA] -> IO [(BareTransaction, Either String [Value])]
deployModule' = deployModuleWithKey mateuszKP'

helper :: Sig.KeyPair -> Nonce -> Energy -> Types.Payload -> BareTransaction
helper kp nonce energy spayload =
      let header = Types.makeTransactionHeader 
                         Sig.Ed25519
                         (Sig.verifyKey kp)
                         (Types.payloadSize encPayload)
                         nonce
                         energy
          encPayload = Types.encodePayload spayload
      in Types.signTransaction kp header encPayload

deployModuleWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> [Module UA]
  -> IO [(BareTransaction, Either String [Value])]
deployModuleWithKey kp back mnonce amount amodules = runInClient back comp
  where
    tx nonce mod = helper kp nonce amount (Types.DeployModule mod)

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transactions = zipWith tx [nonce..] amodules
      mapM (\ctx -> do
                txReturn <- sendHookToBaker (Types.transactionHash ctx)
                sendTransactionToBaker ctx 100
                return (ctx, txReturn)
            ) transactions


initContractWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> Core.ModuleRef
  -> Core.TyName
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (BareTransaction, Either String [Value])
initContractWithKey kp back mnonce energy amount homeModule contractName contractFlags = runInClient back comp
  where
    tx nonce = helper kp nonce energy initContract

    initContract =
      Types.InitContract
        amount
        homeModule
        contractName
        contractFlags

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- sendHookToBaker (Types.transactionHash transaction)
      sendTransactionToBaker transaction 100
      return (transaction, txReturn)


updateContractWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> ContractAddress
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (Transaction, Either String [Value])
updateContractWithKey kp back mnonce energy amount address message = runInClient back comp
  where
    tx nonce =
      Types.signTransaction
        kp
        (txHeader nonce)
        (Types.encodePayload updateContract)

    txHeader nonce =
      Types.makeTransactionHeader
        Sig.Ed25519
        (Sig.verifyKey kp)
        nonce
        energy
        blockPointer

    dummySizeThatWillBeDeprecated =
      1

    updateContract =
      Types.Update
        amount
        address
        message
        dummySizeThatWillBeDeprecated

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- sendHookToBaker (Types.trHash transaction)
      sendTransactionToBaker transaction 100
      return (transaction, txReturn)
