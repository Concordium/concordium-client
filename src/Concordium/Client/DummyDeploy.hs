{-# LANGUAGE LambdaCase #-}
module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.GRPC
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types                    as Types

import qualified Concordium.Crypto.SignatureScheme   as Sig

import           Acorn.Core                          as Core

import           Data.Aeson                          (Value)
import           Data.Maybe
import           Data.Text (pack)
import           Prelude                             hiding (mod)

import qualified Concordium.ID.Account               as IDA

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
  -> IO [(BareTransaction, Either String Value)]
deployModuleWithKey kp back mnonce energy amodules = runInClient back comp
  where
    tx nonce mod = helper kp nonce energy (Types.DeployModule mod)

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transactions = zipWith tx [nonce..] amodules
      mapM (\ctx -> do
                txReturn <- hookTransaction (pack . show $ Types.transactionHash ctx)
                sendTransactionToBaker ctx 100 >>= \case
                  Left err -> return (ctx, Left err)
                  Right False -> return (ctx, Left "Transaction rejected.")
                  Right True -> return (ctx, txReturn)
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
  -> IO (BareTransaction, Either String Value)
initContractWithKey kp back mnonce energy initAmount homeModule contractName contractFlags = runInClient back comp
  where
    tx nonce = helper kp nonce energy initContract

    initContract =
      Types.InitContract
        initAmount
        homeModule
        contractName
        contractFlags

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- hookTransaction (pack . show $ Types.transactionHash transaction)
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, txReturn)

updateContractWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> ContractAddress
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (BareTransaction, Either String Value)
updateContractWithKey kp back mnonce energy transferAmount address msg = runInClient back comp
  where
    tx nonce = helper kp nonce energy updateContract

    updateContract = Types.Update transferAmount address msg

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519) =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- hookTransaction (pack . show $ Types.transactionHash transaction)
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, txReturn)
