{-# LANGUAGE LambdaCase #-}
module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Types.Transaction as CT
import           Concordium.ID.Types                 as IDTypes
import qualified Concordium.Types.Transactions       as Types
import qualified Concordium.Types.Execution          as Types
import           Concordium.Types                    as Types

import           Acorn.Core                          as Core

import           Data.Aeson                          (Value)
import           Data.Maybe
import           Data.Text (pack)
import           Prelude                             hiding (mod)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable
import Control.Monad


topoSort :: (Eq a, Hashable a) => Map.HashMap a [a] -> Maybe [a]
topoSort nodes = snd <$> foldM (go Set.empty) (Set.empty, []) (Map.keys nodes)
  where go temp (perm, l) node
            | node `Set.member` perm = Just (perm, l)
            | node `Set.member` temp = Nothing
            | otherwise = do
                let temp' = Set.insert node temp
                (perm', l') <- foldM (go temp') (perm, l) (Map.lookupDefault [] node nodes)
                Just (Set.insert node perm', node:l')

helper :: AccountAddress -> CT.KeyMap -> Nonce -> Energy -> Types.Payload -> Types.BareTransaction
helper sender keyMap nonce energy payload =
  let encPayload = Types.encodePayload payload
      header = Types.TransactionHeader {
        Types.thSender = sender,
        Types.thNonce = nonce,
        Types.thEnergyAmount = energy,
        Types.thPayloadSize = Types.payloadSize encPayload
        }
  in Types.signTransaction (Map.toList keyMap) header encPayload

deployModuleWithKey ::
     IDTypes.AccountAddress
  -> CT.KeyMap
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> [Module UA]
  -> IO [(Types.BareTransaction, Either String Value, ModuleRef)]
deployModuleWithKey sender keyMap back mnonce energy amodules = runInClient back comp
  where
    tx nonce mhash = (helper sender keyMap nonce energy (Types.DeployModule (moduleMap Map.! mhash)), mhash)

    moduleMap = Map.fromList . map (\m -> (Core.moduleHash m, m)) $ amodules

    -- try to topologically sort the modules.
    processedModules = topoSort . Map.map (map Core.iModule . Core.mImports) $ moduleMap

    comp = do
      case processedModules of
        Nothing -> fail "Circular dependencies. Will not deploy."
        Just orderedMods -> do
          bestBlockHash <- getBestBlockHash
          nonce <- flip fromMaybe mnonce <$> (getAccountNonce sender bestBlockHash)
          alreadyDeployed <- getModuleSet bestBlockHash
          -- TODO: technically the above two lines can fail if finalization
          -- happens to purge the currently best block. failure should be
          -- handled more gracefully by retrying
          let transactions = zipWith tx [nonce..] (filter (not . (`Set.member` alreadyDeployed)) (reverse orderedMods))
          mapM (\(ctx, mhash) -> do
                   txReturn <- hookTransaction (pack . show $ Types.transactionHash ctx)
                   sendTransactionToBaker ctx 100 >>= \case
                     Left err -> return (ctx, Left err, mhash)
                     Right False -> return (ctx, Left "Transaction rejected.", mhash)
                     Right True -> return (ctx, txReturn, mhash)
               ) transactions


initContractWithKey ::
     IDTypes.AccountAddress
  -> CT.KeyMap
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> Core.ModuleRef
  -> Core.TyName
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (Types.BareTransaction, Either String Value)
initContractWithKey sender keyMap back mnonce energy initAmount homeModule contractName contractFlags = runInClient back comp
  where
    tx nonce = helper sender keyMap nonce energy initContract

    initContract =
      Types.InitContract
        initAmount
        homeModule
        contractName
        contractFlags

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce sender =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- hookTransaction (pack . show $ Types.transactionHash transaction)
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, txReturn)

updateContractWithKey ::
     IDTypes.AccountAddress
  -> CT.KeyMap
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Amount
  -> ContractAddress
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (Types.BareTransaction, Either String Value)
updateContractWithKey sender keyMap back mnonce energy transferAmount address msg = runInClient back comp
  where
    tx nonce = helper sender keyMap nonce energy updateContract

    updateContract = Types.Update transferAmount address msg

    comp = do
      nonce <- flip fromMaybe mnonce <$> (getAccountNonce sender =<< getBestBlockHash)
      let transaction = tx nonce
      txReturn <- hookTransaction (pack . show $ Types.transactionHash transaction)
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, txReturn)
