module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Config
import qualified Concordium.Types.Transactions       as Types
import qualified Concordium.Types.Execution          as Types
import           Concordium.Types                    as Types

import           Acorn.Core                          as Core

import           Data.Maybe
import           Data.List
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

helper :: AccountConfig -> Nonce -> Energy -> Types.TransactionExpiryTime -> Types.Payload -> Types.BareBlockItem
helper senderData thNonce thEnergyAmount thExpiry payload =
  let encPayload = Types.encodePayload payload
      header = Types.TransactionHeader{thPayloadSize = payloadSize encPayload, thSender = acAddress senderData, ..}
      -- sign with the necessary number of keys
      threshold = fromIntegral (acThreshold senderData)
      keys = take threshold . sortOn fst . Map.toList $ acKeys senderData
  in Types.NormalTransaction $ Types.signTransaction keys header encPayload

deployModuleWithKey ::
     AccountConfig
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> TransactionExpiryTime
  -> [Module UA]
  -> IO [(Types.BareBlockItem, Either String (), ModuleRef)]
deployModuleWithKey senderData back mnonce energy expiry amodules = withClient back comp
  where
    tx nonce mhash = (helper senderData nonce energy expiry (Types.DeployModule (moduleMap Map.! mhash)), mhash)

    moduleMap = Map.fromList . map (\m -> (Core.moduleHash m, m)) $ amodules

    -- try to topologically sort the modules.
    processedModules = topoSort . Map.map (map Core.iModule . Core.mImports) $ moduleMap

    comp =
      case processedModules of
        Nothing -> fail "Circular dependencies. Will not deploy."
        Just orderedMods -> do
          bestBlockHash <- getBestBlockHash
          nonce <- flip fromMaybe mnonce . fst <$> getAccountNonceBestGuess (acAddress senderData)
          alreadyDeployed <- getModuleSet bestBlockHash
          -- TODO: technically the above two lines can fail if finalization
          -- happens to purge the currently best block. failure should be
          -- handled more gracefully by retrying
          let transactions = zipWith tx [nonce..] (filter (not . (`Set.member` alreadyDeployed)) (reverse orderedMods))
          forM transactions $ \(ctx, mhash) ->
                   sendTransactionToBaker ctx 100 >>= \case
                     Left err -> return (ctx, Left err, mhash)
                     Right False -> return (ctx, Left "Transaction rejected.", mhash)
                     Right True -> return (ctx, Right (), mhash)


initContractWithKey ::
     AccountConfig
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Types.TransactionExpiryTime
  -> Amount
  -> Core.ModuleRef
  -> Core.TyName
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (Types.BareBlockItem, Either String ())
initContractWithKey senderData back mnonce energy expiry initAmount homeModule contractName contractFlags = withClient back comp
  where
    tx nonce = helper senderData nonce energy expiry initContract

    initContract =
      Types.InitContract
        initAmount
        homeModule
        contractName
        contractFlags

    comp = do
      nonce <- flip fromMaybe mnonce . fst <$> (getAccountNonceBestGuess (acAddress senderData))
      let transaction = tx nonce
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, Right ())

updateContractWithKey ::
     AccountConfig
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Types.TransactionExpiryTime
  -> Amount
  -> ContractAddress
  -> Core.Expr Core.UA Core.ModuleName
  -> IO (Types.BareBlockItem, Either String ())
updateContractWithKey senderData back mnonce energy expiry transferAmount address msg = withClient back comp
  where
    tx nonce = helper senderData nonce energy expiry updateContract

    updateContract = Types.Update transferAmount address msg

    comp = do
      nonce <- flip fromMaybe mnonce . fst <$> (getAccountNonceBestGuess (acAddress senderData))
      let transaction = tx nonce
      sendTransactionToBaker transaction 100 >>= \case
        Left err -> return (transaction, Left err)
        Right False -> return (transaction, Left "Transaction rejected.")
        Right True -> return (transaction, Right ())
