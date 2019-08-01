{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.DummyDeploy where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types                    as Types
import qualified Data.HashMap.Strict                 as HM
import           Data.String
import           Data.Word

import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import           Concordium.Crypto.SHA256            (hash)
import qualified Concordium.Crypto.SignatureScheme   as Sig
import           System.Random

import           Acorn.Core                          as Core
import           Data.Aeson

import           Data.Text

import qualified Concordium.ID.Account               as IDA

mateuszKP :: Sig.KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszKP' :: Sig.KeyPair
mateuszKP' = fst (randomKeyPair (mkStdGen 1))

blockPointer :: BlockHash
blockPointer = hash ""

deployModule ::
     Backend -> Maybe Nonce -> Energy -> Core.Module Core.UA -> IO Transaction
deployModule = deployModuleWithKey mateuszKP

deployModule' ::
     Backend -> Maybe Nonce -> Energy -> Core.Module Core.UA -> IO Transaction
deployModule' = deployModuleWithKey mateuszKP'

deployModuleWithKey ::
     Sig.KeyPair
  -> Backend
  -> Maybe Nonce
  -> Energy
  -> Module UA
  -> IO Transaction
deployModuleWithKey kp back mnonce amount amodule = runInClient back comp
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
    comp =
      case mnonce of
        Nothing -> do
          cinfo <- getConsensusStatus
          case cinfo of
            Left err -> fail err
            Right [] -> fail "Should not happen."
            Right (v:_) ->
              case readBestBlock v of
                Just (String blockhash) -> do
                  ainfo <-
                    getAccountInfo
                      blockhash
                      (fromString
                         (show
                            (IDA.accountAddress (Sig.verifyKey kp) Sig.Ed25519)))
                  case ainfo of
                    Left err -> fail err
                    Right [] -> fail "Should not happen."
                    Right (aval:_) ->
                      case readAccountNonce aval of
                        Just (Number nonce) -> do
                          let transaction =
                                tx .
                                Nonce .
                                (fromIntegral :: Integer -> Word64) . floor $
                                nonce
                          sendTransactionToBaker
                            (tx .
                             Nonce . (fromIntegral :: Integer -> Word64) . floor $
                             nonce)
                            100
                          return transaction
                        _ -> fail "Nonce not present in answer"
                _ -> fail "Best Block not present in answer"
        Just nonce -> do
          let transaction = tx nonce
          sendTransactionToBaker transaction 100
          return transaction

readBestBlock :: Value -> Maybe Value
readBestBlock (Object o) = HM.lookup (pack "bestBlock") o
readBestBlock _          = Nothing

readAccountNonce :: Value -> Maybe Value
readAccountNonce (Object o) = HM.lookup (pack "accountNonce") o
readAccountNonce _          = Nothing
