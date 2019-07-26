{-# LANGUAGE OverloadedStrings #-}
module Concordium.Client.DummyDeploy where

import Concordium.Client.Commands
import Concordium.Client.Runner
import Control.Monad.Reader
import Concordium.Types
import Concordium.GlobalState.Transactions
import qualified Concordium.Scheduler.Types as Types

import Concordium.Crypto.Ed25519Signature(randomKeyPair)
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.Crypto.SHA256(hash)
import System.Random

import Acorn.Core as Core

mateuszKP :: Sig.KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszKP' :: Sig.KeyPair
mateuszKP' = fst (randomKeyPair (mkStdGen 1))

blockPointer :: BlockHash
blockPointer = hash ""

deployModule :: MonadIO m => Backend -> Nonce -> Energy -> Core.Module Core.UA -> m Transaction
deployModule = deployModuleWithKey mateuszKP

deployModule' :: MonadIO m => Backend -> Nonce -> Energy -> Core.Module Core.UA -> m Transaction
deployModule' = deployModuleWithKey mateuszKP'

deployModuleWithKey :: MonadIO m => Sig.KeyPair -> Backend -> Nonce -> Energy -> Module UA -> m Transaction
deployModuleWithKey kp back nonce amount amodule = runInClient back (sendTransactionToBaker tx 100)
    where tx = Types.signTransaction kp txHeader (Types.encodePayload (Types.DeployModule amodule))
          txHeader = Types.makeTransactionHeader Sig.Ed25519 (Sig.verifyKey kp) nonce amount blockPointer
