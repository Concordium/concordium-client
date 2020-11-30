{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Info
  ( contractNameFromInitName
  , decodeContractStateUsingSchema
  , ContractInfo(..)
  , ContractState(..)
  ) where

import qualified Concordium.Types as T
import qualified Concordium.Client.Types.Contract.Schema as CS
import qualified Concordium.Client.Types.Contract.Parameter as CP
import qualified Concordium.Wasm as Wasm

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- |Tries to decode the `ContractState` using a `ModuleSchema`.
-- Fails if decoding fails, state is already decoded, or the contract isn't included in the `ModuleSchema`.
decodeContractStateUsingSchema :: ContractInfo -> CS.ModuleSchema -> Either String ContractInfo
decodeContractStateUsingSchema cInfo@ContractInfo{..} schema@CS.ModuleSchema{..} = case ciState of
  WithSchema _ _ -> Left "Contract state has already been decoded."
  JustBytes bytes -> case Map.lookup contractName contractSchemas of
    Nothing -> Left [i|A schema for the contract '#{contractName}' does not exist in the schema provided.|]
    Just contract -> (\decodedState -> cInfo {ciState = WithSchema schema decodedState}) <$> S.runGet (getModelAsJSON contract) bytes
  where contractName = contractNameFromInitName ciName

        getModelAsJSON :: CS.ContractSchema -> S.Get AE.Value
        getModelAsJSON CS.ContractSchema{..} = do
          state' <- case state of
              Nothing    -> return AE.Null
              Just typ -> CP.getJSONUsingSchema typ
          return $ AE.object ["state" .= state', "init" .= initSig, "receive" .= receiveSigs]

-- |Get a contract name from an InitName, i.e. extracting the text and removing the "init_" prefix.
-- If stripping the prefix fails, it simply returns the extracted text
-- (this should never happen, unless the InitName was incorrectly constructed).
contractNameFromInitName :: Wasm.InitName -> Text
contractNameFromInitName initName = case Text.stripPrefix "init_" initNameText of
  Nothing -> initNameText
  Just contrName -> contrName
  where initNameText = Wasm.initName initName

-- |This is returned by the node and specified in Concordium.Getters (from prototype repo).
-- Must stay in sync.
data ContractInfo
  -- |Info about a contract.
  = ContractInfo
    { -- |The contract balance.
      ciAmount :: !T.Amount
      -- |The owner of the contract.
    , ciOwner  :: !T.AccountAddress
      -- |The contract state.
    , ciState :: !ContractState
      -- |The receive functions/methods.
    , ciMethods :: ![Wasm.ReceiveName]
      -- |The contract name.
    , ciName :: !Wasm.InitName
      -- |The corresponding source module.
    , ciSourceModule :: !T.ModuleRef }
  deriving (Eq, Show)

-- TODO: Use CS.ContractSchema to avoid having to lookup by contract name.
data ContractState =
    WithSchema !CS.ModuleSchema !AE.Value  -- ^ The module schema and the decoded contract state.
  | JustBytes !ByteString                  -- ^ The binary-encoded contract state.
  deriving (Eq, Show)

instance AE.FromJSON ContractInfo where
  parseJSON = AE.withObject "Info" $ \v -> do
    ciAmount       <- v .: "amount"
    ciOwner        <- v .: "owner"
    ciState        <- case HM.lookup "model" v of
      Just (AE.String s) -> JustBytes <$> decodeBase16 s
      Just x -> fail [i|Invalid Info, expected "model" field to be a String of base16 bytes, but got: #{x}|]
      Nothing -> fail [i|Invalid Info, missing "model" field.|]
    ciMethods      <- v .: "methods"
    ciName         <- v .: "name"
    ciSourceModule <- v .: "sourceModule"
    return ContractInfo{..}
    where decodeBase16 xs =
            let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs in
              if BS.null remaining
              then return parsed
              else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]
