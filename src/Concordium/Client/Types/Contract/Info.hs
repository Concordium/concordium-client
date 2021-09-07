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

import Data.Aeson ((.:))
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
  WithSchema{} -> Left "Contract state has already been decoded."
  JustBytes bytes -> case Map.lookup contractName contractSchemas of
    Nothing -> Left [i|A schema for the contract '#{contractName}' does not exist in the schema provided.|]
    Just CS.ContractSchema{..} -> case state of
      Nothing -> Right $ cInfo {ciState = WithSchema schema Nothing bytes}
      Just typ -> (\decodedState -> cInfo {ciState = WithSchema schema (Just decodedState) bytes}) <$> S.runGet (CP.getJSONUsingSchema typ) bytes
  where contractName = contractNameFromInitName ciName

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
      -- |The size of the contract state in bytes.
    , ciSize :: !Int
      -- |The receive functions/methods.
    , ciMethods :: ![Wasm.ReceiveName]
      -- |The contract name.
    , ciName :: !Wasm.InitName
      -- |The corresponding source module.
    , ciSourceModule :: !T.ModuleRef }
  deriving (Eq, Show)

-- TODO: Use CS.ContractSchema to avoid having to lookup by contract name.
data ContractState =
    WithSchema
    {
      wsModuleSchema :: !CS.ModuleSchema, -- ^ The module schema
      wsValue :: !(Maybe AE.Value), -- ^ The decoded contract state, if schema type found for the state.
      wsBytes :: !ByteString -- ^ The binary-encoded contract state.
    }   
    | JustBytes
    {
      jbBytes :: !ByteString -- ^ The binary-encoded contract state.
    } 
  deriving (Eq, Show)

instance AE.FromJSON ContractInfo where
  parseJSON = AE.withObject "Info" $ \v -> do
    ciAmount            <- v .: "amount"
    ciOwner             <- v .: "owner"
    (ciState, ciSize)   <- case HM.lookup "model" v of
      Just (AE.String s) -> do
        bs <- decodeBase16 s
        return (JustBytes bs, BS.length bs)
      Just x -> fail [i|Invalid Info, expected "model" field to be a String of base16 bytes, but got: #{x}|]
      Nothing -> fail [i|Invalid Info, missing "model" field.|]
    ciMethods           <- v .: "methods"
    ciName              <- v .: "name"
    ciSourceModule      <- v .: "sourceModule"
    return ContractInfo{..}
    where decodeBase16 xs =
            let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs in
              if BS.null remaining
              then return parsed
              else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]
