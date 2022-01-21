{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Info
  ( contractNameFromInitName
  , addSchemaData
  , getContractName
  , hasReceiveMethod
  , constructModuleInspectInfo
  , ContractInfo(..)
  , ContractStateV0(..)
  , FunctionSchemaJSON(..)
  , SchemaDependentV0(..)
  , SchemaDependentV1(..)
  , ModuleInspectInfo(..)
  , ModuleInspectSigs(..)
  , ContractSigsV0(..)
  , ContractSigsV1(..)
  ) where

import qualified Concordium.Types as T
import qualified Concordium.Client.Types.Contract.Schema as CS
import qualified Concordium.Client.Types.Contract.Parameter as CP
import qualified Concordium.Wasm as Wasm

import Data.Word (Word32)
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
import qualified Concordium.Client.Config as Config

-- |Try to include extra information in the contract info from the module schema.
-- For V0 contracts:
--  - Try to decode the state, if the state schema is available.
--  - Include all available parameter schemas for receive methods.
-- For V1 contracts:
--  - Include all available function schemas for receive methods.
-- The included information is of type AE.Value, so that it can easily be printed.
--
-- Returns left if:
--  - Information has already been added (should never happen).
--  - The version of module schema and contract info does not match (should never happen).
--  - The module schema doesn't contain the given contract.
addSchemaData :: ContractInfo -> CS.ModuleSchema -> Either String ContractInfo
addSchemaData cInfo@ContractInfoV1{..} moduleSchema =
  case moduleSchema of
    CS.ModuleSchemaV0 _ -> Left "Internal error: Cannot use ModuleSchemaV0 with ContractInfoV1." -- Should never happen.
    CS.ModuleSchemaV1{..} ->
      case ciSchemaDependentV1 of
        WithSchemaV1{} -> Left "Internal error: Contract info has already been decoded." -- Should never happen.
        NoSchemaV1{..} -> case Map.lookup ciName ms1ContractSchemas of
          Nothing -> Left [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
          Just contrSchema ->
            let ws1Methods = map (addJSONFuncSchemaToMethod contrSchema) ns1Methods
                withSchema = WithSchemaV1{..}
            in Right (cInfo {ciSchemaDependentV1 = withSchema})
  where addJSONFuncSchemaToMethod :: CS.ContractSchemaV1 -> Text -> (Text, Maybe FunctionSchemaJSON)
        addJSONFuncSchemaToMethod contrSchema rcvName = let mFuncSchema = toFunctionSchemaJSON <$> CS.lookupFunctionSchema contrSchema (CS.ReceiveFuncName ciName rcvName)
                                            in (rcvName, mFuncSchema)
addSchemaData cInfo@ContractInfoV0{..} moduleSchema =
  case moduleSchema of
    CS.ModuleSchemaV1 _ -> Left "Internal error: Cannot use ModuleSchemaV1 with ContractInfoV0." -- Should never happen.
    CS.ModuleSchemaV0{..} ->
      case ciSchemaDependentV0 of
      WithSchemaV0{} -> Left "Internal error: Contract info has already been decoded." -- Should never happen.
      NoSchemaV0{..} -> case Map.lookup ciName ms0ContractSchemas of
        Nothing -> Left [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
        Just CS.ContractSchemaV0{..} ->
          let ws0State = case cs0State of -- Check the schema for state exists.
                Nothing -> Cs0Bytes ns0State
                Just schemaForState -> case S.runGet (CP.getJSONUsingSchema schemaForState) ns0State of
                  Left _ -> Cs0Bytes ns0State -- Could not parse the state using the schema. TODO: Ideally, log a warning.
                  Right jsonState -> Cs0JSON jsonState
              ws0Methods = map addJSONSchemaToMethod ns0Methods
              withSchema = WithSchemaV0{..}
          in Right (cInfo {ciSchemaDependentV0 = withSchema})
  where addJSONSchemaToMethod :: Text -> (Text, Maybe AE.Value)
        addJSONSchemaToMethod rcvName = let mJSONSchema = AE.toJSON <$> CS.lookupParameterSchema moduleSchema (CS.ReceiveFuncName ciName rcvName)
                                        in (rcvName, mJSONSchema)

-- |Get a contract name from an InitName, i.e. extracting the text and removing the "init_" prefix.
-- If stripping the prefix fails, it simply returns the extracted text
-- (this should never happen, unless the InitName was incorrectly constructed).
contractNameFromInitName :: Wasm.InitName -> Text
contractNameFromInitName initName = case Text.stripPrefix "init_" initNameText of
  Nothing -> initNameText
  Just contrName -> contrName
  where initNameText = Wasm.initName initName

-- |Get a method name from a Receive name, i.e. extracting the text and removing the "<contractName>." prefix.
-- If the receiveName does not have the prefix, it simply returns the extracted text.
methodNameFromReceiveName :: Wasm.ReceiveName -> Text
methodNameFromReceiveName rcvName = case Text.split (=='.') receiveNameText of
  [_contrName, methodName] -> methodName
  _ -> receiveNameText
  where receiveNameText = Wasm.receiveName rcvName

-- |Check whether the receive method exists in the contract.
hasReceiveMethod :: Text -> ContractInfo -> Bool
hasReceiveMethod rcvName cInfo = rcvName `elem` methods
  where methods = case cInfo of
          ContractInfoV0{..} -> case ciSchemaDependentV0 of
            NoSchemaV0{..} -> ns0Methods
            WithSchemaV0{..} -> map fst ws0Methods
          ContractInfoV1{..} -> case ciSchemaDependentV1 of
            NoSchemaV1{..} -> ns1Methods
            WithSchemaV1{..} -> map fst ws1Methods

-- |Get the contract name (without the 'init_' prefix).
getContractName :: ContractInfo -> Text
getContractName = \case
  ContractInfoV0{..} -> ciName
  ContractInfoV1{..} -> ciName

-- |This is returned by the node and specified in Concordium.Getters (from prototype repo).
-- Must stay in sync.
data ContractInfo
  -- |Info about a contract.
  = ContractInfoV0
    { -- |The contract balance.
      ciAmount :: !T.Amount
      -- |The owner of the contract.
    , ciOwner  :: !T.AccountAddress
      -- |The size of the contract state in bytes.
    , ciSize :: !Int
      -- |The corresponding source module.
    , ciSourceModule :: !T.ModuleRef
      -- |The contract name.
    , ciName :: !Text
      -- |TODO: Find better names.
    , ciSchemaDependentV0 :: !SchemaDependentV0 }
  | ContractInfoV1
    { -- |The contract balance.
      ciAmount :: !T.Amount
      -- |The owner of the contract.
    , ciOwner  :: !T.AccountAddress
      -- |The contract name.
    , ciName :: !Text
      -- |The corresponding source module.
    , ciSourceModule :: !T.ModuleRef
      -- |TODO: Find better names.
    , ciSchemaDependentV1 :: !SchemaDependentV1 }
  deriving (Eq, Show)

-- TODO: Find better names.
data SchemaDependentV0
  = NoSchemaV0
  { ns0State   :: !ByteString
  , ns0Methods :: ![Text]
  }
  | WithSchemaV0
  { ws0State   :: !ContractStateV0
  , ws0Methods :: ![(Text, Maybe AE.Value)]
  } deriving (Eq, Show)

data SchemaDependentV1
  = NoSchemaV1   { ns1Methods :: ![Text] }
  | WithSchemaV1 { ws1Methods :: ![(Text, Maybe FunctionSchemaJSON)]}
  deriving (Eq, Show)

data FunctionSchemaJSON
  = Parameter AE.Value
  | ReturnValue AE.Value
  | Both { fsjParameter :: AE.Value
         , fsjReturnValue :: AE.Value
         }
  deriving (Eq, Show)

data ContractStateV0
  = Cs0Bytes !ByteString
  | Cs0JSON !AE.Value
  deriving (Eq, Show)

-- |Convert 'FunctionSchema' to 'FunctionSchemaJSON' by serializing the schematype as JSON.
toFunctionSchemaJSON :: CS.FunctionSchema -> FunctionSchemaJSON
toFunctionSchemaJSON = \case
  CS.Parameter schemaType -> Parameter $ AE.toJSON schemaType
  CS.ReturnValue schemaType -> ReturnValue $ AE.toJSON schemaType
  CS.Both{..} -> Both { fsjParameter = AE.toJSON fsParameter
                      , fsjReturnValue = AE.toJSON fsReturnValue
                      }

instance AE.FromJSON ContractInfo where
  parseJSON = AE.withObject "Info" $ \v -> do
    ciAmount            <- v .: "amount"
    ciOwner             <- v .: "owner"
    ciName              <- contractNameFromInitName <$> v .: "name"
    ciSourceModule      <- v .: "sourceModule"
    methods             <- fmap methodNameFromReceiveName <$> v .: "methods"
    (v AE..:! "version" AE..!= (0 :: Word32)) >>= \case
      0 -> do
        (state, ciSize) <- case HM.lookup "model" v of
          Just (AE.String s) -> do
            let decodeBase16 xs =
                  let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs
                  in if BS.null remaining
                     then return parsed
                     else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]
            bs <- decodeBase16 s
            return (bs, BS.length bs)
          Just x -> fail [i|Invalid Info, expected "model" field to be a String of base16 bytes, but got: #{x}|]
          Nothing -> fail [i|Invalid Info, missing "model" field.|]
        let ciSchemaDependentV0 = NoSchemaV0 { ns0State = state, ns0Methods = methods}
        return ContractInfoV0{..}
      1 -> let ciSchemaDependentV1 = NoSchemaV1 { ns1Methods = methods }
           in return ContractInfoV1{..}
      n -> fail [i|Unsupported contract version #{n}.|]


-- |Construct module inspect info.
-- Works by:
--  - Creating ModuleInspectSigs from the list of exported function names.
--  - Inserting the signatures from the moduleSchema into the ModuleInspectSigs
--     - And collect the function names for /extraneous/ schemas,
--     - i.e. schemas for functions that are not exported in the module.
constructModuleInspectInfo :: Config.NamedModuleRef
                           -> Wasm.WasmVersion
                           -> Maybe CS.ModuleSchema
                           -> [Text] -- ^ Exported function names in module.
                           -> ModuleInspectInfo
constructModuleInspectInfo namedModRef wasmVersion moduleSchema exportedFuncNames =
  ModuleInspectInfo { miiNamedModRef = namedModRef
                    , miiWasmVersion = wasmVersion
                    , miiModuleInspectSigs = moduleInspectSigs
                    , miiExtraneousSchemas = extraneousSchemas
                    }
  where
    moduleInspectSigsFromExports :: ModuleInspectSigs
    moduleInspectSigsFromExports = case wasmVersion of
      Wasm.V0 ->
        let mkContrSchemaTuples x xs = case x of
              CS.InitFuncName contrName -> (contrName, ContractSigsV0 { csv0InitSig = Nothing
                                                                      , csv0ReceiveSigs = Map.empty
                                                                      }) : xs
              CS.ReceiveFuncName _ _ -> xs
            cSigsWithoutReceives = Map.fromList . foldr mkContrSchemaTuples [] $ funcNames

            insertReceiveNames :: [CS.FuncName] -> Map.Map Text ContractSigsV0 -> Map.Map Text ContractSigsV0
            insertReceiveNames [] sigMap = sigMap
            insertReceiveNames (CS.InitFuncName _:remaining) sigMap = insertReceiveNames remaining sigMap
            insertReceiveNames (CS.ReceiveFuncName cname fname:remaining) sigMap = case Map.lookup cname sigMap of
              Nothing -> insertReceiveNames remaining sigMap -- This should never happen, as we validate modules before they are put on chain.
              Just cs0@ContractSigsV0{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv0ReceiveSigs
                    sigMap' = Map.insert cname (cs0 {csv0ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis0ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV0{..}
      Wasm.V1 ->
        let mkContrSchemaTuples x xs = case x of
              CS.InitFuncName contrName -> (contrName, ContractSigsV1 { csv1InitSig = Nothing
                                                                      , csv1ReceiveSigs = Map.empty
                                                                      }) : xs
              CS.ReceiveFuncName _ _ -> xs
            cSigsWithoutReceives = Map.fromList . foldr mkContrSchemaTuples [] $ funcNames

            insertReceiveNames :: [CS.FuncName] -> Map.Map Text ContractSigsV1 -> Map.Map Text ContractSigsV1
            insertReceiveNames [] sigMap = sigMap
            insertReceiveNames (CS.InitFuncName _:remaining) sigMap = insertReceiveNames remaining sigMap
            insertReceiveNames (CS.ReceiveFuncName cname fname:remaining) sigMap = case Map.lookup cname sigMap of
              Nothing -> insertReceiveNames remaining sigMap -- This should never happen, as we validate modules before they are put on chain.
              Just cs1@ContractSigsV1{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv1ReceiveSigs
                    sigMap' = Map.insert cname (cs1 {csv1ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis1ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV1{..}

    (moduleInspectSigs, extraneousSchemas) = case (moduleSchema, moduleInspectSigsFromExports) of
      (Just CS.ModuleSchemaV0{..}, ModuleInspectSigsV0{..}) ->
        let
            addSchemas :: Map.Map Text ContractSigsV0 -> Map.Map Text CS.ContractSchemaV0 -> (Map.Map Text ContractSigsV0, [CS.FuncName])
            addSchemas mSigs mSchema = go mSigs [] (Map.toList mSchema)
              where
                    go :: Map.Map Text ContractSigsV0 -> [CS.FuncName] -> [(Text, CS.ContractSchemaV0)] -> (Map.Map Text ContractSigsV0, [CS.FuncName])
                    go sigMap errors [] = (sigMap, errors)
                    go sigMap errors ((cname, CS.ContractSchemaV0{..}):remaining) =
                      case Map.lookup cname sigMap of
                        Nothing -> let receiveErrors = map (CS.ReceiveFuncName cname) . Map.keys $ cs0ReceiveSigs
                                       errors' = CS.InitFuncName cname : receiveErrors ++ errors
                                   in go sigMap errors' remaining -- Schema has init signature for a contract not in the module.
                        Just ContractSigsV0{..} ->
                          let (updatedCsReceiveSigs, receiveErrors) = updateReceiveSigs cname csv0ReceiveSigs [] (Map.toList cs0ReceiveSigs)
                              sigMap' = Map.insert cname (ContractSigsV0 {csv0InitSig = cs0InitSig, csv0ReceiveSigs = updatedCsReceiveSigs}) sigMap
                          in go sigMap' (receiveErrors ++ errors) remaining

                    updateReceiveSigs :: Text -> Map.Map Text (Maybe CS.SchemaType) -> [CS.FuncName]
                                      -> [(Text, CS.SchemaType)] -> (Map.Map Text (Maybe CS.SchemaType), [CS.FuncName])
                    updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
                    updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
                      if Map.member fname sigMap
                      then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
                      else -- Schema has signature for method not in the module.
                        updateReceiveSigs cname sigMap (CS.ReceiveFuncName cname fname:errors) remaining
            (updatedContractSigs, extraSchemas) = addSchemas mis0ContractSigs ms0ContractSchemas
        in (ModuleInspectSigsV0 { mis0ContractSigs = updatedContractSigs}, extraSchemas)
      (Just CS.ModuleSchemaV1{..}, ModuleInspectSigsV1{..}) ->
        let
            addSchemas :: Map.Map Text ContractSigsV1 -> Map.Map Text CS.ContractSchemaV1 -> (Map.Map Text ContractSigsV1, [CS.FuncName])
            addSchemas mSigs mSchema = go mSigs [] (Map.toList mSchema)
              where
                    go :: Map.Map Text ContractSigsV1 -> [CS.FuncName] -> [(Text, CS.ContractSchemaV1)] -> (Map.Map Text ContractSigsV1, [CS.FuncName])
                    go sigMap errors [] = (sigMap, errors)
                    go sigMap errors ((cname, CS.ContractSchemaV1{..}):remaining) =
                      case Map.lookup cname sigMap of
                        Nothing -> let receiveErrors = map (CS.ReceiveFuncName cname) . Map.keys $ cs1ReceiveSigs
                                       errors' = CS.InitFuncName cname : receiveErrors ++ errors
                                   in go sigMap errors' remaining -- Schema has init signature for a contract not in the module.
                        Just ContractSigsV1{..} ->
                          let (updatedCsReceiveSigs, receiveErrors) = updateReceiveSigs cname csv1ReceiveSigs [] (Map.toList cs1ReceiveSigs)
                              sigMap' = Map.insert cname (ContractSigsV1 {csv1InitSig = cs1InitSig, csv1ReceiveSigs = updatedCsReceiveSigs}) sigMap
                          in go sigMap' (receiveErrors ++ errors) remaining

                    updateReceiveSigs :: Text -> Map.Map Text (Maybe CS.FunctionSchema) -> [CS.FuncName]
                                      -> [(Text, CS.FunctionSchema)] -> (Map.Map Text (Maybe CS.FunctionSchema), [CS.FuncName])
                    updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
                    updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
                      if Map.member fname sigMap
                      then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
                      else -- Schema has signature for method not in the module.
                        updateReceiveSigs cname sigMap (CS.ReceiveFuncName cname fname:errors) remaining
            (updatedContractSigs, extraSchemas) = addSchemas mis1ContractSigs ms1ContractSchemas
        in (ModuleInspectSigsV1 { mis1ContractSigs = updatedContractSigs}, extraSchemas)
      (Just CS.ModuleSchemaV0{}, ModuleInspectSigsV1{}) -> (moduleInspectSigsFromExports, []) -- Internal error: Should never happen.
      (Just CS.ModuleSchemaV1{}, ModuleInspectSigsV0{}) -> (moduleInspectSigsFromExports, []) -- Internal error: Should never happen.
      (Nothing, _) -> (moduleInspectSigsFromExports, []) -- No schema, just return the ModuleInspectSigs from exports.

    funcNames :: [CS.FuncName]
    funcNames = toFuncNames exportedFuncNames
      where
        toFuncNames :: [Text] -> [CS.FuncName]
        toFuncNames [] = []
        toFuncNames (name : remaining)
          | Wasm.isValidInitName name = (CS.InitFuncName . Text.drop 5 $ name) : toFuncNames remaining
          | Wasm.isValidReceiveName name =
            case Text.findIndex (== '.') name of
                                        Nothing -> toFuncNames remaining -- Cannot happen, as a valid receive name always has a dot.
                                        Just idx -> let (cname, fname) = Text.splitAt idx name
                                                        fnameWithoutDot = Text.tail fname
                                                    in CS.ReceiveFuncName cname fnameWithoutDot : toFuncNames remaining
          | otherwise = toFuncNames remaining -- Ignore other types of exported functions.

-- |Data type with information used by 'module inspect' command.
data ModuleInspectInfo
  = ModuleInspectInfo
  { miiNamedModRef :: Config.NamedModuleRef
  , miiWasmVersion :: Wasm.WasmVersion
  , miiModuleInspectSigs :: ModuleInspectSigs
  , miiExtraneousSchemas :: [CS.FuncName]
  }

-- |Different from ModuleSchema in that it uses ContractSigsV0/1 instead (see their definition).
data ModuleInspectSigs
  = ModuleInspectSigsV0 { mis0ContractSigs :: Map.Map Text ContractSigsV0 }
  | ModuleInspectSigsV1 { mis1ContractSigs :: Map.Map Text ContractSigsV1 }


-- |Different from ContractSchemaV0 in that the receiveSigs have a Maybe SchemaType.
data ContractSigsV0
  =  ContractSigsV0
  { csv0InitSig :: Maybe CS.SchemaType -- ^ Type signature for the init function.
  , csv0ReceiveSigs :: Map.Map Text (Maybe CS.SchemaType) -- ^ Type signatures for the receive functions.
  }

-- |Different from ContractSchemaV1 in that the receiveSigs have a Maybe FunctionSchema.
data ContractSigsV1
  = ContractSigsV1
  { csv1InitSig :: Maybe CS.FunctionSchema -- ^ Schema for the init function.
  , csv1ReceiveSigs :: Map.Map Text (Maybe CS.FunctionSchema) -- ^ Schemas for the receive functions.
  }
