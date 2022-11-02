{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Info
  ( contractNameFromInitName
  , addSchemaData
  , getContractName
  , hasFallbackReceiveSupport
  , hasReceiveMethod
  , constructModuleInspectInfo
  , ContractInfo(..)
  , ContractStateV0(..)
  , MethodsAndState(..)
  , Methods(..)
  , ModuleInspectInfo(..)
  , ModuleInspectSigs(..)
  , ContractSigsV0(..)
  , ContractSigsV1(..)
  , ContractSigsV2(..)
  , ContractSigsV3(..)
  ) where

import qualified Concordium.Types as T
import qualified Concordium.Client.Types.Contract.Schema as CS
import qualified Concordium.Client.Types.Contract.Parameter as CP
import qualified Concordium.Wasm as Wasm

import Data.Word (Word32)
import Data.Aeson ((.:))
import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Concordium.Client.Config as Config
import Concordium.Client.GRPC (ClientMonad)
import Concordium.Client.Cli

-- |Try to include extra information in the contract info from the module schema.
-- For V0 contracts:
--  - Try to decode the state, if the state schema is available.
--  - Include all available parameter schemas for receive methods.
-- For V1 contracts:
--  - Include all available function schemas for receive methods.
--
-- Logs warnings if:
--  - The contract is not included in the module schema.
--  - The state could not be parsed using the schema (only for v0 contracts).
--
-- Logs fatally on internal errors that should never occur, namely:
--  - Schema data has already been added.
--  - The version of module schema and contract info does not match.
addSchemaData :: ContractInfo -> CS.ModuleSchema -> ClientMonad IO (Maybe ContractInfo)
addSchemaData cInfo@ContractInfoV1{..} moduleSchema =
  case moduleSchema of
    CS.ModuleSchemaV0 _ -> logFatal ["Internal error: Cannot use ModuleSchemaV0 with ContractInfoV1."] -- Should never happen.
    CS.ModuleSchemaV1{..} ->
      case ciMethods of
        NoSchemaV1{..} -> case Map.lookup ciName ms1ContractSchemas of
          Nothing -> do
            logWarn [ [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
                    , "Showing the contract without information from the schema."]
            return Nothing
          Just contrSchema ->
            let ws1Methods = map (addFuncSchemaToMethodV1 contrSchema) ns1Methods
                withSchema = WithSchemaV1{..}
            in return $ Just (cInfo {ciMethods = withSchema})
        _ -> logFatal ["Internal error: Contract info has already been decoded."] -- Matches WithSchema1 / WithSchema2. Should never happen.
    CS.ModuleSchemaV2{..} ->
      case ciMethods of
        NoSchemaV1{..} -> case Map.lookup ciName ms2ContractSchemas of
          Nothing -> do
            logWarn [ [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
                    , "Showing the contract without information from the schema."]
            return Nothing
          Just contrSchema ->
            let ws2Methods = map (addFuncSchemaToMethodV2 contrSchema) ns1Methods
                withSchema = WithSchemaV2{..}
            in return $ Just (cInfo {ciMethods = withSchema})
        _ -> logFatal ["Internal error: Contract info has already been decoded."] -- Matches WithSchema1 / WithSchema2. Should never happen.
    CS.ModuleSchemaV3{..} ->
      case ciMethods of
        NoSchemaV1{..} -> case Map.lookup ciName ms3ContractSchemas of
          Nothing -> do
            logWarn [ [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
                    , "Showing the contract without information from the schema."]
            return Nothing
          Just contrSchema ->
            let ws2Methods = map (addFuncSchemaToMethodV3 contrSchema) ns1Methods
                withSchema = WithSchemaV2{..}
            in return $ Just (cInfo {ciMethods = withSchema})
        _ -> logFatal ["Internal error: Contract info has already been decoded."] -- Matches WithSchema1 / WithSchema2. Should never happen.
  where addFuncSchemaToMethodV1 :: CS.ContractSchemaV1 -> Text -> (Text, Maybe CS.FunctionSchemaV1)
        addFuncSchemaToMethodV1 contrSchema rcvName = let mFuncSchema = CS.lookupFunctionSchemaV1 contrSchema (CS.ReceiveFuncName ciName rcvName)
                                                    in (rcvName, mFuncSchema)
        addFuncSchemaToMethodV2 :: CS.ContractSchemaV2 -> Text -> (Text, Maybe CS.FunctionSchemaV2)
        addFuncSchemaToMethodV2 contrSchema rcvName = let mFuncSchema = CS.lookupFunctionSchemaV2 contrSchema (CS.ReceiveFuncName ciName rcvName)
                                                    in (rcvName, mFuncSchema)
        addFuncSchemaToMethodV3 :: CS.ContractSchemaV3 -> Text -> (Text, Maybe CS.FunctionSchemaV2)
        addFuncSchemaToMethodV3 contrSchema rcvName = let mFuncSchema = CS.lookupFunctionSchemaV3 contrSchema (CS.ReceiveFuncName ciName rcvName)
                                                    in (rcvName, mFuncSchema)

addSchemaData cInfo@ContractInfoV0{..} moduleSchema =
  case moduleSchema of
    CS.ModuleSchemaV3 _ -> logFatal ["Internal error: Cannot use ModuleSchemaV3 with ContractInfoV0."]  -- Should never happen.
    CS.ModuleSchemaV2 _ -> logFatal ["Internal error: Cannot use ModuleSchemaV2 with ContractInfoV0."]  -- Should never happen.
    CS.ModuleSchemaV1 _ -> logFatal ["Internal error: Cannot use ModuleSchemaV1 with ContractInfoV0."]  -- Should never happen.
    CS.ModuleSchemaV0{..} ->
      case ciMethodsAndState of
      WithSchemaV0{} -> logFatal ["Internal error: Contract info has already been decoded."] -- Should never happen.
      NoSchemaV0{..} -> case Map.lookup ciName ms0ContractSchemas of
        Nothing -> do
          logWarn [ [i|A schema for the contract '#{ciName}' does not exist in the schema provided.|]
                  , "Showing the contract without information from the schema."]
          return Nothing
        Just CS.ContractSchemaV0{..} -> do
          ws0State <- case cs0State of -- Check if the state exists in the schema.
                Nothing -> return $ Cs0Bytes ns0State
                Just schemaForState -> case S.runGet (CP.getJSONUsingSchema schemaForState) ns0State of
                  Left _ -> do
                    logWarn [ "Could not parse the state using the schema."
                            , "Showing the contract without information from the schema."]
                    return $ Cs0Bytes ns0State
                  Right jsonState -> return $ Cs0JSON jsonState
          let ws0Methods = map addSchemaToMethod ns0Methods
          let withSchema = WithSchemaV0{..}
          return $ Just (cInfo {ciMethodsAndState = withSchema})
  where addSchemaToMethod :: Text -> (Text, Maybe CS.SchemaType)
        addSchemaToMethod rcvName = let mSchema = CS.lookupParameterSchema moduleSchema (CS.ReceiveFuncName ciName rcvName)
                                        in (rcvName, mSchema)

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
          ContractInfoV0{..} -> case ciMethodsAndState of
            NoSchemaV0{..} -> ns0Methods
            WithSchemaV0{..} -> map fst ws0Methods
          ContractInfoV1{..} -> case ciMethods of
            NoSchemaV1{..} -> ns1Methods
            WithSchemaV1{..} -> map fst ws1Methods
            WithSchemaV2{..} -> map fst ws2Methods

-- |Get the contract name (without the 'init_' prefix).
getContractName :: ContractInfo -> Text
getContractName = \case
  ContractInfoV0{..} -> ciName
  ContractInfoV1{..} -> ciName

-- |Returns True if the contract has fallback entrypoint support,
-- False otherwise.
hasFallbackReceiveSupport :: ContractInfo -> Bool
hasFallbackReceiveSupport = \case
  ContractInfoV0{} -> False
  ContractInfoV1{} -> True

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
      -- |The methods and state of the contract.
    , ciMethodsAndState :: !MethodsAndState }
  | ContractInfoV1
    { -- |The contract balance.
      ciAmount :: !T.Amount
      -- |The owner of the contract.
    , ciOwner  :: !T.AccountAddress
      -- |The contract name.
    , ciName :: !Text
      -- |The corresponding source module.
    , ciSourceModule :: !T.ModuleRef
      -- |The methods of the contract.
    , ciMethods :: !Methods }
  deriving (Eq, Show)

-- |Methods and State for V0 Contracts.
--  Additional information from the schema can be added with `addSchemaData`.
data MethodsAndState
  = NoSchemaV0
  { ns0State   :: !ByteString
  , ns0Methods :: ![Text]
  }
  | WithSchemaV0
  { ws0State   :: !ContractStateV0
  , ws0Methods :: ![(Text, Maybe CS.SchemaType)]
  } deriving (Eq, Show)

-- |Methods for V1 Contracts.
--  Additional information from the schema can be added with `addSchemaData`.
--  The schemas can be either of version 1 or 2.
data Methods
  = NoSchemaV1   { ns1Methods :: ![Text] }
  | WithSchemaV1 { ws1Methods :: ![(Text, Maybe CS.FunctionSchemaV1)]}
  | WithSchemaV2 { ws2Methods :: ![(Text, Maybe CS.FunctionSchemaV2)]}
  deriving (Eq, Show)

-- |Contract state for a V0 contract.
--  Can either be the raw bytes or a JSON value with the parsed state (parsed with a schema).
data ContractStateV0
  = Cs0Bytes !ByteString
  | Cs0JSON !AE.Value
  deriving (Eq, Show)

instance AE.FromJSON ContractInfo where
  parseJSON = AE.withObject "Info" $ \v -> do
    ciAmount            <- v .: "amount"
    ciOwner             <- v .: "owner"
    ciName              <- contractNameFromInitName <$> v .: "name"
    ciSourceModule      <- v .: "sourceModule"
    methods             <- fmap methodNameFromReceiveName <$> v .: "methods"
    (v AE..:! "version" AE..!= (0 :: Word32)) >>= \case
      0 -> do
        (state, ciSize) <- case KM.lookup "model" v of
          Just (AE.String s) -> do
            let decodeBase16 xs =
                  case BS16.decode . Text.encodeUtf8 $ xs of
                    Right parsed -> return parsed
                    Left err -> fail [i|Invalid model. #{err}'|]
            bs <- decodeBase16 s
            return (bs, BS.length bs)
          Just x -> fail [i|Invalid Info, expected "model" field to be a String of base16 bytes, but got: #{x}|]
          Nothing -> fail [i|Invalid Info, missing "model" field.|]
        let ciMethodsAndState = NoSchemaV0 { ns0State = state, ns0Methods = methods}
        return ContractInfoV0{..}
      1 -> let ciMethods = NoSchemaV1 { ns1Methods = methods }
           in return ContractInfoV1{..}
      n -> fail [i|Unsupported contract version #{n}.|]

-- | Version of a module schema.
data ModuleSchemaVersion = SchemaV0 | SchemaV1 | SchemaV2 | SchemaV3

-- |Construct module inspect info.
-- Works by:
--  - Creating ModuleInspectSigs from the list of exported function names.
--  - Inserting the signatures from the moduleSchema into the ModuleInspectSigs
--     - And collect the function names for /extraneous/ schemas,
--     - i.e. schemas for functions that are not exported in the module.
--
-- If a schema is provided, it will create the corresponding version of ModuleInspectInfo.
-- Otherwise, it will create ModuleInspectInfoV0 for V0 contracts and V2 for V1 contracts.
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
    (moduleInspectSigs, extraneousSchemas) = case moduleSchema of
                                              Nothing -> case wasmVersion of
                                                Wasm.V0 -> (moduleInspectSigsFromExports SchemaV0, []) -- Only V0 schemas work for V0 contracts
                                                Wasm.V1 -> (moduleInspectSigsFromExports SchemaV2, []) -- Defaults to V2 schema
                                              Just modSchema -> mkModInspectWithSchema modSchema

    -- Creates the moduleInspectSigs from the exported func names.
    moduleInspectSigsFromExports :: ModuleSchemaVersion -> ModuleInspectSigs
    moduleInspectSigsFromExports v = case v of
      SchemaV0 ->
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
              Nothing -> insertReceiveNames remaining sigMap
              Just cs0@ContractSigsV0{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv0ReceiveSigs
                    sigMap' = Map.insert cname (cs0 {csv0ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis0ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV0{..}
      SchemaV1 ->
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
              Nothing -> insertReceiveNames remaining sigMap
              Just cs1@ContractSigsV1{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv1ReceiveSigs
                    sigMap' = Map.insert cname (cs1 {csv1ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis1ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV1{..}
      SchemaV2 ->
        let mkContrSchemaTuples x xs = case x of
              CS.InitFuncName contrName -> (contrName, ContractSigsV2 { csv2InitSig = Nothing
                                                                      , csv2ReceiveSigs = Map.empty
                                                                      }) : xs
              CS.ReceiveFuncName _ _ -> xs
            cSigsWithoutReceives = Map.fromList . foldr mkContrSchemaTuples [] $ funcNames

            insertReceiveNames :: [CS.FuncName] -> Map.Map Text ContractSigsV2 -> Map.Map Text ContractSigsV2
            insertReceiveNames [] sigMap = sigMap
            insertReceiveNames (CS.InitFuncName _:remaining) sigMap = insertReceiveNames remaining sigMap
            insertReceiveNames (CS.ReceiveFuncName cname fname:remaining) sigMap = case Map.lookup cname sigMap of
              Nothing -> insertReceiveNames remaining sigMap
              Just cs2@ContractSigsV2{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv2ReceiveSigs
                    sigMap' = Map.insert cname (cs2 {csv2ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis2ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV2{..}
      SchemaV3 ->
        let mkContrSchemaTuples x xs = case x of
              CS.InitFuncName contrName -> (contrName, ContractSigsV3 { csv3InitSig = Nothing
                                                                      , csv3ReceiveSigs = Map.empty
                                                                      , cs3EventSchemas = Nothing
                                                                      }) : xs
              CS.ReceiveFuncName _ _ -> xs
            cSigsWithoutReceives = Map.fromList . foldr mkContrSchemaTuples [] $ funcNames

            insertReceiveNames :: [CS.FuncName] -> Map.Map Text ContractSigsV3 -> Map.Map Text ContractSigsV3
            insertReceiveNames [] sigMap = sigMap
            insertReceiveNames (CS.InitFuncName _:remaining) sigMap = insertReceiveNames remaining sigMap
            insertReceiveNames (CS.ReceiveFuncName cname fname:remaining) sigMap = case Map.lookup cname sigMap of
              Nothing -> insertReceiveNames remaining sigMap
              Just cs3@ContractSigsV3{..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csv3ReceiveSigs
                    sigMap' = Map.insert cname (cs3 {csv3ReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

            mis3ContractSigs = insertReceiveNames funcNames cSigsWithoutReceives
        in ModuleInspectSigsV3{..}

    -- Creates a ModuleInspectSigs and a list of extraneous schemas from module schema and the exported func names.
    mkModInspectWithSchema modSchema = case modSchema of
      CS.ModuleSchemaV0{..} ->
        let
            ModuleInspectSigsV0{..} = moduleInspectSigsFromExports SchemaV0 -- We know this becomes a ModuleInspectSigsV0
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
      CS.ModuleSchemaV1{..} ->
        let
            ModuleInspectSigsV1{..} = moduleInspectSigsFromExports SchemaV1 -- We know this becomes a ModuleInspectSigsV1
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

                    updateReceiveSigs :: Text -> Map.Map Text (Maybe CS.FunctionSchemaV1) -> [CS.FuncName]
                                      -> [(Text, CS.FunctionSchemaV1)] -> (Map.Map Text (Maybe CS.FunctionSchemaV1), [CS.FuncName])
                    updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
                    updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
                      if Map.member fname sigMap
                      then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
                      else -- Schema has signature for method not in the module.
                        updateReceiveSigs cname sigMap (CS.ReceiveFuncName cname fname:errors) remaining
            (updatedContractSigs, extraSchemas) = addSchemas mis1ContractSigs ms1ContractSchemas
        in (ModuleInspectSigsV1 { mis1ContractSigs = updatedContractSigs}, extraSchemas)
      CS.ModuleSchemaV2{..} ->
        let
            ModuleInspectSigsV2{..} = moduleInspectSigsFromExports SchemaV2 -- We know this becomes a ModuleInspectSigsV2
            addSchemas :: Map.Map Text ContractSigsV2 -> Map.Map Text CS.ContractSchemaV2 -> (Map.Map Text ContractSigsV2, [CS.FuncName])
            addSchemas mSigs mSchema = go mSigs [] (Map.toList mSchema)
              where
                    go :: Map.Map Text ContractSigsV2 -> [CS.FuncName] -> [(Text, CS.ContractSchemaV2)] -> (Map.Map Text ContractSigsV2, [CS.FuncName])
                    go sigMap errors [] = (sigMap, errors)
                    go sigMap errors ((cname, CS.ContractSchemaV2{..}):remaining) =
                      case Map.lookup cname sigMap of
                        Nothing -> let receiveErrors = map (CS.ReceiveFuncName cname) . Map.keys $ cs2ReceiveSigs
                                       errors' = CS.InitFuncName cname : receiveErrors ++ errors
                                   in go sigMap errors' remaining -- Schema has init signature for a contract not in the module.
                        Just ContractSigsV2{..} ->
                          let (updatedCsReceiveSigs, receiveErrors) = updateReceiveSigs cname csv2ReceiveSigs [] (Map.toList cs2ReceiveSigs)
                              sigMap' = Map.insert cname (ContractSigsV2 {csv2InitSig = cs2InitSig, csv2ReceiveSigs = updatedCsReceiveSigs}) sigMap
                          in go sigMap' (receiveErrors ++ errors) remaining

                    updateReceiveSigs :: Text -> Map.Map Text (Maybe CS.FunctionSchemaV2) -> [CS.FuncName]
                                      -> [(Text, CS.FunctionSchemaV2)] -> (Map.Map Text (Maybe CS.FunctionSchemaV2), [CS.FuncName])
                    updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
                    updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
                      if Map.member fname sigMap
                      then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
                      else -- Schema has signature for method not in the module.
                        updateReceiveSigs cname sigMap (CS.ReceiveFuncName cname fname:errors) remaining
            (updatedContractSigs, extraSchemas) = addSchemas mis2ContractSigs ms2ContractSchemas
        in (ModuleInspectSigsV2 { mis2ContractSigs = updatedContractSigs}, extraSchemas)
      CS.ModuleSchemaV3{..} ->
        let
            ModuleInspectSigsV3{..} = moduleInspectSigsFromExports SchemaV3 -- We know this becomes a ModuleInspectSigsV3
            addSchemas :: Map.Map Text ContractSigsV3 -> Map.Map Text CS.ContractSchemaV3 -> (Map.Map Text ContractSigsV3, [CS.FuncName])
            addSchemas mSigs mSchema = go mSigs [] (Map.toList mSchema)
              where
                    go :: Map.Map Text ContractSigsV3 -> [CS.FuncName] -> [(Text, CS.ContractSchemaV3)] -> (Map.Map Text ContractSigsV3, [CS.FuncName])
                    go sigMap errors [] = (sigMap, errors)
                    go sigMap errors ((cname, CS.ContractSchemaV3{..}):remaining) =
                      case Map.lookup cname sigMap of
                        Nothing -> let receiveErrors = map (CS.ReceiveFuncName cname) . Map.keys $ cs3ReceiveSigs
                                       errors' = CS.InitFuncName cname : receiveErrors ++ errors
                                   in go sigMap errors' remaining -- Schema has init signature for a contract not in the module.
                        Just ContractSigsV3{..} ->
                          let (updatedCsReceiveSigs, receiveErrors) = updateReceiveSigs cname csv3ReceiveSigs [] (Map.toList cs3ReceiveSigs)
                              sigMap' = Map.insert cname (ContractSigsV3 {csv3InitSig = cs3InitSig, csv3ReceiveSigs = updatedCsReceiveSigs, cs3EventSchemas = cs3EventSchemas}) sigMap
                          in go sigMap' (receiveErrors ++ errors) remaining

                    updateReceiveSigs :: Text -> Map.Map Text (Maybe CS.FunctionSchemaV2) -> [CS.FuncName]
                                      -> [(Text, CS.FunctionSchemaV2)] -> (Map.Map Text (Maybe CS.FunctionSchemaV2), [CS.FuncName])
                    updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
                    updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
                      if Map.member fname sigMap
                      then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
                      else -- Schema has signature for method not in the module.
                        updateReceiveSigs cname sigMap (CS.ReceiveFuncName cname fname:errors) remaining
            (updatedContractSigs, extraSchemas) = addSchemas mis3ContractSigs ms3ContractSchemas
        in (ModuleInspectSigsV3 { mis3ContractSigs = updatedContractSigs}, extraSchemas)

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
  | ModuleInspectSigsV2 { mis2ContractSigs :: Map.Map Text ContractSigsV2 }
  | ModuleInspectSigsV3 { mis3ContractSigs :: Map.Map Text ContractSigsV3 }


-- |Different from ContractSchemaV0 in that the receiveSigs have a Maybe SchemaType.
data ContractSigsV0
  =  ContractSigsV0
  { csv0InitSig :: Maybe CS.SchemaType -- ^ Type signature for the init function.
  , csv0ReceiveSigs :: Map.Map Text (Maybe CS.SchemaType) -- ^ Type signatures for the receive functions.
  }

-- |Different from ContractSchemaV1 in that the receiveSigs have a Maybe FunctionSchemaV1.
data ContractSigsV1
  = ContractSigsV1
  { csv1InitSig :: Maybe CS.FunctionSchemaV1 -- ^ Schema for the init function.
  , csv1ReceiveSigs :: Map.Map Text (Maybe CS.FunctionSchemaV1) -- ^ Schemas for the receive functions.
  }

-- |Different from ContractSchemaV2 in that the receiveSigs have a Maybe FunctionSchemaV2.
data ContractSigsV2
  = ContractSigsV2
  { csv2InitSig :: Maybe CS.FunctionSchemaV2 -- ^ Schema for the init function.
  , csv2ReceiveSigs :: Map.Map Text (Maybe CS.FunctionSchemaV2) -- ^ Schemas for the receive functions.
  }

-- |Different from ContractSchemaV3 in that the receiveSigs have a Maybe FunctionSchemaV2.
data ContractSigsV3
  = ContractSigsV3
  { csv3InitSig :: Maybe CS.FunctionSchemaV2 -- ^ Schema for the init function.
  , csv3ReceiveSigs :: Map.Map Text (Maybe CS.FunctionSchemaV2) -- ^ Schemas for the receive functions.
  , cs3EventSchemas :: Maybe CS.SchemaType -- ^ Schemas for the events
  }