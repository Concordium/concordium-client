module Concordium.Client.Commands (
    optsParser,
    backendParser,
    allowedValuesDelegationTargetAsString,
    Verbose,
    Options (..),
    Backend (..),
    Cmd (..),
    ConfigCmd (..),
    AccountExportFormat (..),
    ConfigAccountCmd (..),
    TransactionOpts (..),
    Interval (..),
    InteractionOpts (..),
    TransactionCmd (..),
    PLTCmd (..),
    TokenSupplyAction (..),
    ModifyListAction (..),
    AccountCmd (..),
    ModuleCmd (..),
    ContractCmd (..),
    LegacyCmd (..),
    ConsensusCmd (..),
    BlockCmd (..),
    BakerCmd (..),
    DelegatorCmd (..),
    IdentityCmd (..),
    IdentityShowCmd (..),
    PoolCmd (..),
    MemoInput (..),
    RegisterDataInput (..),
    ParameterFileInput (..),
    InvokerInput (..),
    ExtraBakerAddData (..),
    EpochSpecifier (..),
) where

import Concordium.Client.LegacyCommands
import Concordium.Client.Types.Account
import Concordium.Client.Utils
import Concordium.Common.Time
import Concordium.ID.Types (AccountThreshold, CredentialIndex, CredentialRegistrationID, KeyIndex)
import Concordium.Types
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm
import Control.Monad
import Data.String
import Data.Text hiding (map, unlines)
import Data.Time.Format.ISO8601
import Data.Version (showVersion)
import Data.Word (Word64)
import Network.HTTP2.Client
import Options.Applicative
import Options.Applicative.Help.Pretty (fillCat, hang, softline)
import Paths_concordium_client (version)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Terminal as P
import Text.Printf

type Verbose = Bool

data Options = Options
    { optsCmd :: Cmd,
      optsBackend :: Backend,
      optsConfigDir :: Maybe FilePath,
      optsVerbose :: Verbose
    }
    deriving (Show)

data Backend = GRPC
    { grpcHost :: !HostName,
      grpcPort :: !PortNumber,
      grpcTarget :: !(Maybe String),
      grpcRetryNum :: !Int,
      grpcUseTls :: !Bool
    }
    deriving (Show)

data Cmd
    = LegacyCmd
        {legacyCmd :: LegacyCmd}
    | ConfigCmd
        {configCmd :: ConfigCmd}
    | TransactionCmd
        {transactionCmd :: TransactionCmd}
    | AccountCmd
        {accountCmd :: AccountCmd}
    | ModuleCmd
        {moduleCmd :: ModuleCmd}
    | ContractCmd
        {contractCmd :: ContractCmd}
    | ConsensusCmd
        {consensusCmd :: ConsensusCmd}
    | BlockCmd
        {blockCmd :: BlockCmd}
    | BakerCmd
        {bakerCmd :: BakerCmd}
    | DelegatorCmd
        {delegatorCmd :: DelegatorCmd}
    | IdentityCmd
        {identityCmd :: IdentityCmd}
    | PoolCmd
        {poolCmd :: PoolCmd}
    deriving (Show)

data ConfigCmd
    = ConfigInit
    | ConfigShow
    | ConfigBackupExport
        {cbeFileName :: !FilePath}
    | ConfigBackupImport
        { cbiFileName :: !FilePath,
          cbiSkipExisting :: !Bool
        }
    | ConfigAccountCmd -- groups 'config account' commands
        {configAccountCmd :: ConfigAccountCmd}
    deriving (Show)

data ConfigAccountCmd
    = ConfigAccountName
        { caaAddr :: !Text,
          caaName :: !Text
        }
    | ConfigAccountRemove
        {carAddr :: !Text}
    | ConfigAccountImport
        { caiFile :: !FilePath,
          caiName :: !(Maybe Text),
          caiFormat :: !AccountExportFormat,
          caiSkipExisting :: !Bool
        }
    | ConfigAccountAddKeys
        { caakAddr :: !Text,
          caakKeysFile :: !FilePath
        }
    | ConfigAccountUpdateKeys
        { caukAddr :: !Text,
          caukKeysFile :: !FilePath
        }
    | ConfigAccountChangeKeyPassword
        { cackpName :: !Text,
          cackpCIndex :: !CredentialIndex,
          cackpIndex :: !KeyIndex
        }
    | ConfigAccountRemoveKeys
        { carkAddr :: !Text,
          carkCidx :: !CredentialIndex,
          carkKeys :: ![KeyIndex]
        }
    | ConfigAccountRemoveName
        {carnText :: !Text}
    deriving (Show)

data Interval
    = Minute -- 60 secs
    | Hour -- 60 mins
    | Day -- 24 hours
    | Week -- 7 days
    | Month -- 30 days
    | Year -- 365 days
    deriving (Show, Read)

data MemoInput
    = MemoString Text
    | MemoJSON FilePath
    | MemoRaw FilePath
    deriving (Show, Read)

memoInputParser :: Parser (Maybe MemoInput)
memoInputParser =
    optional
        ( (MemoString <$> strOption (long "memo" <> metavar "MEMO" <> help "Transaction memo."))
            <|> (MemoJSON <$> strOption (long "memo-json" <> metavar "FILE" <> help "File with transaction memo."))
            <|> (MemoRaw <$> strOption (long "memo-raw" <> metavar "FILE" <> help "File with raw bytes."))
        )

data RegisterDataInput
    = RegisterString Text
    | RegisterJSON FilePath
    | RegisterRaw FilePath
    deriving (Show, Read)

registerDataParser :: Parser RegisterDataInput
registerDataParser =
    (RegisterString <$> strOption (long "string" <> metavar "STRING" <> help "String to be registered on chain."))
        <|> (RegisterJSON <$> strOption (long "json" <> metavar "FILE" <> help "File with JSON to registered on chain."))
        <|> (RegisterRaw <$> strOption (long "raw" <> metavar "FILE" <> help "File with raw bytes to be registered on chain."))

data TransactionCmd
    = TransactionSubmit
        { tsFile :: !FilePath,
          tsInteractionOpts :: !InteractionOpts
        }
    | TransactionAddSignature
        { tasFile :: !FilePath,
          tasSigners :: !(Maybe Text),
          tasToKeys :: !(Maybe FilePath)
        }
    | TransactionStatus
        { tsHash :: !Text,
          -- | Path to a contract schema, used to display the transaction event info.
          tsSchema :: !(Maybe FilePath)
        }
    | TransactionSendCcd
        { tsgReceiver :: !Text,
          tsgAmount :: !Amount,
          tsgMemo :: !(Maybe MemoInput),
          tsgOpts :: !(TransactionOpts (Maybe Energy))
        }
    | TransactionSendWithSchedule
        { twsReceiver :: !Text,
          -- | Eiher total amount, interval, number of intervals and starting time or a raw list of timestamps and amounts.
          twsSchedule :: !(Either (Amount, Interval, Int, Timestamp) [(Timestamp, Amount)]),
          twsMemo :: !(Maybe MemoInput),
          twsOpts :: !(TransactionOpts (Maybe Energy))
        }
    | TransactionDeployCredential
        { tdcFile :: !FilePath,
          tdcInteractionOpts :: !InteractionOpts
        }
    | -- | Register data on chain.
      TransactionRegisterData
        { -- | File containing the data.
          trdData :: !RegisterDataInput,
          -- | Options for transaction.
          trdTransactionOptions :: !(TransactionOpts (Maybe Energy))
        }
    | PLTCmd PLTCmd
    deriving (Show)

data TokenSupplyAction = Mint | Burn
    deriving (Show, Eq)

data ModifyListAction = AddAllowList | RemoveAllowList | AddDenyList | RemoveDenyList
    deriving (Show, Eq)

data PLTCmd
    = TransactionPLTTransfer
        { tptReceiver :: !Text,
          tptAmount :: !PreTokenAmount,
          tptTokenId :: !Text,
          tptMemo :: !(Maybe MemoInput),
          tptOpts :: !(TransactionOpts (Maybe Energy))
        }
    | TransactionPLTUpdateSupply
        { tpusAction :: !TokenSupplyAction,
          tpusAmount :: !PreTokenAmount,
          tpusTokenId :: !Text,
          tpusOpts :: !(TransactionOpts (Maybe Energy))
        }
    | TransactionPLTModifyList
        { tpmlAction :: !ModifyListAction,
          tpmlAccount :: !Text,
          tpmlTokenId :: !Text,
          tpmlOpts :: !(TransactionOpts (Maybe Energy))
        }
    deriving (Show)

data AccountCmd
    = AccountShow
        { asAddress :: !(Maybe Text),
          asBlockHash :: !(Maybe Text),
          asShowEncryptedBalance :: !Bool,
          asDecryptEncryptedBalance :: !Bool
        }
    | AccountList
        {alBlockHash :: !(Maybe Text)}
    | AccountUpdateKeys
        { aukKeys :: !FilePath,
          aukCredId :: !CredentialRegistrationID,
          aukTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | -- | Transfer part of the encrypted balance to the public balance of the
      --  account.
      AccountDecrypt
        { adTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          -- | Amount to transfer from encrypted to public balance.
          adAmount :: !Amount,
          -- | Which indices of incoming amounts to use as inputs.
          -- If none are provided all existing ones will be used.
          adIndex :: !(Maybe Int)
        }
    | -- | Updated credentials and account threshold (i.e. how many credential holders that need to sign transactions)
      AccountUpdateCredentials
        { aucNewCredInfos :: !(Maybe FilePath), -- File containing the new CredentialDeploymentInformation's
          aucRemoveCredIds :: !(Maybe FilePath), -- File containing the CredentialRegistrationID's for the credentials to be removed
          aucNewThreshold :: !AccountThreshold, -- The new account threshold
          aucTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | -- | Show an alias for the account.
      AccountShowAlias
        { -- | Name or address of the account.
          asaAddress :: !Text,
          asaAlias :: !Word
        }
    deriving (Show)

data ModuleCmd
    = -- | Deploy the provided smart contract module on chain.
      ModuleDeploy
        { -- | Path to the module.
          mdModuleFile :: !FilePath,
          -- | Local alias for the module reference.
          mdName :: !(Maybe Text),
          -- | Optional Wasm version for the module.
          mdWasmVersion :: !(Maybe Wasm.WasmVersion),
          -- | Options for transaction.
          mdTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | -- | List all modules.
      ModuleList
        { -- | Hash of the block (default "best").
          mlBlockHash :: !(Maybe Text)
        }
    | -- | Output the binary source code of the module to the provided file.
      ModuleShow
        { -- | Reference to the module OR a module name.
          msModuleRefOrName :: !Text,
          -- | Output the module to this file.
          --  Use '-' to output to stdout.
          msOutFile :: !FilePath,
          -- | Hash of the block (default "best").
          msBlockHash :: !(Maybe Text)
        }
    | -- | Show the functions available in a module, including type signatures if schema is provided.
      ModuleInspect
        { -- | Reference to the module OR a module name.
          miModuleRefOrName :: !Text,
          -- | Path to a contract schema, used to display the type signatures.
          miSchema :: !(Maybe FilePath),
          -- | Hash of the block (default "best").
          miBlockHash :: !(Maybe Text)
        }
    | -- | Add a local name to a module.
      ModuleName
        { -- | Module reference OR path to the module (reference then calculated by hashing).
          mnModule :: !String,
          -- | Name for the module.
          mnName :: !Text,
          -- | Optional Wasm version for the module.
          mnWasmVersion :: !(Maybe Wasm.WasmVersion)
        }
    | -- | Remove a local name from the module name map
      ModuleRemoveName
        { -- | The module name to remove
          mrnText :: !Text
        }
    deriving (Show)

data ContractCmd
    = -- | Show the state of specified contract.
      ContractShow
        { -- | Index of the contract address OR a contract name.
          csAddressIndexOrName :: !Text,
          -- | Subindex of the address for the contract (default: 0).
          csAddressSubindex :: !(Maybe Word64),
          -- | Path to a contract schema, used to display the contract info.
          csSchema :: !(Maybe FilePath),
          -- | Hash of the block (default "best").
          csBlockHash :: !(Maybe Text)
        }
    | -- | List all contracts on chain.
      ContractList
        { -- | Hash of the block (default "best").
          clBlockHash :: !(Maybe Text)
        }
    | -- | Initialize a contract from a module on chain.
      ContractInit
        { -- | Module reference OR module name OR (if ciPath == True) path to the module (reference then calculated by hashing).
          ciModule :: !String,
          -- | Name of the contract to initialize. This corresponds to a specific init function.
          ciContractName :: !Text,
          -- | Optional path to a JSON or binary file containing parameters for the init function.
          ciParameterFileJSON :: !(Maybe ParameterFileInput),
          -- | Path to a contract schema.
          ciSchema :: !(Maybe FilePath),
          -- | Local alias for the contract address.
          ciName :: !(Maybe Text),
          -- | Determines whether ciModule should be interpreted as a path.
          ciPath :: !Bool,
          -- | Optional Wasm version for the module.
          ciWasmVersion :: !(Maybe Wasm.WasmVersion),
          -- | Amount to be send to contract (default: 0).
          ciAmount :: !Amount,
          -- | Options for transaction.
          ciTransactionOpts :: !(TransactionOpts Energy)
        }
    | -- | Update an existing contract, i.e. invoke a receive function.
      ContractUpdate
        { -- | Index of the contract address OR a contract name.
          cuAddressIndexOrName :: !Text,
          -- | Subindex of the address for the contract to invoke (default: 0).
          cuAddressSubindex :: !(Maybe Word64),
          -- | Name of the receive function to use.
          cuReceiveName :: !Text,
          -- | Optional path to a JSON or binary file containing parameters for the receive function.
          cuParameterFileJSON :: !(Maybe ParameterFileInput),
          -- | Path to a contract schema.
          cuSchema :: !(Maybe FilePath),
          -- | Amount to invoke the receive function with (default: 0).
          cuAmount :: !Amount,
          -- | Options for transaction.
          cuTransactionOpts :: !(TransactionOpts Energy)
        }
    | -- | Invoke a contract locally and view its output.
      ContractInvoke
        { -- | Index of the contract address OR a contract name.
          civAddressIndexOrName :: !Text,
          -- | Subindex of the address fro the contract to invoke (default: 0).
          civAddressSubindex :: !(Maybe Word64),
          -- | Name of the receive function to use.
          civReceiveName :: !Text,
          -- | Optional path to a JSON or binary file containing parameters for the receive function.
          civParameterFile :: !(Maybe ParameterFileInput),
          -- | Path to a contract schema.
          civSchema :: !(Maybe FilePath),
          -- | Amount to invoke the receive function with (default: 0).
          civAmount :: !Amount,
          -- | Account address or name to use as invoker.
          civInvoker :: !(Maybe InvokerInput),
          -- | Maximum energy allowed for the invocation (default: 10,000,000).
          civMaxEnergy :: !(Maybe Energy),
          -- | Hash of the block (default: "best").
          civBlockHash :: !(Maybe Text)
        }
    | -- | Add a local name to a contract.
      ContractName
        { -- | Index of the address for the contract.
          cnAddressIndex :: !Word64,
          -- | Subindex of the address for the contract (default: 0).
          cnAddressSubindex :: !(Maybe Word64),
          -- | Name for the contract.
          cnName :: !Text
        }
    | -- | Remove a local name from the contract name map
      ContractRemoveName
        { -- | The contract name to remove
          crnText :: !Text
        }
    deriving (Show)

-- | The type parameter 'energyOrMaybe' should be Energy or Maybe Energy.
data TransactionOpts energyOrMaybe = TransactionOpts
    { toSender :: !(Maybe Text),
      toAlias :: !(Maybe Word),
      toKeys :: !(Maybe FilePath),
      toSigners :: !(Maybe Text),
      toNonce :: !(Maybe Nonce),
      toMaxEnergyAmount :: !energyOrMaybe,
      toExpiration :: !(Maybe Text),
      -- | Optional file name and path to output the signed/partially-signed
      --  transaction to instead of submitting the transaction on-chain.
      toOutFile :: !(Maybe FilePath),
      toInteractionOpts :: !InteractionOpts
    }
    deriving (Show)

data InteractionOpts = InteractionOpts
    { ioConfirm :: !Bool,
      ioTail :: !Bool
    }
    deriving (Show)

data ConsensusCmd
    = ConsensusStatus
    | ConsensusShowParameters
        { cspBlockHash :: !(Maybe Text),
          cspIncludeBakers :: !Bool
        }
    | ConsensusShowChainParameters
        {cscpBlockHash :: !(Maybe Text)}
    | ConsensusChainUpdate
        { ccuUpdate :: !FilePath,
          ccuKeys :: ![FilePath],
          ccuInteractionOpts :: !InteractionOpts
        }
    | ConsensusDetailedStatus
        { cdsGenesisIndex :: !(Maybe GenesisIndex)
        }
    deriving (Show)

data BlockCmd = BlockShow
    {bsBlockHash :: !(Maybe Text)}
    deriving (Show)

-- Extra data to add a baker in protocol version > 4
data ExtraBakerAddData = ExtraBakerAddData
    { ebadOpenForDelegation :: !OpenStatus,
      ebadMetadataURL :: !String,
      ebadTransactionFeeCommission :: !AmountFraction,
      ebadBakingRewardCommission :: !AmountFraction,
      ebadFinalizationRewardCommission :: !AmountFraction
    }
    deriving (Show)

data BakerCmd
    = BakerGenerateKeys
        { bgkFile :: !(Maybe FilePath),
          bgkBakerId :: !(Maybe BakerId)
        }
    | BakerAdd
        { baFile :: !FilePath,
          baTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          baStake :: !Amount,
          baAutoAddEarnings :: !Bool,
          baExtraData :: !(Maybe ExtraBakerAddData),
          baOutputFile :: !(Maybe FilePath)
        }
    | BakerSetKeys
        { bsaKeysFile :: !FilePath,
          bsaTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          bsaOutputFile :: Maybe FilePath
        }
    | BakerRemove
        {brTransactionOpts :: !(TransactionOpts (Maybe Energy))}
    | BakerUpdateStake
        { busStake :: !Amount,
          busTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | BakerUpdateRestakeEarnings
        { bursRestake :: !Bool,
          bursTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | BakerConfigure
        { bcTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          bcCapital :: !(Maybe Amount),
          bcRestake :: !(Maybe Bool),
          bcOpenForDelegation :: !(Maybe OpenStatus),
          bcMetadataURL :: !(Maybe String),
          bcTransactionFeeCommission :: !(Maybe AmountFraction),
          bcBakingRewardCommission :: !(Maybe AmountFraction),
          bcFinalizationRewardCommission :: !(Maybe AmountFraction),
          bcSuspend :: !(Maybe Bool),
          bcInputKeyFile :: !(Maybe FilePath),
          bcOutputKeyFile :: !(Maybe FilePath)
        }
    | BakerUpdateMetadataURL
        { bumuMetadataURL :: !String,
          bumuTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | BakerUpdateOpenDelegationStatus
        { bodsOpenForDelegation :: !OpenStatus,
          bodsTransactionOpts :: !(TransactionOpts (Maybe Energy))
        }
    | BakerGetEarliestWinTime
        { bgewtBakerId :: !BakerId,
          bgewtShowLocalTime :: !Bool,
          bgewtPoll :: !Bool
        }
    deriving (Show)

data DelegatorCmd
    = DelegatorConfigure
        { dcTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          dcCapital :: !(Maybe Amount),
          dcRestake :: !(Maybe Bool),
          dcDelegationTarget :: !(Maybe Text)
        }
    | DelegatorRemove
        {drTransactionOpts :: !(TransactionOpts (Maybe Energy))}
    | DelegatorAdd
        { daTransactionOpts :: !(TransactionOpts (Maybe Energy)),
          daCapital :: !Amount,
          daRestake :: !Bool,
          daDelegationTarget :: !Text
        }
    deriving (Show)

data IdentityCmd = IdentityShow
    {identityShow :: IdentityShowCmd} -- groups `identity show` commands
    deriving (Show)

data IdentityShowCmd
    = IdentityShowIPs
        {isipsBlockHash :: !(Maybe Text)}
    | IdentityShowARs
        {isarsBlockHash :: !(Maybe Text)}
    deriving (Show)

data PoolCmd = PoolStatus
    { psPool :: !(Maybe BakerId),
      psBlockHash :: !(Maybe Text)
    }
    deriving (Show)

visibleHelper :: Parser (a -> a)
visibleHelper =
    abortOption (ShowHelpText Nothing) $
        mconcat
            [ long "help",
              short 'h',
              help "Show detailed help text."
            ]

optsParser :: ParserInfo Options
optsParser =
    info
        (programOptions <**> versionOption <**> visibleHelper)
        ( fullDesc
            <> progDesc "Concordium Client CLI."
            <> header "concordium-client - a client to interact with the concordium network."
        )

versionOption :: Parser (a -> a)
versionOption =
    infoOption (showVersion version) (hidden <> long "version" <> help "Show version.")

backendParser :: Parser Backend
backendParser = GRPC <$> hostParser <*> portParser <*> targetParser <*> retryNumParser <*> tlsParser

hostParser :: Parser HostName
hostParser =
    strOption
        ( long "grpc-ip"
            <> metavar "GRPC-IP"
            <> value "127.0.0.1"
            <> showDefault -- matches the default RPC listen address for the node
            <> help "Address on which the gRPC server is listening. Either an IP address or a hostname."
        )

portParser :: Parser PortNumber
portParser =
    option
        auto
        ( long "grpc-port"
            <> metavar "GRPC-PORT"
            <> value 20000
            <> showDefault -- default value to match the node default GRPC port
            <> help "Port where the gRPC server is listening."
        )

targetParser :: Parser (Maybe String)
targetParser =
    optional $
        strOption
            ( hidden
                <> long "grpc-target"
                <> metavar "GRPC-TARGET"
                <> help "Target node name when using a proxy."
            )

retryNumParser :: Parser Int
retryNumParser =
    option
        auto
        ( hidden
            <> long "grpc-retry"
            <> value 0
            <> showDefault -- default is to only try once
            <> metavar "GRPC-RETRY"
            <> help "How many times to retry the connection if it fails the first time."
        )

tlsParser :: Parser Bool
tlsParser =
    switch
        ( long "secure"
            <> help "Enable TLS."
        )

-- | Parse transactionOpts with an optional energy flag
transactionOptsParser :: Parser (TransactionOpts (Maybe Energy))
transactionOptsParser =
    transactionOptsParserBuilder $
        optional (option (eitherReader energyFromStringInform) (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction."))

-- | Parse transactionOpts with a required energy flag
requiredEnergyTransactionOptsParser :: Parser (TransactionOpts Energy)
requiredEnergyTransactionOptsParser =
    transactionOptsParserBuilder $
        option (eitherReader energyFromStringInform) (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction.")

-- | Helper function to build an transactionOptsParser with or without a required energy flag
transactionOptsParserBuilder :: Parser energyOrMaybe -> Parser (TransactionOpts energyOrMaybe)
transactionOptsParserBuilder energyOrMaybeParser =
    TransactionOpts
        <$> optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender."))
        <*> optional (option (eitherReader aliasFromStringInform) (long "alias" <> metavar "ALIAS" <> help "Which alias to use as the sender address."))
        <*>
        -- TODO Specify / refer to format of JSON file when new commands (e.g. account add-keys) that accept same format are
        -- added.
        optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file."))
        <*> optional (strOption (long "signers" <> metavar "SIGNERS" <> help "Specification of which (local) keys to sign with. Example: \"0:1,0:2,3:0,3:1\" specifies that credential holder 0 signs with keys 1 and 2, while credential holder 3 signs with keys 0 and 1"))
        <*> optional (option auto (long "nonce" <> metavar "NONCE" <> help "Transaction nonce."))
        <*> energyOrMaybeParser
        <*> optional (strOption (long "expiry" <> metavar "EXPIRY" <> help "Expiration time of a transaction, specified as a relative duration (\"30s\", \"5m\", etc.) or UNIX epoch timestamp."))
        <*> optional (strOption (long "out" <> metavar "FILE" <> help "File to output the signed/partially-signed transaction to instead of submitting the transaction on-chain."))
        <*> interactionOptsParser

interactionOptsParser :: Parser InteractionOpts
interactionOptsParser =
    InteractionOpts
        <$> (not <$> switch (long "no-confirm" <> help "Do not ask for confirmation before proceeding to send the transaction."))
        <*> (not <$> switch (long "no-wait" <> help "Exit right after sending the transaction without waiting for it to be committed and finalized."))

programOptions :: Parser Options
programOptions =
    Options
        <$> ( hsubparser
                ( metavar "command"
                    <> transactionCmds
                    <> accountCmds
                    <> moduleCmds
                    <> contractCmds
                    <> configCmds
                    <> consensusCmds
                    <> blockCmds
                    <> bakerCmds "validator"
                    <> delegatorCmds
                    <> identityCmds
                    <> poolCmds
                    <> rawCmds
                )
                <|> hsubparser
                    ( metavar "command"
                        <> bakerCmds "baker"
                        <> internal
                    )
            )
        <*> backendParser
        <*> optional (strOption (long "config" <> metavar "DIR" <> help "Path to the configuration directory."))
        <*> switch (hidden <> long "verbose" <> short 'v' <> help "Make output verbose.")

rawCmds :: Mod CommandFields Cmd
rawCmds =
    command
        "raw"
        ( info
            (LegacyCmd <$> legacyProgramOptions)
            (progDesc "Commands that directly expose the node's GRPC interface.")
        )

transactionCmds :: Mod CommandFields Cmd
transactionCmds =
    command
        "transaction"
        ( info
            ( TransactionCmd
                <$> hsubparser
                    ( transactionSubmitCmd
                        <> transactionAddSignatureCmd
                        <> transactionStatusCmd
                        <> transactionSendCcdCmd
                        <> transactionWithScheduleCmd
                        <> transactionDeployCredentialCmd
                        <> transactionRegisterDataCmd
                        <> pltCmds
                    )
            )
            (progDesc "Commands for submitting and inspecting transactions.")
        )

pltCmds :: Mod CommandFields TransactionCmd
pltCmds =
    command
        "plt"
        ( info
            ( PLTCmd
                <$> hsubparser
                    ( transactionPLTTransferCmd
                        <> transactionPLTMintCmd
                        <> transactionPLTBurnCmd
                        <> transactionPLTAddAllowListCmd
                        <> transactionPLTAddDenyListCmd
                        <> transactionPLTRemoveAllowListCmd
                        <> transactionPLTRemoveDenyListCmd
                    )
            )
            (progDesc "Commands for PLTs (protocol level tokens) transactions.")
        )

transactionSubmitCmd :: Mod CommandFields TransactionCmd
transactionSubmitCmd =
    command
        "submit"
        ( info
            ( TransactionSubmit
                <$> strArgument (metavar "FILE" <> help "File containing a signed transaction in JSON format.")
                <*> interactionOptsParser
            )
            ( progDescDoc $
                docFromLines $
                    [ "Parse signed transaction and send it to the node.",
                      "Expected format of the signed transaction in the `FILE`:"
                    ]
                        ++ expectedSignedTransactionFormat
            )
        )

transactionAddSignatureCmd :: Mod CommandFields TransactionCmd
transactionAddSignatureCmd =
    command
        "add-signature"
        ( info
            ( TransactionAddSignature
                <$> strArgument (metavar "FILE" <> help "File containing a signed transaction in JSON format.")
                <*> optional
                    ( strOption
                        ( long "signers" <> metavar "SIGNERS" <> help "Specification of which (local) keys to sign with. Example: \"0:1,0:2,3:0,3:1\" specifies that credential holder 0 signs with keys 1 and 2, while credential holder 3 signs with keys 0 and 1"
                        )
                    )
                <*> optional
                    (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file."))
            )
            ( progDescDoc $
                docFromLines $
                    [ "Adds a signature to the transaction in the file.",
                      "Expected format of the signed transaction in the `FILE`:"
                    ]
                        ++ expectedSignedTransactionFormat
                        ++ [ "Expected format of the keys in the `KEYS` file:"
                           ]
                        ++ expectedKeysFileFormat
            )
        )

expectedSignedTransactionFormat :: [String]
expectedSignedTransactionFormat =
    [ "   {",
      "     \"energy\": 5000,",
      "     \"expiry\": 1715708777,",
      "     \"nonce\": 12,",
      "     \"payload\": {",
      "       \"address\": {",
      "         \"index\": 3383,",
      "         \"subindex\": 0",
      "       },",
      "       \"amount\": \"0\",",
      "       \"message\": \"01000101420c0000000000000000000000000000\",",
      "       \"receiveName\": \"cis2-bridgeable.updateOperator\"",
      "       \"transactionType\": \"update\"",
      "     },",
      "     \"sender\": \"4jxvYasaPncfmCFCLZCvuL5cZuvR5HAQezCHZH7ZA7AGsRYpix\",",
      "     \"signature\": {",
      "       \"0\": {",
      "         \"0\": \"6f17c110965054b262ef0d6dee02f77dccb7bd031c2af324b544f5ee3e6e18b3fd1be8a95782e92a89dd40a1b69cad8a37e8b86fc9107c8528d8267212cf030b\"",
      "       }",
      "     },",
      "     \"version\": 1",
      "    }"
    ]

transactionDeployCredentialCmd :: Mod CommandFields TransactionCmd
transactionDeployCredentialCmd =
    command
        "deploy-credential"
        ( info
            ( TransactionDeployCredential
                <$> strArgument (metavar "FILE" <> help "File containing the credential deployment information.")
                <*> interactionOptsParser
            )
            (progDesc "Parse credential and send it to the node.")
        )

transactionStatusCmd :: Mod CommandFields TransactionCmd
transactionStatusCmd =
    command
        "status"
        ( info
            ( TransactionStatus
                <$> strArgument (metavar "TX-HASH" <> help "Hash of the transaction.")
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file used to display smart contract events."))
            )
            (progDesc "Get status of a transaction.")
        )

transactionSendCcdCmd :: Mod CommandFields TransactionCmd
transactionSendCcdCmd =
    command
        "send"
        ( info
            ( TransactionSendCcd
                <$> strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.")
                <*> option (eitherReader amountFromStringInform) (long "amount" <> metavar "CCD-AMOUNT" <> help "Amount of CCDs to send.")
                <*> memoInputParser
                <*> transactionOptsParser
            )
            (progDesc "Transfer CCD from one account to another.")
        )

transactionPLTTransferCmd :: Mod CommandFields PLTCmd
transactionPLTTransferCmd =
    command
        "send"
        ( info
            ( TransactionPLTTransfer
                <$> strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.")
                <*> option (eitherReader preTokenAmountFromStringInform) (long "amount" <> metavar "TOKEN-AMOUNT" <> help "Amount of tokens to send.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> memoInputParser
                <*> transactionOptsParser
            )
            (progDesc "Transfer tokens from one account to another.")
        )

transactionPLTMintCmd :: Mod CommandFields PLTCmd
transactionPLTMintCmd =
    command
        "mint"
        ( info
            ( TransactionPLTUpdateSupply Mint
                <$> option (eitherReader preTokenAmountFromStringInform) (long "amount" <> metavar "TOKEN-AMOUNT" <> help "Amount of tokens to send.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Mint PLTs (protocol level tokens).")
        )

transactionPLTBurnCmd :: Mod CommandFields PLTCmd
transactionPLTBurnCmd =
    command
        "burn"
        ( info
            ( TransactionPLTUpdateSupply Burn
                <$> option (eitherReader preTokenAmountFromStringInform) (long "amount" <> metavar "TOKEN-AMOUNT" <> help "Amount of tokens to send.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Burn PLTs (protocol level tokens).")
        )

transactionPLTAddAllowListCmd :: Mod CommandFields PLTCmd
transactionPLTAddAllowListCmd =
    command
        "add-to-allow-list"
        ( info
            ( TransactionPLTModifyList AddAllowList
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "The account to add to the list.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Add an account to the allow list.")
        )

transactionPLTAddDenyListCmd :: Mod CommandFields PLTCmd
transactionPLTAddDenyListCmd =
    command
        "add-to-deny-list"
        ( info
            ( TransactionPLTModifyList AddDenyList
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "The account to add to the list.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Add an account to the deny list.")
        )

transactionPLTRemoveAllowListCmd :: Mod CommandFields PLTCmd
transactionPLTRemoveAllowListCmd =
    command
        "remove-from-allow-list"
        ( info
            ( TransactionPLTModifyList RemoveAllowList
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "The account to remove from the list.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Remove an account from the allow list.")
        )

transactionPLTRemoveDenyListCmd :: Mod CommandFields PLTCmd
transactionPLTRemoveDenyListCmd =
    command
        "remove-from-deny-list"
        ( info
            ( TransactionPLTModifyList RemoveDenyList
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "The account to remove from the list.")
                <*> strOption (long "tokenId" <> metavar "TOKEN_ID" <> help "ID of the token.")
                <*> transactionOptsParser
            )
            (progDesc "Remove an account from the deny list.")
        )

transactionWithScheduleCmd :: Mod CommandFields TransactionCmd
transactionWithScheduleCmd =
    command
        "send-scheduled"
        ( info
            ( let implicit =
                    (\a b c d -> Left (a, b, c, d))
                        <$> option (eitherReader amountFromStringInform) (long "amount" <> metavar "CCD-AMOUNT" <> help "Total amount of CCD to send.")
                        <*> option auto (long "every" <> metavar "INTERVAL" <> help "Interval between releases, one of 'Minute', 'Hour', 'Day', 'Week', 'Month' (30 days), or 'Year' (365 days). ")
                        <*> option auto (long "for" <> metavar "N" <> help "Number of releases.")
                        <*> option
                            (eitherReader timeFromString)
                            ( long "starting"
                                <> metavar "starting"
                                <> help "Start time of the first release, as a ISO8601 UTC time string, e.g., 2021-12-31T00:00:00Z."
                            )
                  explicit =
                    Right
                        <$> option (eitherReader eitherParseScheduleInform) (long "schedule" <> metavar "schedule" <> help "Explicit schedule in the form of a comma separated list of elements of the form '3.0 at 2020-12-13T23:35:59Z' (send 3 CCD on December 13, 2020). Timestamps must be given in UTC.")
              in  TransactionSendWithSchedule
                    <$> strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.")
                    <*> (implicit <|> explicit)
                    <*> memoInputParser
                    <*> transactionOptsParser
            )
            ( progDescDoc . Just $
                fillCat
                    [ "Transfer CCD from one account to another with the provided schedule of releases.",
                      "Releases can be specified in one of two ways, either as regular releases via intervals,"
                        <> softline
                        <> "or as an explicit schedule at specific timestamps.",
                      "",
                      "To specify the release via regular intervals use the options" <> softline <> "'--amount', '--every', '--for', and '--starting'.",
                      "To specify an explicit schedule provide a list of releases in"
                        <> softline
                        <> "the form of a '--schedule' flag, which takes a comma separated list of releases.",
                      "Each release must be of the form 100 at 2020-12-13T23:23:23Z.",
                      "",
                      "For example, to supply three releases, of 100, 150, and 200 CCD,"
                        <> softline
                        <> "on January 1, 2021, February 15, 2021, and December 31, 2021,"
                        <> softline
                        <> "the following input would be used. All releases are at the beginning of"
                        <> softline
                        <> "the day in the UTC time zone.",
                      "",
                      hang 4 "\"100 at 2021-01-01T00:00:00Z, 150 at 2021-02-15T00:00:00Z, 200 at 2021-12-31T00:00:00Z\"",
                      "",
                      "Times are parsed according to the ISO8601 standard for the UTC time zone."
                    ]
            )
        )
  where
    eitherParseScheduleInform :: String -> Either String [(Timestamp, Amount)]
    eitherParseScheduleInform s =
        let items = map Data.Text.words (splitOn "," (pack s))
        in  forM items $ \case
                [amountString, "at", timeString] ->
                    case amountFromString (unpack amountString) of
                        Nothing -> Left "Could not parse amount."
                        Just amount ->
                            case iso8601ParseM (unpack timeString) of
                                Nothing -> Left "Could not parse a timestamp."
                                Just time -> return (utcTimeToTimestamp time, amount)
                _ -> Left "Could not parse schedule. It should be a comma separated list of the form '3.0 at 2020-12-13T23:23:23Z'."

    timeFromString :: String -> Either String Timestamp
    timeFromString s =
        case iso8601ParseM s of
            Nothing -> Left "Starting point could not be read."
            Just time -> return (utcTimeToTimestamp time)

transactionRegisterDataCmd :: Mod CommandFields TransactionCmd
transactionRegisterDataCmd =
    command
        "register-data"
        ( info
            ( TransactionRegisterData
                <$> registerDataParser
                <*> transactionOptsParser
            )
            (progDesc "Register data on the chain.")
        )

accountCmds :: Mod CommandFields Cmd
accountCmds =
    command
        "account"
        ( info
            ( AccountCmd
                <$> hsubparser
                    ( accountShowCmd
                        <> accountListCmd
                        <> accountUpdateKeysCmd
                        <> accountUpdateCredentialsCmd
                        <> accountDecryptCmd
                        <> accountShowAliasCmd
                    )
            )
            (progDesc "Commands for inspecting and modifying accounts.")
        )

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
    command
        "show"
        ( info
            ( AccountShow
                <$> optional (strArgument (metavar "ACCOUNT" <> help "Name, credential registration ID, account index, or address of the account."))
                <*> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
                <*> switch (long "shielded" <> help "Show the shielded balance.")
                <*> switch (long "reveal-shielded" <> help "Show the shielded balance, and also reveal all the amounts (potentially slow).")
            )
            (progDesc "Display account details.")
        )

accountListCmd :: Mod CommandFields AccountCmd
accountListCmd =
    command
        "list"
        ( info
            ( AccountList
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "List all accounts.")
        )

accountDecryptCmd :: Mod CommandFields AccountCmd
accountDecryptCmd =
    command
        "unshield"
        ( info
            ( AccountDecrypt
                <$> transactionOptsParser
                <*> option (maybeReader amountFromString) (long "amount" <> metavar "CCD-AMOUNT" <> help "The amount to transfer to public balance.")
                <*> optional (option auto (long "index" <> metavar "INDEX" <> help "Optionally specify the index up to which shielded amounts should be combined."))
            )
            (progDesc "Transfer an amount from shielded to public balance of the account.")
        )

accountUpdateKeysCmd :: Mod CommandFields AccountCmd
accountUpdateKeysCmd =
    command
        "update-keys"
        ( info
            ( AccountUpdateKeys
                <$> strArgument (metavar "FILE" <> help "File containing the new account keys.")
                <*> option (eitherReader credIdFromStringInform) (long "credId" <> metavar "CRED-ID" <> help "The credential registration id of the credential whose keys we want to update.")
                <*> transactionOptsParser
            )
            ( progDescDoc $
                docFromLines
                    [ "Set keys for the credential. Expected format of the key file:",
                      "   {",
                      "     \"keys\" : {",
                      "       idx: {",
                      "         \"verifyKey\": ...",
                      "       },",
                      "       ...",
                      "     },",
                      "     \"threshold\": number",
                      "   }",
                      "where each key is given a numeric index idx and the threshold is at most the number of keys.",
                      "This replaces all existing keys for the credential with the new set."
                    ]
            )
        )

accountUpdateCredentialsCmd :: Mod CommandFields AccountCmd
accountUpdateCredentialsCmd =
    command
        "update-credentials"
        ( info
            ( AccountUpdateCredentials
                <$> optional (strOption (long "new-credentials" <> metavar "FILE" <> help "File containing the new credential deployment informations."))
                <*> optional (strOption (long "remove-credentials" <> metavar "FILE" <> help "File containing credential registration ids of the credentials to be removed."))
                <*> option auto (long "new-threshold" <> metavar "THRESHOLD" <> help "New account threshold, i.e. how many credential holders needed to sign a transaction.")
                <*> transactionOptsParser
            )
            ( progDescDoc $
                docFromLines
                    [ "Add and/or remove credentials, and update account threshold. Expected format of the file containing new credentials:",
                      "   {",
                      "     \"cidx\": {",
                      "       \"arData\": ...,",
                      "       \"credId\": ...,",
                      "       \"credentialPublicKeys\": {",
                      "           \"keys\": {",
                      "             \"kidx\": {",
                      "                 \"schemeId\": ...,",
                      "                 \"verifyKey\": ...,",
                      "              },",
                      "              ...",
                      "           },",
                      "           \"threshold\": ...",
                      "       },",
                      "       \"ipIdentity\": ...,",
                      "       \"policy\": ...,",
                      "       \"proofs\": ...,",
                      "       \"revocationThreshold\": ...,",
                      "     },",
                      "     ...",
                      "   }",
                      "where cidx is the credential index and kidx is the key index of a verify key."
                    ]
            )
        )

accountShowAliasCmd :: Mod CommandFields AccountCmd
accountShowAliasCmd =
    command
        "show-alias"
        ( info
            ( AccountShowAlias
                <$> strArgument (metavar "ACCOUNT" <> help "Name or address of the account.")
                <*> option (eitherReader aliasFromStringInform) (long "alias" <> metavar "ALIAS" <> help "Which alias to generate.")
            )
            (progDesc "Generate an alias based on an account address and counter.")
        )

moduleCmds :: Mod CommandFields Cmd
moduleCmds =
    command
        "module"
        ( info
            ( ModuleCmd
                <$> hsubparser
                    ( moduleDeployCmd
                        <> moduleListCmd
                        <> moduleShowCmd
                        <> moduleInspectCmd
                        <> moduleNameCmd
                        <> moduleRemoveNameCmd
                    )
            )
            (progDesc "Commands for inspecting and deploying modules.")
        )

-- | Parse a contract version.
contractVersionOption :: Parser Wasm.WasmVersion
contractVersionOption =
    option
        (eitherReader contractVersionFromStringInform)
        ( long "contract-version"
            <> metavar "CONTRACT-VERSION"
            <> help "Optional version of the module (should only be used for modules built with cargo-concordium version < 2)."
        )

moduleDeployCmd :: Mod CommandFields ModuleCmd
moduleDeployCmd =
    command
        "deploy"
        ( info
            ( ModuleDeploy
                <$> strArgument (metavar "FILE" <> help "Path to the smart contract module.")
                <*> optional (strOption (long "name" <> metavar "NAME" <> help "Name for the module."))
                <*> optional contractVersionOption
                <*> transactionOptsParser
            )
            (progDesc "Deploy a smart contract module on the chain, optionally naming the module.")
        )

moduleListCmd :: Mod CommandFields ModuleCmd
moduleListCmd =
    command
        "list"
        ( info
            ( ModuleList
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "List all modules.")
        )

moduleShowCmd :: Mod CommandFields ModuleCmd
moduleShowCmd =
    command
        "show"
        ( info
            ( ModuleShow
                <$> strArgument (metavar "MODULE-OR-NAME" <> help "Module reference OR a module name.")
                <*> strOption (long "out" <> metavar "FILE" <> help "File to output the source code to (use '-' for stdout).")
                <*> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Get the source code for a module.")
        )

moduleInspectCmd :: Mod CommandFields ModuleCmd
moduleInspectCmd =
    command
        "inspect"
        ( info
            ( ModuleInspect
                <$> strArgument (metavar "MODULE-OR-NAME" <> help "Module reference OR a module name.")
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to display the type signatures of the functions."))
                <*> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Show the functions from a module, including type signatures if a schema is provided.")
        )

moduleNameCmd :: Mod CommandFields ModuleCmd
moduleNameCmd =
    command
        "name"
        ( info
            ( ModuleName
                <$> strArgument (metavar "MODULE" <> help "Module reference OR path to the module.")
                <*> strOption (long "name" <> metavar "NAME" <> help "Name for the module.")
                <*> optional contractVersionOption
            )
            (progDesc "Name a module.")
        )

moduleRemoveNameCmd :: Mod CommandFields ModuleCmd
moduleRemoveNameCmd =
    command
        "remove-name"
        ( info
            ( ModuleRemoveName
                <$> strArgument (metavar "NAME" <> help "The module-name to forget")
            )
            ( progDescDoc $
                docFromLines
                    ["Removes the given name from the list of named modules"]
            )
        )

contractCmds :: Mod CommandFields Cmd
contractCmds =
    command
        "contract"
        ( info
            ( ContractCmd
                <$> hsubparser
                    ( contractShowCmd
                        <> contractListCmd
                        <> contractInitCmd
                        <> contractUpdateCmd
                        <> contractInvokeCmd
                        <> contractNameCmd
                        <> contractRemoveNameCmd
                    )
            )
            (progDesc "Commands for inspecting and initializing smart contracts.")
        )

contractShowCmd :: Mod CommandFields ContractCmd
contractShowCmd =
    command
        "show"
        ( info
            ( ContractShow
                <$> strArgument (metavar "INDEX-OR-NAME" <> help "Index of the contract address OR a contract name.")
                <*> optional
                    ( option
                        auto
                        ( long "subindex"
                            <> metavar "SUBINDEX"
                            <> help "Subindex of address for the contract (default: 0)"
                        )
                    )
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to display the contract info."))
                <*> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Display contract state at given block.")
        )

contractListCmd :: Mod CommandFields ContractCmd
contractListCmd =
    command
        "list"
        ( info
            ( ContractList
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "List all contracts on a specific block.")
        )

contractInitCmd :: Mod CommandFields ContractCmd
contractInitCmd =
    command
        "init"
        ( info
            ( ContractInit
                <$> strArgument (metavar "MODULE" <> help "Module reference OR module name OR (if --path is used) path to a module.")
                <*> strOption
                    ( long "contract"
                        <> metavar "CONTRACT-NAME"
                        <> help "Name of the contract (i.e. init function) in the module."
                    )
                <*> parameterFileParser
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to parse the params file."))
                <*> optional (strOption (long "name" <> metavar "NAME" <> help "Name for the contract."))
                <*> switch (long "path" <> help "Use when MODULE is a path to a module file.")
                <*> optional contractVersionOption
                <*> option
                    (eitherReader amountFromStringInform)
                    ( long "amount"
                        <> metavar "CCD-AMOUNT"
                        <> value 0
                        <> help "Amount of CCD to transfer to the contract."
                    )
                <*> requiredEnergyTransactionOptsParser
            )
            (progDesc "Initialize contract from already deployed module, optionally naming the contract.")
        )

contractUpdateCmd :: Mod CommandFields ContractCmd
contractUpdateCmd =
    command
        "update"
        ( info
            ( ContractUpdate
                <$> strArgument (metavar "INDEX-OR-NAME" <> help "Index of the contract address OR a contract name.")
                <*> optional
                    ( option
                        auto
                        ( long "subindex"
                            <> metavar "SUBINDEX"
                            <> help "Subindex of address for the contract on chain (default: 0)"
                        )
                    )
                <*> strOption
                    ( long "entrypoint"
                        <> metavar "ENTRYPOINT-NAME"
                        <> help "Name of the entrypoint of the contract to invoke."
                    )
                <*> parameterFileParser
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to parse the params file."))
                <*> option
                    (eitherReader amountFromStringInform)
                    ( long "amount"
                        <> metavar "CCD-AMOUNT"
                        <> value 0
                        <> help "Amount of CCD to transfer to the contract."
                    )
                <*> requiredEnergyTransactionOptsParser
            )
            (progDesc "Update an existing contract.")
        )

contractInvokeCmd :: Mod CommandFields ContractCmd
contractInvokeCmd =
    command
        "invoke"
        ( info
            ( ContractInvoke
                <$> strArgument (metavar "INDEX-OR-NAME" <> help "Index of the contract address OR a contract name.")
                <*> optional
                    ( option
                        auto
                        ( long "subindex"
                            <> metavar "SUBINDEX"
                            <> help "Subindex of address for the contract on chain (default: 0)"
                        )
                    )
                <*> strOption
                    ( long "entrypoint"
                        <> metavar "ENTRYPOINT-NAME"
                        <> help "Name of the entrypoint of the contract to invoke."
                    )
                <*> parameterFileParser
                <*> optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to parse the params file."))
                <*> option
                    (eitherReader amountFromStringInform)
                    ( long "amount"
                        <> metavar "CCD-AMOUNT"
                        <> value 0
                        <> help "Amount of CCD to transfer to the contract."
                    )
                <*> invokerParser
                <*> optional (option (eitherReader energyFromStringInform) (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on the invocation. (default: 10,000,000)"))
                <*> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Simulate an invocation of an existing contract.")
        )

contractNameCmd :: Mod CommandFields ContractCmd
contractNameCmd =
    command
        "name"
        ( info
            ( ContractName
                <$> argument auto (metavar "INDEX" <> help "Index of the contract address to be named.")
                <*> optional
                    ( option
                        auto
                        ( long "subindex"
                            <> metavar "SUBINDEX"
                            <> help "Subindex of contract address to be named (default: 0)"
                        )
                    )
                <*> strOption (long "name" <> metavar "NAME" <> help "Name for the contract.")
            )
            (progDesc "Name a contract.")
        )

contractRemoveNameCmd :: Mod CommandFields ContractCmd
contractRemoveNameCmd =
    command
        "remove-name"
        ( info
            ( ContractRemoveName
                <$> strArgument (metavar "NAME" <> help "The contract-name to forget")
            )
            ( progDescDoc $
                docFromLines
                    ["Removes the given name from the list of named contracts"]
            )
        )

configCmds :: Mod CommandFields Cmd
configCmds =
    command
        "config"
        ( info
            ( ConfigCmd
                <$> hsubparser
                    ( configInitCmd
                        <> configShowCmd
                        <> configBackupExportCmd
                        <> configBackupImportCmd
                        <> configAccountCmds
                    )
            )
            (progDesc "Commands for inspecting and changing local configuration.")
        )

configInitCmd :: Mod CommandFields ConfigCmd
configInitCmd =
    command
        "init"
        ( info
            (pure ConfigInit)
            (progDesc "Initialize configuration.")
        )

configShowCmd :: Mod CommandFields ConfigCmd
configShowCmd =
    command
        "show"
        ( info
            (pure ConfigShow)
            (progDesc "Show configuration.")
        )

configBackupExportCmd :: Mod CommandFields ConfigCmd
configBackupExportCmd =
    command
        "export-backup"
        ( info
            ( ConfigBackupExport
                <$> strArgument (metavar "FILE" <> help "Name for backup file")
            )
            (progDesc "Save config to backup file, optionally encrypted with a password")
        )

configBackupImportCmd :: Mod CommandFields ConfigCmd
configBackupImportCmd =
    command
        "import-backup"
        ( info
            ( ConfigBackupImport
                <$> strArgument (metavar "FILE" <> help "Backup file name")
                <*> switch (long "skip-existing" <> short 's' <> help "Automatically skip importing accounts when the keydirectory already exists")
            )
            (progDesc "Import config backup file, requires password if encrypted")
        )

configAccountCmds :: Mod CommandFields ConfigCmd
configAccountCmds =
    command
        "account"
        ( info
            ( ConfigAccountCmd
                <$> hsubparser
                    ( configAccountNameCmd
                        <> configAccountRemove
                        <> configAccountImportCmd
                        <> configAccountAddKeysCmd
                        <> configAccountUpdateKeysCmd
                        <> configAccountChangeKeyPasswordCmd
                        <> configAccountRemoveKeysCmd
                        <> configAccountRemoveNameCmd
                    )
            )
            (progDesc "Commands for inspecting and changing account-specific configuration.")
        )

configAccountNameCmd :: Mod CommandFields ConfigAccountCmd
configAccountNameCmd =
    command
        "name"
        ( info
            ( ConfigAccountName
                <$> strArgument (metavar "ADDRESS" <> help "Address of the account.")
                <*> strOption (long "name" <> metavar "NAME" <> help "Name of the account.")
            )
            (progDesc "Adds a named account address to persistent config")
        )

configAccountRemove :: Mod CommandFields ConfigAccountCmd
configAccountRemove =
    command
        "remove"
        ( info
            ( ConfigAccountRemove
                <$> strArgument (metavar "ACCOUNT" <> help "Name or address of the account.")
            )
            (progDesc "Remove the account from the persistent config.")
        )

configAccountImportCmd :: Mod CommandFields ConfigAccountCmd
configAccountImportCmd =
    command
        "import"
        ( info
            ( ConfigAccountImport
                <$> strArgument (metavar "FILE" <> help "File with one or more accounts exported from the wallet.")
                <*> optional (strOption (long "name" <> metavar "NAME" <> help nameOptionHelp))
                <*> option readAccountExportFormat (long "format" <> metavar "FORMAT" <> value FormatMobile <> help "Export format. Supported values are 'mobile' and 'genesis' (default: 'mobile').")
                <*> switch (long "skip-existing" <> short 's' <> help "Automatically skip importing accounts when the keydirectory already exists")
            )
            (progDesc "Import an account to persistent config.")
        )
  where
    nameOptionHelp = "Name of the account. For the 'genesis' format, this sets the name to assign to the account. For the 'mobile' format (which contains multiple already named accounts), it selects which account to import."

configAccountAddKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountAddKeysCmd =
    command
        "add-keys"
        ( info
            ( ConfigAccountAddKeys
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.")
                <*> strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file.")
            )
            ( progDescDoc $
                docFromLines $
                    [ "Add one or several key pairs to a specific account configuration.",
                      "Expected format of the key file:"
                    ]
                        ++ expectedKeysFileFormat
            )
        )

configAccountUpdateKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountUpdateKeysCmd =
    command
        "update-keys"
        ( info
            ( ConfigAccountUpdateKeys
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.")
                <*> strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file.")
            )
            ( progDescDoc $
                docFromLines $
                    [ "Update one or several key pairs to a specific account configuration.",
                      "Expected format of the key file:"
                    ]
                        ++ expectedKeysFileFormat
            )
        )

expectedKeysFileFormat :: [String]
expectedKeysFileFormat =
    [ "   {",
      "     \"cidx\": {",
      "        \"kidx\": {",
      "          \"encryptedSignKey\": {",
      "            \"metadata\": {",
      "              \"encryptionMethod\": \"AES-256\",",
      "              \"iterations\": ...,",
      "              \"salt\": ...,",
      "              \"initializationVector\": ...,",
      "              \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\"",
      "            },",
      "            \"cipherText\": ...",
      "          },",
      "          \"verifyKey\": ...,",
      "          \"schemeId\": \"Ed25519\"",
      "        },",
      "        ...",
      "     },",
      "     ...",
      "   }",
      "where cidx is the credential index and kidx is the index of a key pair."
    ]

configAccountChangeKeyPasswordCmd :: Mod CommandFields ConfigAccountCmd
configAccountChangeKeyPasswordCmd =
    command
        "change-password"
        ( info
            ( ConfigAccountChangeKeyPassword
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Name or Address of account to update the keypair of")
                <*> option (eitherReader credentialIndexFromStringInform) (long "cred-index" <> metavar "CREDINDEX" <> help "Credential index for the key to update password for")
                <*> option (eitherReader indexFromStringInform) (long "key-index" <> metavar "KEYINDEX" <> help "Key index to update password for")
            )
            (progDesc "Change the password used to encrypt the account keys")
        )

configAccountRemoveKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountRemoveKeysCmd =
    command
        "remove-keys"
        ( info
            ( ConfigAccountRemoveKeys
                <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.")
                <*> option (eitherReader credentialIndexFromStringInform) (long "credential-index" <> metavar "CREDENTIALINDEX" <> help "Index of the credential containing the keys to remove")
                <*> some (argument auto (metavar "KEYINDICES" <> help "space-separated list of indices of the keys to remove."))
            )
            ( progDescDoc $
                docFromLines
                    [ "Removes the keys from the account at the specified indices.",
                      "The --threshold option may be used to update the signature threshold."
                    ]
            )
        )

readAccountExportFormat :: ReadM AccountExportFormat
readAccountExportFormat =
    str >>= \case
        "mobile" -> return FormatMobile
        "genesis" -> return FormatGenesis
        s -> readerError $ printf "invalid format: %s (supported values: 'mobile' and 'genesis')" (s :: String)

configAccountRemoveNameCmd :: Mod CommandFields ConfigAccountCmd
configAccountRemoveNameCmd =
    command
        "remove-name"
        ( info
            ( ConfigAccountRemoveName
                <$> strArgument (metavar "NAME" <> help "Name of the account")
            )
            ( progDescDoc $
                docFromLines
                    ["Removes the given name from the list of named accounts"]
            )
        )

consensusCmds :: Mod CommandFields Cmd
consensusCmds =
    command
        "consensus"
        ( info
            ( ConsensusCmd
                <$> ( hsubparser
                        ( consensusStatusCmd
                            <> consensusDetailedStatusCmd
                            <> consensusShowChainParametersCmd
                            <> consensusShowParametersCmd
                        )
                        <|> hsubparser
                            (commandGroup "internal" <> consensusChainUpdateCmd <> internal)
                    )
            )
            (progDesc "Commands for inspecting chain health (branching, finalization), block content/history (including listing transactions), election (Birk) and reward/minting parameters.")
        )

consensusStatusCmd :: Mod CommandFields ConsensusCmd
consensusStatusCmd =
    command
        "status"
        ( info
            (pure ConsensusStatus)
            (progDesc "List various parameters related to the current state of the consensus protocol.")
        )

consensusDetailedStatusCmd :: Mod CommandFields ConsensusCmd
consensusDetailedStatusCmd =
    command
        "detailed-status"
        ( info
            ( ConsensusDetailedStatus
                <$> optional (option auto (long "genesis-index" <> metavar "GENINDEX" <> help "Genesis index (defaults to latest)"))
            )
            (progDesc "Show detailed consensus status information.")
        )

consensusShowParametersCmd :: Mod CommandFields ConsensusCmd
consensusShowParametersCmd =
    command
        "show-parameters"
        ( info
            ( ConsensusShowParameters
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
                <*> ( switch (internal <> long "include-bakers" <> includeHelp)
                        <|> switch (hidden <> long "include-validators" <> includeHelp)
                    )
            )
            (progDesc "Show election parameters for given block.")
        )
  where
    includeHelp = help "Include the \"lottery power\" of individual validators."

consensusShowChainParametersCmd :: Mod CommandFields ConsensusCmd
consensusShowChainParametersCmd =
    command
        "show-chain-parameters"
        ( info
            ( ConsensusShowChainParameters
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Show chain parameters for given block.")
        )

consensusChainUpdateCmd :: Mod CommandFields ConsensusCmd
consensusChainUpdateCmd =
    command
        "chain-update"
        ( info
            ( ConsensusChainUpdate
                <$> strArgument (metavar "UPDATE" <> help "File containing the update command in JSON format.")
                <*> some (strOption (long "key" <> metavar "FILE" <> help "File containing key-pair to sign the update command. This option can be provided multiple times, once for each key-pair to use."))
                <*> interactionOptsParser
            )
            ( progDescDoc $
                docFromLines
                    [ "Send a chain-update command to the chain.",
                      "For instance, when creating a new Protocol Level Token (PLT), the `UPDATE` file must contain a JSON object structured as follows:",
                      "    {",
                      "      \"seqNumber\":1,",
                      "      \"effectiveTime\":0,",
                      "      \"timeout\":1234,",
                      "      \"payload\":{",
                      "         \"updateType\":\"createPLT\",",
                      "         \"update\":{",
                      "             \"tokenId\":\"EUROSTABLE\",",
                      "             \"tokenModule\":\"6b7eef36dc48bb59ef9290cdbf123dad7e85efa76caf7df1ae8775735f8f59d3\",",
                      "             \"governanceAccount\":\"4FmiTW2L2AccyR9VjzsnpWFSAcohXWf7Vf797i36y526mqiEcp\",",
                      "             \"decimals\":6,",
                      "             \"initializationParameters\":{",
                      "                 \"name\":\"Stablecoin\",",
                      "                 \"metadata\":\"https://myUrl.com\",",
                      "                 \"allowList\":false,",
                      "                 \"denyList\":false,",
                      "                 \"initialSupply\":12345,",
                      "                 \"mintable\":true,",
                      "                 \"burnable\":true",
                      "             }",
                      "         }",
                      "       }",
                      "    }",
                      "Note:",
                      "Creating a new PLT token is effective immediately, hence the `effectiveTime` has to be set to 0.",
                      "All fields are required when creating a new PLT token except the `initialSupply` field which is optional and",
                      "the `allowList`, `denyList`, `mintable`, and `burnable` values which are set to `false` if not present."
                    ]
            )
        )

blockCmds :: Mod CommandFields Cmd
blockCmds =
    command
        "block"
        ( info
            ( BlockCmd
                <$> hsubparser
                    blockShowCmd
            )
            (progDesc "Commands for inspecting individual blocks.")
        )

blockShowCmd :: Mod CommandFields BlockCmd
blockShowCmd =
    command
        "show"
        ( info
            ( BlockShow
                <$> optional (strArgument (metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Show the node's information about a specific block. Note that some fields (e.g. block time) are objective while others (e.g. arrival time) are specific to the particular node being queried.")
        )

bakerCmds :: String -> Mod CommandFields Cmd
bakerCmds name = command name subcommands
  where
    subcommands =
        info
            ( BakerCmd
                <$> hsubparser
                    ( bakerGenerateKeysCmd
                        <> bakerAddCmd
                        <> bakerRemoveCmd
                        <> bakerSetKeysCmd
                        <> bakerUpdateRestakeCmd
                        <> bakerUpdateStakeCmd
                        <> bakerUpdateUpdateMetadataURL
                        <> bakerUpdateOpenDelegationStatus
                        <> bakerConfigureCmd
                        <> bakerGetEarliestWinTime
                    )
            )
            (progDesc "Commands for creating and deploying validator credentials.")

delegatorCmds :: Mod CommandFields Cmd
delegatorCmds =
    command
        "delegator"
        ( info
            ( DelegatorCmd
                <$> hsubparser
                    ( delegatorAddCmd
                        <> delegatorRemoveCmd
                        <> delegatorConfigureCmd
                    )
            )
            (progDesc "Commands for delegation.")
        )

validatorOrBakerId :: Parser (Maybe BakerId)
validatorOrBakerId = common (internal <> long "baker-id") <|> common (long "validator-id")
  where
    common props = optional (option auto (props <> metavar "VALIDATORID" <> help "Optionally provide the validator id to be included with generated validator keys."))

bakerGenerateKeysCmd :: Mod CommandFields BakerCmd
bakerGenerateKeysCmd =
    command
        "generate-keys"
        ( info
            ( BakerGenerateKeys
                <$> optional (strArgument (metavar "FILE" <> help "File to write keys to."))
                <*> validatorOrBakerId
            )
            ( progDescDoc $
                docFromLines
                    [ "Create validator credentials and write them to a file or stdout.",
                      "If the output file is specified secret keys are written to it,",
                      " and public keys are written to the file with the same name but '.pub.json' extension.",
                      "Format:",
                      "    {",
                      "      \"signatureSignKey\": ...,",
                      "      \"signatureVerifyKey\": ...,",
                      "      \"aggregationSignKey\": ...,",
                      "      \"aggregationVerifyKey\": ...,",
                      "      \"electionPrivateKey\": ...,",
                      "      \"electionVerifyKey\": ...",
                      "    }"
                    ]
            )
        )

rangesHelpString :: String -> String
rangesHelpString name =
    "Command 'consensus show-chain-parameters' can be used to determine the range of allowed values for "
        ++ name
        ++ "."

bakerOrValidatorUrl :: Parser String
bakerOrValidatorUrl = strOption (internal <> long "baker-url" <> commonProps) <|> strOption (long "validator-url" <> commonProps)
  where
    commonProps = metavar "URL" <> help "A link to information about the validator."

bakerAddCmd :: Mod CommandFields BakerCmd
bakerAddCmd =
    command
        "add"
        ( info
            ( BakerAdd
                <$> strArgument (metavar "FILE" <> help "File containing the validator credentials.")
                <*> transactionOptsParser
                <*> option (eitherReader amountFromStringInform) (long "stake" <> metavar "CCD-AMOUNT" <> help "The amount of CCD to stake.")
                <*> (not <$> switch (long "no-restake" <> help "If supplied, the earnings will not be added to the validator stake automatically."))
                <*> optional
                    ( ExtraBakerAddData
                        <$> option (eitherReader openStatusFromStringInform) (long "open-delegation-for" <> metavar "SELECTION" <> help helpOpenDelegationFor)
                        <*> bakerOrValidatorUrl
                        <*> option (eitherReader amountFractionFromStringInform) (long "delegation-transaction-fee-commission" <> metavar "DECIMAL-FRACTION" <> help ("Fraction the validator takes in commission from delegators on transaction fee rewards. " ++ rangesHelpString "transaction fee commission"))
                        <*> blockCommission
                        <*> option (eitherReader amountFractionFromStringInform) (long "delegation-finalization-commission" <> metavar "DECIMAL-FRACTION" <> help ("Fraction the validator takes in commission from delegators on finalization rewards. " ++ rangesHelpString "finalization reward commission"))
                    )
                <*> optional (strOption (long "validator-credentials-out" <> metavar "FILE" <> help "File to write the validator credentials to, in case of successful transaction. These can be used to start the node."))
            )
            (progDesc "Deploy validator credentials to the chain.")
        )

-- | Parser for block reward commission. It supports the legacy terminology of "baking-commission" but it is hidden.
blockCommission :: Parser AmountFraction
blockCommission = blockCommissionGen internal "delegation-baking-commission" <|> blockCommissionGen mempty "delegation-block-reward-commission"
  where
    blockCommissionGen modifier name = option (eitherReader amountFractionFromStringInform) (modifier <> long name <> metavar "DECIMAL-FRACTION" <> help ("Fraction the validator takes in commission from delegators on block rewards. " ++ rangesHelpString "block reward commission"))

allowedValuesOpenDelegationForAsString :: String
allowedValuesOpenDelegationForAsString =
    " - 'all' (delegators are allowed to join the pool),\n"
        ++ " - 'existing' (keep the existing delegators, but do not allow new delegators),\n"
        ++ " - 'none' (move existing delegators to passive delegation and do not allow new delegators)"

openStatusFromStringInform :: String -> Either String OpenStatus
openStatusFromStringInform "all" = Right OpenForAll
openStatusFromStringInform "existing" = Right ClosedForNew
openStatusFromStringInform "none" = Right ClosedForAll
openStatusFromStringInform s =
    Left $
        "Unexpected SELECTION '" ++ s ++ "'. The allowed values are:\n" ++ allowedValuesOpenDelegationForAsString

helpOpenDelegationFor :: String
helpOpenDelegationFor =
    "Select whether the validator will allow other parties (delegators) to delegate CCD to the pool. Available values for SELECTION are:\n" ++ allowedValuesOpenDelegationForAsString ++ "."

bakerConfigureCmd :: Mod CommandFields BakerCmd
bakerConfigureCmd =
    command
        "configure"
        ( info
            ( BakerConfigure
                <$> transactionOptsParser
                <*> optional (option (eitherReader amountFromStringInform) (long "stake" <> metavar "CCD-AMOUNT" <> help "The amount of CCD to stake."))
                <*> optional
                    ( flag' True (long "restake" <> help "The earnings will be added to the validator stake automatically.")
                        <|> flag' False (long "no-restake" <> help "The earnings will not be added to the validator stake automatically.")
                    )
                <*> optional (option (eitherReader openStatusFromStringInform) (long "open-delegation-for" <> metavar "SELECTION" <> help helpOpenDelegationFor))
                <*> optional bakerOrValidatorUrl
                <*> optional (option (eitherReader amountFractionFromStringInform) (long "delegation-transaction-fee-commission" <> metavar "DECIMAL-FRACTION" <> help ("Fraction the validator takes in commission from delegators on transaction fee rewards. " ++ rangesHelpString "transaction fee commission")))
                <*> optional blockCommission
                <*> optional (option (eitherReader amountFractionFromStringInform) (long "delegation-finalization-commission" <> metavar "DECIMAL-FRACTION" <> help ("Fraction the validator takes in commission from delegators on finalization rewards. " ++ rangesHelpString "finalization reward commission")))
                <*> optional
                    ( flag' True (long "suspend" <> help "Suspend the validator. The validator will not participate in the consensus anymore.")
                        <|> flag' False (long "resume" <> help "Resume the validator.")
                    )
                <*> optional (strOption (long "keys-in" <> metavar "FILE" <> help "File containing validator credentials."))
                <*> optional (strOption (long "keys-out" <> metavar "FILE" <> help "File to write updated validator credentials to, in case of successful transaction. These can be used to start the node."))
            )
            (progDesc "Add a new validator, update or remove an existing one.")
        )

bakerUpdateUpdateMetadataURL :: Mod CommandFields BakerCmd
bakerUpdateUpdateMetadataURL =
    command
        "update-url"
        ( info
            ( BakerUpdateMetadataURL
                <$> strArgument (metavar "URL" <> help "Link to information about the validator.")
                <*> transactionOptsParser
            )
            (progDesc "Change link to information about the validator.")
        )

bakerUpdateOpenDelegationStatus :: Mod CommandFields BakerCmd
bakerUpdateOpenDelegationStatus =
    command
        "update-delegation-status"
        ( info
            ( BakerUpdateOpenDelegationStatus
                <$> argument (eitherReader openStatusFromStringInform) (metavar "SELECTION" <> help helpOpenDelegationFor)
                <*> transactionOptsParser
            )
            (progDesc "Change whether to allow other parties to delegate stake to the validator.")
        )

bakerGetEarliestWinTime :: Mod CommandFields BakerCmd
bakerGetEarliestWinTime =
    command
        "win-time"
        ( info
            ( BakerGetEarliestWinTime
                <$> argument auto (metavar "VALIDATOR-ID" <> help "Validator ID to query.")
                <*> switch
                    ( long "local-time"
                        <> help "Display time in the local time zone (instead of UTC)."
                    )
                <*> switch
                    ( long "poll"
                        <> help "Repeatedly poll for the latest time."
                    )
            )
            (progDesc "Show the earliest time the validator may be expected to produce a block.")
        )

bakerSetKeysCmd :: Mod CommandFields BakerCmd
bakerSetKeysCmd =
    command
        "set-key"
        ( info
            ( BakerSetKeys
                <$> strArgument (metavar "FILE" <> help "File containing the new public and private keys.")
                <*> transactionOptsParser
                <*> optional (strOption (long "out" <> metavar "FILE" <> help "File to write the validator credentials to, in case of succesful transaction. These can be used to start the node."))
            )
            ( progDescDoc $
                docFromLines
                    [ "Update the keys of a validator. Expected format of the key file:",
                      "   {",
                      "     \"signatureSignKey\": ...,",
                      "     \"signatureVerifyKey\": ...",
                      "     \"aggregationSignKey\": ...,",
                      "     \"aggregationVerifyKey\": ...",
                      "     \"electionPrivateKey\": ...,",
                      "     \"electionVerifyKey\": ...,",
                      "   }"
                    ]
            )
        )

bakerRemoveCmd :: Mod CommandFields BakerCmd
bakerRemoveCmd =
    command
        "remove"
        ( info
            ( BakerRemove
                <$> transactionOptsParser
            )
            (progDesc "Remove a validator from the chain.")
        )

bakerUpdateRestakeCmd :: Mod CommandFields BakerCmd
bakerUpdateRestakeCmd =
    command
        "update-restake"
        ( info
            ( BakerUpdateRestakeEarnings
                <$> argument auto (metavar "UPDATE" <> help "True|False")
                <*> transactionOptsParser
            )
            (progDesc "Change whether to restake the earnings automatically or not")
        )

bakerUpdateStakeCmd :: Mod CommandFields BakerCmd
bakerUpdateStakeCmd =
    command
        "update-stake"
        ( info
            ( BakerUpdateStake
                <$> option (eitherReader amountFromStringInform) (long "stake" <> metavar "CCD-AMOUNT" <> help "The amount of CCD to stake.")
                <*> transactionOptsParser
            )
            (progDesc "Update the amount staked for the validator.")
        )

allowedValuesDelegationTargetAsString :: String
allowedValuesDelegationTargetAsString =
    "'Passive' (for passive delegation), VALIDATORID (delegate stake to a staking pool with validator id VALIDATORID), ACCOUNT (delegate stake to validator with account ACCOUNT)"

helpDelegationTarget :: Mod OptionFields Text
helpDelegationTarget =
    help $
        "Select whether to delegate stake passively or to a staking pool. Available values for TARGET are: " ++ allowedValuesDelegationTargetAsString ++ "."

delegatorConfigureCmd :: Mod CommandFields DelegatorCmd
delegatorConfigureCmd =
    command
        "configure"
        ( info
            ( DelegatorConfigure
                <$> transactionOptsParser
                <*> optional (option (eitherReader amountFromStringInform) (long "stake" <> metavar "CCD-AMOUNT" <> help "The amount of CCD to stake."))
                <*> optional
                    ( flag' True (long "restake" <> help "The earnings will be added to the delegated stake automatically.")
                        <|> flag' False (long "no-restake" <> help "The earnings will not be added to the delegated stake automatically.")
                    )
                <*> optional (strOption (long "target" <> metavar "TARGET" <> helpDelegationTarget))
            )
            (progDesc "Configure delegator information on the chain.")
        )

delegatorRemoveCmd :: Mod CommandFields DelegatorCmd
delegatorRemoveCmd =
    command
        "remove"
        ( info
            (DelegatorRemove <$> transactionOptsParser)
            (progDesc "Stop delegating.")
        )

delegatorAddCmd :: Mod CommandFields DelegatorCmd
delegatorAddCmd =
    command
        "add"
        ( info
            ( DelegatorAdd
                <$> transactionOptsParser
                <*> option (eitherReader amountFromStringInform) (long "stake" <> metavar "CCD-AMOUNT" <> help "The amount of CCD to stake.")
                <*> (not <$> switch (long "no-restake" <> help "The earnings will not be added to the delegated stake automatically."))
                <*> strOption (long "target" <> metavar "TARGET" <> helpDelegationTarget)
            )
            (progDesc "Add delegator to the chain.")
        )

identityCmds :: Mod CommandFields Cmd
identityCmds =
    command
        "identity"
        ( info
            ( IdentityCmd
                <$> hsubparser
                    identityShowCmd
            )
            (progDesc "Commands for interacting with the ID layer.")
        )

identityShowCmd :: Mod CommandFields IdentityCmd
identityShowCmd =
    command
        "show"
        ( info
            ( IdentityShow
                <$> hsubparser
                    ( identityShowIPsCmd
                        <> identityShowARsCmd
                    )
            )
            (progDesc "Show ID layer values at a given block.")
        )

identityShowIPsCmd :: Mod CommandFields IdentityShowCmd
identityShowIPsCmd =
    command
        "identity-providers"
        ( info
            ( IdentityShowIPs
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Show identity providers at a given block.")
        )

identityShowARsCmd :: Mod CommandFields IdentityShowCmd
identityShowARsCmd =
    command
        "anonymity-revokers"
        ( info
            ( IdentityShowARs
                <$> optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\")."))
            )
            (progDesc "Show anonymity revokers at a given block.")
        )

poolCmds :: Mod CommandFields Cmd
poolCmds =
    command
        "pool"
        ( info
            ( PoolCmd
                <$> hsubparser
                    poolStatusCmd
            )
            (progDesc "Commands for inspecting a validator pool or passive delegation.")
        )

-- argument auto (metavar "VALIDATOR-ID" <> help "Validator ID to query.")
poolStatusCmd :: Mod CommandFields PoolCmd
poolStatusCmd =
    command
        "status"
        ( info
            ( PoolStatus
                <$> optional (argument auto (metavar "POOL" <> help "Validator ID of pool. If not provided, status of passive delegation is queried."))
                <*> optional
                    ( strOption
                        ( long "block"
                            <> metavar "BLOCK-HASH"
                            <> help "Hash of the block to query (default: Query the best block)"
                        )
                    )
            )
            (progDesc "Inspect the status of a validator pool or passive delegation.")
        )

docFromLines :: [String] -> Maybe (P.Doc P.AnsiStyle)
docFromLines = Just . P.vsep . map fromString

-- | A parameter file used for initializing, updating, and invoking smart contracts.
--   For the JSON parameter a schema must be embedded in the module or supplied with the --schema flag.
--   The schema is then used to serialize the JSON to binary.
data ParameterFileInput
    = ParameterJSON FilePath
    | ParameterBinary FilePath
    deriving (Show)

-- | Parse an optional parameter file.
--   Either with '--parameter-json' or '--parameter-binary', but not both.
parameterFileParser :: Parser (Maybe ParameterFileInput)
parameterFileParser =
    optional
        ( ( ParameterJSON
                <$> strOption
                    ( long "parameter-json"
                        <> metavar "FILE"
                        <> help
                            "JSON file with parameters for the receive function. \
                            \This parameter format should be used if the schema is supplied (default: no parameter)."
                    )
          )
            <|> ( ParameterBinary
                    <$> strOption
                        ( long "parameter-binary"
                            <> metavar "FILE"
                            <> help
                                "Binary file with parameters for the receive function. \
                                \This should _not_ be used if a schema is supplied (default: no parameter)."
                        )
                )
        )

-- | An invoker of a smart contract used with 'contract invoke'.
--   The invoker can either be an account or a contract.
--   For the contract, the subindex is optional and defaults to 0.
data InvokerInput
    = InvokerAccount Text
    | InvokerContract
        { icIndexOrName :: Text,
          icSubindex :: Maybe Word64
        }
    deriving (Show)

-- | Parse an optional invoker.
--   Either with '--invoker-account' or '--invoker-contract', but not both.
--   If the invoker is a contract, the subindex can be provided with '--invoker-contract-subindex'.
invokerParser :: Parser (Maybe InvokerInput)
invokerParser =
    optional $
        ( InvokerAccount
            <$> strOption
                ( long "invoker-account"
                    <> metavar "ACCOUNT"
                    <> help "Name or address of an account used as invoker (default: uses address 0 with sufficient funds)."
                )
        )
            <|> ( InvokerContract
                    <$> strOption
                        ( long "invoker-contract"
                            <> metavar "INDEX-OR-NAME"
                            <> help "Index of contract address OR contract name used as invoker."
                        )
                    <*> optional
                        ( option
                            auto
                            ( long "invoker-contract-subindex"
                                <> metavar "SUBINDEX"
                                <> help "Subindex of contract address used as invoker (default: 0)"
                            )
                        )
                )
