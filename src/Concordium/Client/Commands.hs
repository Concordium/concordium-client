module Concordium.Client.Commands
  ( optsParser
  , backendParser
  , Verbose
  , Options(..)
  , Backend(..)
  , Cmd(..)
  , ConfigCmd(..)
  , AccountExportFormat(..)
  , ConfigAccountCmd(..)
  , TransactionOpts(..)
  , InteractionOpts(..)
  , TransactionCmd(..)
  , AccountCmd(..)
  , ModuleCmd(..)
  , ContractCmd(..)
  , LegacyCmd(..)
  , ConsensusCmd(..)
  , BlockCmd(..)
  , BakerCmd(..)
  , IdentityCmd(..)
  , IdentityShowCmd(..)
  ) where

import Data.Text hiding (map)
import Data.Version (showVersion)
import Network.HTTP2.Client
import Options.Applicative
import Paths_simple_client (version)
import Concordium.Client.LegacyCommands
import Concordium.Client.Types.Account
import Concordium.Types
import Text.Printf
import qualified Text.PrettyPrint.ANSI.Leijen as P

type Verbose = Bool

data Options =
  Options
  { optsCmd :: Cmd
  , optsBackend :: Backend
  , optsConfigDir :: Maybe FilePath
  , optsVerbose :: Verbose }
  deriving (Show)

data Backend =
  GRPC
    { grpcHost   :: !HostName
    , grpcPort   :: !PortNumber
    , grpcAuthenticationToken :: !String
    , grpcTarget :: !(Maybe String)
    , grpcRetryNum :: !Int }
  deriving (Show)

data Cmd
  = LegacyCmd
    { legacyCmd  :: LegacyCmd }
  | ConfigCmd
    { configCmd :: ConfigCmd }
  | TransactionCmd
    { transactionCmd :: TransactionCmd }
  | AccountCmd
    { accountCmd :: AccountCmd }
  | ModuleCmd
    { moduleCmd :: ModuleCmd }
  | ContractCmd
    { contractCmd :: ContractCmd }
  | ConsensusCmd
    { consensusCmd :: ConsensusCmd }
  | BlockCmd
    { blockCmd :: BlockCmd }
  | BakerCmd
    { bakerCmd :: BakerCmd }
  | IdentityCmd
    { identityCmd :: IdentityCmd }
  | RawCmd
    { rawCmd :: LegacyCmd }
  deriving (Show)

data ConfigCmd
  = ConfigInit
  | ConfigShow
  | ConfigAccountCmd -- groups 'config account' commands
    { configAccountCmd :: ConfigAccountCmd }
  deriving (Show)

data ConfigAccountCmd
  = ConfigAccountAdd
    { caaAddr :: !Text
    , caaName :: !(Maybe Text) }
  | ConfigAccountImport
    { caiFile :: !FilePath
    , caiName :: !(Maybe Text)
    , caiFormat :: !(Maybe AccountExportFormat) }
  | ConfigAccountAddKeys
    { caakAddr :: !Text
    , caakKeysFile :: !FilePath }
  deriving (Show)

data TransactionCmd
  = TransactionSubmit
    { tsFile :: !FilePath
    , tsInteractionOpts :: !InteractionOpts }
  | TransactionStatus
    { tsHash :: !Text }
  | TransactionSendGtu
    { tsgReceiver :: !Text
    , tsgAmount :: !Amount
    , tsgOpts :: !TransactionOpts }
  | TransactionDeployCredential
    { tdcFile :: !FilePath
    , tdcInteractionOpts :: !InteractionOpts }
  deriving (Show)

data AccountCmd
  = AccountShow
    { asAddress :: !(Maybe Text)
    , asBlockHash :: !(Maybe Text) }
  | AccountList
    { alBlockHash :: !(Maybe Text) }
  | AccountDelegate
    { adBakerId :: !BakerId
    , adTransactionOpts :: !TransactionOpts }
  | AccountUndelegate
    { auTransactionOpts :: !TransactionOpts }
  deriving (Show)

data ModuleCmd
  = ModuleShowSource
    { mssRef :: !Text
    , mssBlockHash :: !(Maybe Text) }
  | ModuleList
    { mlBlockHash :: !(Maybe Text) }
  deriving (Show)

data ContractCmd
  = ContractShow
    { csAddress :: !Text
    , csBlockHash :: !(Maybe Text) }
  | ContractList
    { clBlockHash :: !(Maybe Text) }
  | ContractInit
    { ciModuleName :: !Text
    , ciName :: !Text
    , ciParameter :: !Text
    , ciTransactionOpts :: !TransactionOpts }
  deriving (Show)

data TransactionOpts =
  TransactionOpts
  { toSender :: !(Maybe Text)
  , toKeys :: !(Maybe FilePath)
  , toNonce :: !(Maybe Nonce)
  , toMaxEnergyAmount :: !(Maybe Energy)
  , toExpiration :: !(Maybe Text)
  , toInteractionOpts :: !InteractionOpts }
  deriving (Show)

data InteractionOpts =
  InteractionOpts
  { ioConfirm :: !Bool
  , ioTail :: !Bool }
  deriving (Show)

data ConsensusCmd
  = ConsensusStatus
  | ConsensusShowParameters
    { cspBlockHash :: !(Maybe Text)
    , cspIncludeBakers :: !Bool }
  | ConsensusSetElectionDifficulty
    { cseDifficulty :: !ElectionDifficulty
    , cseTransactionOpts :: !TransactionOpts }
  deriving (Show)

data BlockCmd
  = BlockShow
    { bsBlockHash :: !(Maybe Text) }
  deriving (Show)

data BakerCmd
  = BakerGenerateKeys
    { bgkFile :: !(Maybe FilePath) }
  | BakerAdd
    { baFile :: !FilePath
    , baTransactionOpts :: !TransactionOpts }
  | BakerSetAccount
    { bsaBakerId :: !BakerId
    , bsaAccountRef :: !Text
    , bsaTransactionOpts :: !TransactionOpts }
  | BakerSetKey
    { buskBakerId :: !BakerId
    , bsaSignatureKeysFile :: !FilePath
    , buskTransactionOpts :: !TransactionOpts }
  | BakerRemove
    { brBakerId :: !BakerId
    , brTransactionOpts :: !TransactionOpts }
  | BakerSetAggregationKey
    { bsakBakerId :: !BakerId
    , bsakBakerAggregationKeyFile :: !FilePath
    , bsakTransactionOpts :: !TransactionOpts }
  | BakerSetElectionKey
    { bsekBakerId :: !BakerId
    , bsekBakerElectionKeyFile :: !FilePath
    , bsakTransactionOps :: !TransactionOpts }
  deriving (Show)

data IdentityCmd
  = IdentityShow
    { identityShow :: IdentityShowCmd } -- groups `identity show` commands
  deriving (Show)

data IdentityShowCmd
  = IdentityShowIPs
    { isipsBlockHash :: !(Maybe Text) }
  | IdentityShowARs
    { isarsBlockHash :: !(Maybe Text) }
  deriving (Show)

visibleHelper :: Parser (a -> a)
visibleHelper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short 'h'
  , help "Show detailed help text." ]

optsParser :: ParserInfo Options
optsParser = info
               (programOptions <**> versionOption <**> visibleHelper)
               (fullDesc <> progDesc "Concordium Client CLI." <>
                header "concordium-client - a client to interact with the concordium network.")

versionOption :: Parser (a -> a)
versionOption =
  infoOption (showVersion version) (hidden <> long "version" <> help "Show version.")

backendParser :: Parser Backend
backendParser = GRPC <$> hostParser <*> portParser <*> grpcAuthenticationTokenParser <*> targetParser <*> retryNumParser

hostParser :: Parser HostName
hostParser =
  strOption
    (long "grpc-ip" <>
     metavar "GRPC-IP" <>
     value "localhost" <> -- default value
     showDefault <>
     help "IP address on which the gRPC server is listening.")

portParser :: Parser PortNumber
portParser =
  option
    auto
    (long "grpc-port" <>
     metavar "GRPC-PORT" <>
     value 10000 <> -- default value to match the node default GRPC port
     showDefault <>
     help "Port where the gRPC server is listening.")

grpcAuthenticationTokenParser :: Parser String
grpcAuthenticationTokenParser =
  strOption
    (long "grpc-authentication-token" <>
     metavar "GRPC-AUTHENTICATION-TOKEN" <>
     value "rpcadmin" <> -- default value matches the default value for the node
     showDefault <>
     help "gRPC authentication token used to talk to the node")

targetParser :: Parser (Maybe String)
targetParser =
  optional $
  strOption
    (hidden <>
     long "grpc-target" <>
     metavar "GRPC-TARGET" <>
     help "Target node name when using a proxy.")

retryNumParser :: Parser Int
retryNumParser =
    option auto
    (hidden <>
     long "grpc-retry" <>
     value 0 <> -- default is to only try once
     showDefault <>
     metavar "GRPC-RETRY" <>
     help "How many times to retry the connection if it fails the first time.")

transactionOptsParser :: Parser TransactionOpts
transactionOptsParser =
  TransactionOpts <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender.")) <*>
    -- TODO Specify / refer to format of JSON file when new commands (e.g. account add-keys) that accept same format are
    -- added.
    optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file.")) <*>
    optional (option auto (long "nonce" <> metavar "NONCE" <> help "Transaction nonce.")) <*>
    optional (option auto (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction. Depeding on the transaction type, this flag may be optional.")) <*>
    optional (strOption (long "expiry" <> metavar "EXPIRY" <> help "Expiration time of a transaction, specified as a relative duration (\"30s\", \"5m\", etc.) or UNIX epoch timestamp.")) <*>
    interactionOptsParser

interactionOptsParser :: Parser InteractionOpts
interactionOptsParser =
  InteractionOpts <$>
    (not <$> switch (long "no-confirm" <> help "Do not ask for confirmation before proceeding to send the transaction.")) <*>
    (not <$> switch (long "no-wait" <> help "Exit right after sending the transaction without waiting for it to be committed and finalized."))

programOptions :: Parser Options
programOptions = Options <$>
                   (hsubparser
                     (metavar "command" <>
                      transactionCmds <>
                      accountCmds <>
                      moduleCmds <>
                      contractCmds <>
                      configCmds <>
                      consensusCmds <>
                      blockCmds <>
                      bakerCmds <>
                      identityCmds <>
                      rawCmds
                     )) <*>
                   backendParser <*>
                   optional (strOption (long "config" <> metavar "DIR" <> help "Path to the configuration directory.")) <*>
                   switch (hidden <> long "verbose" <> short 'v' <> help "Make output verbose.")

rawCmds :: Mod CommandFields Cmd
rawCmds =
  command
    "raw"
    (info
      (LegacyCmd <$> legacyProgramOptions)
      (progDesc "Commands that directly expose the node's GRPC interface."))

transactionCmds :: Mod CommandFields Cmd
transactionCmds =
  command
    "transaction"
    (info
      (TransactionCmd <$>
        hsubparser
          (transactionSubmitCmd <>
           transactionStatusCmd <>
           transactionSendGtuCmd <>
           transactionDeployCredentialCmd))
      (progDesc "Commands for submitting and inspecting transactions."))

transactionSubmitCmd :: Mod CommandFields TransactionCmd
transactionSubmitCmd =
  command
    "submit"
    (info
      (TransactionSubmit <$>
        strArgument (metavar "FILE" <> help "File containing the transaction parameters in JSON format.") <*>
        interactionOptsParser)
      (progDesc "Parse transaction and send it to the baker."))

transactionDeployCredentialCmd :: Mod CommandFields TransactionCmd
transactionDeployCredentialCmd =
  command
    "deploy-credential"
    (info
      (TransactionDeployCredential <$>
        strArgument (metavar "FILE" <> help "File containing the credential deployment information.") <*>
        interactionOptsParser)
      (progDesc "Parse credential and send it to the baker."))

transactionStatusCmd :: Mod CommandFields TransactionCmd
transactionStatusCmd =
  command
    "status"
    (info
      (TransactionStatus <$>
        strArgument (metavar "TX-HASH" <> help "Hash of the transaction."))
      (progDesc "Get status of a transaction."))

transactionSendGtuCmd :: Mod CommandFields TransactionCmd
transactionSendGtuCmd =
  command
    "send-gtu"
    (info
      (TransactionSendGtu <$>
        strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.") <*>
        option auto (long "amount" <> metavar "GTU-AMOUNT" <> help "Amount of GTUs to send.") <*>
        transactionOptsParser)
      (progDesc "Transfer GTU from one account to another (sending to contracts is currently not supported with this method - use 'transaction submit')."))

accountCmds :: Mod CommandFields Cmd
accountCmds =
  command
    "account"
    (info
      (AccountCmd <$>
        hsubparser
          (accountShowCmd <>
           accountListCmd <>
           accountDelegateCmd <>
           accountUndelegateCmd))
      (progDesc "Commands for inspecting accounts."))

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
  command
    "show"
    (info
       (AccountShow <$>
         optional (strArgument (metavar "ACCOUNT" <> help "Name or address of the account.")) <*>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
       (progDesc "Display account details."))

accountListCmd :: Mod CommandFields AccountCmd
accountListCmd =
  command
    "list"
    (info
       (AccountList <$>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
       (progDesc "List all accounts."))

accountDelegateCmd :: Mod CommandFields AccountCmd
accountDelegateCmd =
  command
    "delegate"
    (info
      (AccountDelegate <$>
        option auto (long "baker" <> metavar "BAKER-ID" <> help "Baker to which the stake of the sender's account should be delegated.") <*>
        transactionOptsParser)
      (progDesc "Delegate stake of account to baker."))

accountUndelegateCmd :: Mod CommandFields AccountCmd
accountUndelegateCmd =
  command
    "undelegate"
    (info
      (AccountUndelegate <$>
        transactionOptsParser)
      (progDesc "Delegate stake of account to baker."))

moduleCmds :: Mod CommandFields Cmd
moduleCmds =
  command
    "module"
    (info
      (ModuleCmd <$>
        hsubparser
          (moduleShowSourceCmd <>
           moduleListCmd))
      (progDesc "Commands for inspecting and deploying modules."))

moduleShowSourceCmd :: Mod CommandFields ModuleCmd
moduleShowSourceCmd =
  command
    "show-source"
    (info
      (ModuleShowSource <$>
        strArgument (metavar "REF" <> help "Reference ID of the module.") <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Display module source code."))

moduleListCmd :: Mod CommandFields ModuleCmd
moduleListCmd =
  command
    "list"
    (info
      (ModuleList <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "List all modules."))

contractCmds :: Mod CommandFields Cmd
contractCmds =
  command
    "contract"
    (info
      (ContractCmd <$>
        hsubparser
          (contractShowCmd <>
           contractListCmd <>
           contractInitCmd))
      (progDesc "Commands for inspecting and initializing smart contracts."))

contractShowCmd :: Mod CommandFields ContractCmd
contractShowCmd =
  command
    "show"
    (info
      (ContractShow <$>
        strArgument (metavar "ADDRESS" <> help "Address of the contract.") <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Display contract state at given block."))

contractListCmd :: Mod CommandFields ContractCmd
contractListCmd =
  command
    "list"
    (info
      (ContractList <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
    (progDesc "List all contracts on a specific block."))

contractInitCmd :: Mod CommandFields ContractCmd
contractInitCmd =
  command
    "init"
    (info
      (ContractInit <$>
        strOption (long "module" <> metavar "MODULE" <> help "Module containing the contract.") <*>
        strOption (long "name" <> metavar "NAME" <> help "Name of the contract in the module.") <*>
        option auto (long "amount" <> metavar "AMOUNT" <> help "Amount of GTU to transfer to the contract.") <*>
        transactionOptsParser)
      (progDesc "Initialize contract from already deployed module."))

configCmds :: Mod CommandFields Cmd
configCmds =
  command
    "config"
    (info
      (ConfigCmd <$>
        hsubparser
          (configInitCmd <>
           configShowCmd <>
           configAccountCmds))
      (progDesc "Commands for inspecting and changing local configuration."))

configInitCmd :: Mod CommandFields ConfigCmd
configInitCmd =
  command
    "init"
    (info
      (pure ConfigInit)
      (progDesc "Initialize configuration."))

configShowCmd :: Mod CommandFields ConfigCmd
configShowCmd =
  command
    "show"
    (info
      (pure ConfigShow)
      (progDesc "Show configuration."))

configAccountCmds :: Mod CommandFields ConfigCmd
configAccountCmds =
  command
    "account"
    (info
      (ConfigAccountCmd <$>
        hsubparser
          (configAccountAddCmd <>
           configAccountImportCmd <>
           configAccountAddKeysCmd))
      (progDesc "Commands for inspecting and changing account-specific configuration."))

configAccountAddCmd :: Mod CommandFields ConfigAccountCmd
configAccountAddCmd =
  command
    "add"
    (info
      (ConfigAccountAdd <$>
        strArgument (metavar "ADDRESS" <> help "Address of the account.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name of the account.")))
      (progDesc "Add account address to persistent config, optionally naming the account."))

configAccountImportCmd :: Mod CommandFields ConfigAccountCmd
configAccountImportCmd =
  command
    "import"
    (info
      (ConfigAccountImport <$>
        strArgument (metavar "FILE" <> help "Account file exported from the wallet.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name of the account. For the 'web' format, this sets the name to assign to the account. For the 'mobile' format (which contains multiple already named accounts), it selects which account to import.")) <*>
        optional (option readAccountExportFormat (long "format" <> metavar "FORMAT" <> help "Export format. Supported values are 'web', 'mobile'. If omitted, the format is inferred from the filename ('mobile' if the extension is \".concordiumwallet\", 'web' otherwise)."))
      )
      (progDesc "Import an account to persistent config."))

configAccountAddKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountAddKeysCmd =
  command
    "add-keys"
    (info
      (ConfigAccountAddKeys <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file."))
      (progDescDoc $ docFromLines
       [ "Add key pairs specified in a JSON file to a specific account configuration. This does not register the keys on the chain. Expected format:"
       , "   {"
       , "     \"idx1\": {"
       , "       ..."
       , "       \"encryptedSignKey\": {"
       , "         \"metadata\": {"
       , "           \"encryptionMethod\": \"AES-256\","
       , "           \"iterations\": ...,"
       , "           \"salt\": \"...\","
       , "           \"initializationVector\": \"...\","
       , "           \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\""
       , "         },"
       , "         \"cipherText\": \"...\""
       , "       },"
       , "       \"verifyKey\": \"...\","
       , "       \"schemeId\": \"Ed25519\""
       , "     },"
       , "     \"idx2\": {"
       , "       ..."
       , "       \"encryptedSignKey\": {"
       , "         \"metadata\": {"
       , "           \"encryptionMethod\": \"AES-256\","
       , "           \"iterations\": ...,"
       , "           \"salt\": \"...\","
       , "           \"initializationVector\": \"...\","
       , "           \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\""
       , "         },"
       , "         \"cipherText\": \"...\""
       , "       },"
       , "       \"verifyKey\": \"...\","
       , "       \"schemeId\": \"Ed25519\""
       , "     }"
       , "     ..."
       , "   }"
       , "where idx1, idx2 etc. are the indices associated with the respective keys."
       ]
      ))

readAccountExportFormat :: ReadM AccountExportFormat
readAccountExportFormat = str >>= \case
  "web" -> return FormatWeb
  "mobile" -> return FormatMobile
  s -> readerError $ printf "invalid format: %s (supported values: 'web', 'mobile')" (s :: String)

consensusCmds :: Mod CommandFields Cmd
consensusCmds =
  command
    "consensus"
    (info
      (ConsensusCmd <$>
        hsubparser
          (consensusStatusCmd <>
           consensusShowParametersCmd <>
           consensusSetElectionDifficultyCmd))
      (progDesc "Commands for inspecting chain health (branching, finalization), block content/history (including listing transactions), election (Birk) and reward/minting parameters."))

consensusStatusCmd :: Mod CommandFields ConsensusCmd
consensusStatusCmd =
  command
    "status"
    (info
      (pure ConsensusStatus)
      (progDesc "List various parameters related to the current state of the consensus protocol."))

consensusShowParametersCmd :: Mod CommandFields ConsensusCmd
consensusShowParametersCmd =
  command
    "show-parameters"
    (info
      (ConsensusShowParameters <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")) <*>
        switch (long "include-bakers" <> help "Include the \"lottery power\" of individual bakers."))
      (progDesc "Show election parameters for given block."))

consensusSetElectionDifficultyCmd :: Mod CommandFields ConsensusCmd
consensusSetElectionDifficultyCmd =
  command
    "set-election-difficulty"
    (info
      (ConsensusSetElectionDifficulty <$>
        argument auto (metavar "DIFFICULTY" <> help "Difficulty as a decimal number between 0 (inclusive) and 1 (exclusive).") <*>
        transactionOptsParser)
      (progDesc "Set the election difficulty parameter."))

blockCmds :: Mod CommandFields Cmd
blockCmds =
  command
    "block"
    (info
      (BlockCmd <$>
        hsubparser
          blockShowCmd)
      (progDesc "Commands for inspecting individual blocks."))

blockShowCmd :: Mod CommandFields BlockCmd
blockShowCmd =
  command
    "show"
    (info
      (BlockShow <$>
        optional (strArgument (metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Show the backend's information about a specific block. Note that some fields (e.g. slot time) are objective while others (e.g. arrival time) are specific to the particular baker being queried."))

bakerCmds :: Mod CommandFields Cmd
bakerCmds =
  command
    "baker"
    (info
      (BakerCmd <$>
        hsubparser
          (bakerGenerateKeysCmd <>
           bakerAddCmd <>
           bakerRemoveCmd <>
           bakerSetAccountCmd <>
           bakerSetKeyCmd <>
           bakerSetAggregationKeyCmd <>
           bakerSetElectionKeyCmd))
      (progDesc "Commands for creating and deploying baker credentials."))

bakerGenerateKeysCmd :: Mod CommandFields BakerCmd
bakerGenerateKeysCmd =
  command
    "generate-keys"
    (info
      (BakerGenerateKeys <$>
        optional (strArgument (metavar "FILE" <> help "Target file of generated credentials.")))
      (progDescDoc $ docFromLines
        [ "Create baker credentials and write them to a file or stdout. Format:"
        , "    {"
        , "      \"signatureSignKey\": ...,"
        , "      \"signatureVerifyKey\": ...,"
        , "      \"aggregationSignKey\": ...,"
        , "      \"aggregationVerifyKey\": ...,"
        , "      \"electionPrivateKey\": ...,"
        , "      \"electionVerifyKey\": ..."
        , "    }" ]))

bakerAddCmd :: Mod CommandFields BakerCmd
bakerAddCmd =
  command
    "add"
    (info
      (BakerAdd <$>
        strArgument (metavar "FILE" <> help "File containing the baker credentials.") <*>
        transactionOptsParser)
      (progDesc "Deploy baker credentials to the chain."))

bakerSetAccountCmd :: Mod CommandFields BakerCmd
bakerSetAccountCmd =
  command
    "set-account"
    (info
      (BakerSetAccount <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker.") <*>
        strArgument (metavar "ACCOUNT" <> help "Name or address of the account to send rewards to.") <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Update the account that a baker's rewards are sent to." ]))

bakerSetKeyCmd :: Mod CommandFields BakerCmd
bakerSetKeyCmd =
  command
    "set-key"
    (info
      (BakerSetKey <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker.") <*>
        strArgument (metavar "FILE" <> help "File containing the signature keys.") <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Update the signature keys of a baker. Expected format:"
        , "   {"
        , "     \"signatureSignKey\": ...,"
        , "     \"signatureVerifyKey\": ..."
        , "   }" ]))

bakerRemoveCmd :: Mod CommandFields BakerCmd
bakerRemoveCmd =
  command
    "remove"
    (info
      (BakerRemove <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker.") <*>
        transactionOptsParser)
      (progDesc "Remove a baker from the chain."))

bakerSetAggregationKeyCmd :: Mod CommandFields BakerCmd
bakerSetAggregationKeyCmd =
  command
    "set-aggregation-key"
    (info
      (BakerSetAggregationKey <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker.") <*>
        strArgument (metavar "FILE" <> help "File containing the aggregation key.") <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Update the aggregation key of a baker. Expected format:"
        , "   {"
        , "     \"aggregationSignKey\": ...,"
        , "     \"aggregationVerifyKey\": ..."
        , "   }" ]))

bakerSetElectionKeyCmd :: Mod CommandFields BakerCmd
bakerSetElectionKeyCmd =
  command
    "set-election-key"
    (info
      (BakerSetElectionKey <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker.") <*>
        strArgument (metavar "FILE" <> help "File containing the election key.") <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Update the election key of a baker. Expected format:"
        , "   {"
        , "     ..."
        , "     \"electionPrivateKey\": ...,"
        , "     \"electionVerifyKey\": ...,"
        , "     ..."
        , "   }" ]))

identityCmds :: Mod CommandFields Cmd
identityCmds =
  command
    "identity"
    (info
      (IdentityCmd <$>
        hsubparser
          (identityShowCmd))
      (progDesc "Commands for interacting with the ID layer."))


identityShowCmd :: Mod CommandFields IdentityCmd
identityShowCmd=
  command
    "show"
    (info
      (IdentityShow <$>
        hsubparser
         (identityShowIPsCmd <>
          identityShowARsCmd))
      (progDesc "Show ID layer values at a given block."))

identityShowIPsCmd :: Mod CommandFields IdentityShowCmd
identityShowIPsCmd =
  command
    "identity-providers"
    (info
      (IdentityShowIPs <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
    (progDesc "Show identity providers at a given block."))

identityShowARsCmd :: Mod CommandFields IdentityShowCmd
identityShowARsCmd =
  command
    "anonymity-revokers"
    (info
      (IdentityShowARs <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Show anonymity revokers at a given block."))

docFromLines :: [String] -> Maybe P.Doc
docFromLines = Just . P.vsep . map P.text
