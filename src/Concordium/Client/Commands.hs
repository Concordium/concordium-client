module Concordium.Client.Commands
  ( optsParser
  , backendParser
  , Verbose
  , Options(..)
  , Backend(..)
  , Cmd(..)
  , ConfigCmd(..)
  , ConfigAccountCmd(..)
  , ConfigKeyCmd(..)
  , TransactionOpts(..)
  , TransactionCmd(..)
  , AccountCmd(..)
  , ModuleCmd(..)
  , ContractCmd(..)
  , LegacyCmd(..)
  , ConsensusCmd(..)
  , BlockCmd(..)
  , BakerCmd(..)
  ) where

import Data.Text
import Data.Version (showVersion)
import Network.HTTP2.Client
import Options.Applicative
import Paths_simple_client (version)
import Concordium.Client.LegacyCommands
import Concordium.ID.Types (KeyIndex)
import Concordium.Types

type Verbose = Bool

data Options =
  Options
  { optsCmd :: Cmd
  , optsBackend :: Maybe Backend
  , optsConfigDir :: Maybe FilePath
  , optsVerbose :: Verbose }
  deriving (Show)

data Backend =
  GRPC
    { grpcHost   :: !HostName
    , grpcPort   :: !PortNumber
    , grpcTarget :: !(Maybe String)
    , grpcRetryNum :: !(Maybe Int) }
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
  deriving (Show)

data ConfigCmd
  = ConfigInit
  | ConfigShow
  | ConfigAccountCmd -- groups 'config account' commands
    { configAccountCmd :: ConfigAccountCmd }
  | ConfigKeyCmd -- groups 'config key' commands
    { configKeyCmd :: ConfigKeyCmd }
  deriving (Show)

data ConfigAccountCmd
  = ConfigAccountAdd
    { caaAddr :: !Text
    , caaName :: !(Maybe Text)}
  | ConfigAccountImport
    { caiName :: !(Maybe Text),
      caiFile :: FilePath
    }
  deriving (Show)

data ConfigKeyCmd
  = ConfigKeyAdd
    { ckaAddr :: !Text
    , ckaIdx :: !(Maybe KeyIndex)
    , ckaSignKey :: !Text
    , ckaVerifyKey :: !Text }
  deriving (Show)

data TransactionCmd
  = TransactionSubmit
    { tsFile :: !FilePath }
  | TransactionStatus
    { tsHash :: !Text }
  | TransactionSendGtu
    { tsgReceiver :: !Text
    , tsgAmount :: !Amount
    , tsgOpts :: !TransactionOpts }
  | TransactionDeployCredential
    { tdcFile :: !FilePath }
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
  , toKeys :: !(Maybe Text)
  , toNonce :: !(Maybe Nonce)
  , toMaxEnergyAmount :: !(Maybe Energy)
  , toExpiration :: !(Maybe Text) }
  deriving (Show)

data ConsensusCmd
  = ConsensusStatus
  | ConsensusShowParameters
    { cspBlockHash :: !(Maybe Text)
    , cspIncludeBakers :: !Bool }
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
  | BakerUpdateAccount
    { buaBakerId :: !BakerId
    , buaAccountKeysFile :: !FilePath
    , buaTransactionOpts :: !TransactionOpts }
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
backendParser = GRPC <$> hostParser <*> portParser <*> targetParser <*> retryNumParser

hostParser :: Parser HostName
hostParser =
  strOption
    (long "grpc-ip" <> metavar "GRPC-IP" <>
     help "IP address on which the gRPC server is listening.")

portParser :: Parser PortNumber
portParser =
  option
    auto
    (long "grpc-port" <> metavar "GRPC-PORT" <>
     help "Port where the gRPC server is listening.")

targetParser :: Parser (Maybe String)
targetParser =
  optional $
  strOption
    (hidden <>
     long "grpc-target" <>
     metavar "GRPC-TARGET" <>
     help "Target node name when using a proxy.")

retryNumParser :: Parser (Maybe Int)
retryNumParser =
  optional $
    option auto
    (hidden <>
     long "grpc-retry" <>
     metavar "GRPC-RETRY" <>
     help "How many times to retry the connection if it fails the first time.")

transactionOptsParser :: Parser TransactionOpts
transactionOptsParser =
  TransactionOpts <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender.")) <*>
    optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified as JSON ({<key-idx>: {<sign-key>, <verify-key>}).")) <*>
    optional (option auto (long "nonce" <> metavar "NONCE" <> help "Transaction nonce.")) <*>
    optional (option auto (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction. Depeding on the transaction type, this flag may be optional.")) <*>
    optional (strOption (long "expiry" <> metavar "EXPIRY" <> help "Expiration time of a transaction, specified as a relative duration (\"30s\", \"5m\", etc.) or UNIX epoch timestamp."))

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
                      bakerCmds
                     ) <|> LegacyCmd <$> legacyProgramOptions) <*>
                   optional backendParser <*>
                   optional (strOption (long "config" <> metavar "PATH" <> help "Path to the configuration directory.")) <*>
                   switch (hidden <> long "verbose" <> short 'v' <> help "Make output verbose.")

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
        strArgument (metavar "FILE" <> help "File containing the transaction parameters in JSON format."))
      (progDesc "Parse transaction and send it to the baker."))

transactionDeployCredentialCmd :: Mod CommandFields TransactionCmd
transactionDeployCredentialCmd =
  command
    "deploy-credential"
    (info
      (TransactionDeployCredential <$>
        strArgument (metavar "FILE" <> help "File containing the credential deployment information."))
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
      (progDesc "Transfer GTU from one account to another account (sending to contracts is currently not supported with this method - use 'transaction submit')."))

accountCmds :: Mod CommandFields Cmd
accountCmds =
  command
    "account"
    (info
      (AccountCmd <$>
        hsubparser
          (accountShowCmd <>
           accountListCmd <>
           accountDelegateCmd))
      (progDesc "Commands for inspecting accounts."))

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
  command
    "show"
    (info
       (AccountShow <$>
         optional (strArgument (metavar "ADDRESS" <> help "Address of the account.")) <*>
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
           configAccountCmds <>
           configKeyCmds))
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
          (configAccountAddCmd <> configAccountImportCmd))
      (progDesc "Commands for inspecting and changing account-specific configuration."))

configAccountAddCmd :: Mod CommandFields ConfigAccountCmd
configAccountAddCmd =
  command
    "add"
    (info
      (ConfigAccountAdd <$>
        strArgument (metavar "ADDRESS" <> help "Address of the account.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name of the account.")))
      (progDesc "Add account to persistent config."))

configAccountImportCmd :: Mod CommandFields ConfigAccountCmd
configAccountImportCmd =
  command
    "import"
    (info
      (ConfigAccountImport <$>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name of the account.")) <*>
        strArgument (metavar "PATH" <> help "Account file exported from the wallet.")
      )
      (progDesc "Import an account to persistent config."))

configKeyCmds :: Mod CommandFields ConfigCmd
configKeyCmds =
  command
    "key"
    (info
      (ConfigKeyCmd <$>
        hsubparser
          configKeyAddCmd)
      (progDesc "Commands for adding and removing keys of an account."))

configKeyAddCmd :: Mod CommandFields ConfigKeyCmd
configKeyAddCmd =
  command
    "add"
    (info
      (ConfigKeyAdd <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        optional (option auto (long "index" <> metavar "INDEX" <> help "Index of the key.")) <*>
        strOption (long "sign" <> metavar "KEY" <> help "Sign (private) key.") <*>
        strOption (long "verify" <> metavar "KEY" <> help "Verify (public) key."))
      (progDesc "Add key pair of specific account, optionally on a specific index."))
consensusCmds :: Mod CommandFields Cmd
consensusCmds =
  command
    "consensus"
    (info
      (ConsensusCmd <$>
        hsubparser
          (consensusStatusCmd <>
           consensusShowParametersCmd))
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
           bakerUpdateAccountCmd))
      (progDesc "Commands for creating and deploying baker credentials."))

bakerGenerateKeysCmd :: Mod CommandFields BakerCmd
bakerGenerateKeysCmd =
  command
    "generate-keys"
    (info
      (BakerGenerateKeys <$>
        optional (strArgument (metavar "FILE" <> help "Target file of generated credentials.")))
      (progDesc "Create baker credentials and write them to a file or stdout."))

bakerAddCmd :: Mod CommandFields BakerCmd
bakerAddCmd =
  command
    "add"
    (info
      (BakerAdd <$>
        strArgument (metavar "FILE" <> help "File containing the baker credentials.") <*>
        transactionOptsParser)
      (progDesc "Deploy baker credentials to the chain."))

bakerUpdateAccountCmd :: Mod CommandFields BakerCmd
bakerUpdateAccountCmd =
  command
    "update-account"
    (info
      (BakerUpdateAccount <$>
        argument auto (metavar "BAKER-ID" <> help "ID of the baker") <*>
        strArgument (metavar "FILE" <> help "File containing keys of the account to send rewards to") <*>
        transactionOptsParser)
      (progDesc "Update the account that a baker's rewards are sent to")
    )
