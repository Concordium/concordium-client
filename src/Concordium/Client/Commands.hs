
module Concordium.Client.Commands
  ( optsParser
  , backendParser
  , Verbose
  , Options(..)
  , Backend(..)
  , Cmd(..)
  , ConfigCmd(..)
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
    , grpcRetryNum :: !(Maybe Int)
    }
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
  = ConfigShow
  deriving (Show)

data TransactionCmd
  = TransactionSubmit
    { transactionSourceFile :: !FilePath }
  | TransactionStatus
    { transactionHash :: !Text }
  | TransactionSendGtu
    { transactionToAccount :: !Text
    , transactionAmount :: !Amount
    , transactionOpts :: !TransactionOpts }
  | TransactionDeployCredential
    {
      transactionCredentialFile :: !FilePath
    }
  deriving (Show)

data AccountCmd
  = AccountShow
    { accountAddress :: !Text
    , accountBlockHash :: !(Maybe Text) }
  | AccountList
    { accountBlockHash :: !(Maybe Text) }
  deriving (Show)

data ModuleCmd
  = ModuleShow
    { ref :: !Text
    , moduleBlockHash :: !(Maybe Text) }
  | ModuleList
    { moduleBlockHash :: !(Maybe Text) }
  | ModuleDeploy
    { moduleName :: !Text
    , moduleTransactionOpts :: !TransactionOpts }
  deriving (Show)

data ContractCmd
  = ContractShow
    { contractAddress :: !Text
    , contractBlockHash :: !(Maybe Text) }
  | ContractList
    { contractBlockHash :: !(Maybe Text) }
  | ContractInit
    { contractModuleName :: !Text
    , contractName :: !Text
    , contractParameter :: !Text
    , contractTransactionOpts :: !TransactionOpts }
  deriving (Show)

data TransactionOpts =
  TransactionOpts
  { toSender :: !(Maybe Text)
  , toKeys :: !(Maybe Text)
  , toNonce :: !(Maybe Nonce)
  , toMaxEnergyAmount :: !(Maybe Energy)
  , toExpiration :: !(Maybe TransactionExpiryTime) }
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

transactionCfgParser :: Parser TransactionOpts
transactionCfgParser =
  TransactionOpts <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender.")) <*>
    optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified as JSON ({<key-idx>: {<sign-key>, <verify-key>}).")) <*>
    optional (option auto (long "nonce" <> metavar "NONCE" <> help "Transaction nonce.")) <*>
    optional (option auto (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction.")) <*>
    optional (option auto (long "expiry" <> metavar "EXPIRY" <> help "Expiration time of a transaction, specified as a UNIX epoch timestamp."))

programOptions :: Parser Options
programOptions = Options <$>
                   ((hsubparser
                     (metavar "command" <>
                      transactionCmds <>
                      accountCmds <>
                      moduleCmds <>
                      contractCmds <>
                      configCmds <>
                      consensusCmds <>
                      blockCmds <>
                      bakerCmds
                     )) <|> (LegacyCmd <$> legacyProgramOptions)) <*>
                   (optional backendParser) <*>
                   (optional (strOption (long "config" <> metavar "DIR" <> help "Configuration directory path."))) <*>
                   (switch (hidden <> long "verbose" <> short 'v' <> help "Make output verbose."))

transactionCmds :: Mod CommandFields Cmd
transactionCmds =
  command
    "transaction"
    (info
      (TransactionCmd <$>
        (hsubparser
          (transactionSubmitCmd <>
           transactionStatusCmd <>
           transactionSendGtuCmd <>
           transactionDeployCredentialCmd
          )))
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
        transactionCfgParser)
      (progDesc "Transfer GTU from one account to another account (sending to contracts is currently not supported with this method - use 'transaction submit')."))

accountCmds :: Mod CommandFields Cmd
accountCmds =
  command
    "account"
    (info
      (AccountCmd <$>
        (hsubparser
          (accountShowCmd <>
           accountListCmd)))
      (progDesc "Commands for inspecting accounts."))

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
  command
    "show"
    (info
       (AccountShow <$>
         strArgument (metavar "ADDRESS" <> help "Address of the account.") <*>
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

moduleCmds :: Mod CommandFields Cmd
moduleCmds =
  command
    "module"
    (info
      (ModuleCmd <$>
        (hsubparser
          (moduleShowCmd <>
           moduleListCmd <>
           moduleDeployCmd)))
      (progDesc "Commands for inspecting and deploying modules."))

moduleShowCmd :: Mod CommandFields ModuleCmd
moduleShowCmd =
  command
    "show"
    (info
      (ModuleShow <$>
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

moduleDeployCmd :: Mod CommandFields ModuleCmd
moduleDeployCmd =
  command
    "deploy"
    (info
      (ModuleDeploy <$>
        strArgument (metavar "MODLE-NAME" <> help "Name of the module to deploy.") <*>
        transactionCfgParser
      )
      (progDesc "Deploy module."))

contractCmds :: Mod CommandFields Cmd
contractCmds =
  command
    "contract"
    (info
      (ContractCmd <$>
        (hsubparser
          (contractShowCmd <>
           contractListCmd <>
           contractInitCmd)))
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
        transactionCfgParser)
      (progDesc "Initialize contract from already deployed module."))

configCmds :: Mod CommandFields Cmd
configCmds =
  command
    "config"
    (info
      (ConfigCmd <$>
        (hsubparser
          configShowCmd))
      (progDesc "Commands for inspecting and changing local configuration."))

configShowCmd :: Mod CommandFields ConfigCmd
configShowCmd =
  command
    "show"
    (info
      (pure ConfigShow)
      (progDesc "Show configuration."))

consensusCmds :: Mod CommandFields Cmd
consensusCmds =
  command
    "consensus"
    (info
      (ConsensusCmd <$>
        (hsubparser
          (consensusStatusCmd <>
           consensusShowParametersCmd)))
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
        (hsubparser
          (blockShowCmd)))
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
        (hsubparser
          (bakerGenerateKeysCmd <>
           bakerAddCmd)))
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
        transactionCfgParser)
      (progDesc "Deploy baker credentials to the chain."))
