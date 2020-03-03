module Concordium.Client.Commands
  ( optsParser
  , backendParser
  , Verbose
  , Options(..)
  , Cmd(..)
  , TransactionCmd(..)
  , AccountCmd(..)
  , ModuleCmd(..)
  , ContractCmd(..)
  , LegacyCmd(..)
  , TransactionCfg(..)
  , Backend(..)
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
  { cmd :: Cmd
  , backend :: Maybe Backend
  , verbose :: Verbose }
  deriving (Show)

data Backend =
  GRPC
    { host   :: HostName
    , port   :: PortNumber
    , target :: Maybe String }
  deriving (Show)

data Cmd
  = LegacyCmd
    { legacyCmd  :: LegacyCmd }
  | TransactionCmd
    { transactionCmd :: TransactionCmd }
  | AccountCmd
    { accountCmd :: AccountCmd }
  | ModuleCmd
    { moduleCmd :: ModuleCmd }
  | ContractCmd
    { contractCmd :: ContractCmd }
  deriving (Show)

data TransactionCmd
  = TransactionSubmit
    { transactionSourceFile :: !FilePath }
  | TransactionStatus
    { transactionHash :: !Text }
  | TransactionSendGtu
    { transactionToAccount :: !Text
    , transactionAmount :: !Amount
    , transactionCfg :: !TransactionCfg }
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
    , moduleTransactionCfg :: !TransactionCfg }
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
    , contractTransactionCfg :: !TransactionCfg }
  deriving (Show)

data TransactionCfg =
  TransactionCfg
  { sender :: !(Maybe Text)
  , keys :: !(Maybe Text)
  , nonce :: !(Maybe Nonce)
  , maxEnergyAmount :: !(Maybe Energy)
  , expiration :: !(Maybe TransactionExpiryTime) }
  deriving (Show)

optsParser :: ParserInfo Options
optsParser = info
               (helper <*> versionOption <*> programOptions)
               (fullDesc <> progDesc "Simple Client" <>
                header "simple-client - a small client to interact with the p2p-client")

versionOption :: Parser (a -> a)
versionOption =
  infoOption (showVersion version) (long "version" <> help "Show version")

backendParser :: Parser Backend
backendParser = GRPC <$> hostParser <*> portParser <*> targetParser

hostParser :: Parser HostName
hostParser =
  strOption
    (long "grpc-ip" <> metavar "GRPC-IP" <>
     help "IP address on which the gRPC server is listening")

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
    (long "grpc-target" <> metavar "GRPC-TARGET" <>
     help "Target node name when using a proxy.")

transactionCfgParser :: Parser TransactionCfg
transactionCfgParser =
  TransactionCfg <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "address of the transaction sender")) <*>
    optional (strOption (long "keys" <> metavar "KEYS" <> help "any number of sign/verify keys specified as JSON ({<key-idx>: {<sign-key>, <verify-key>})")) <*>
    optional (option auto (long "nonce" <> metavar "NONCE" <> help "transaction nonce")) <*>
    optional (option auto (long "energy" <> metavar "MAX-ENERGY" <> help "maximum allowed amount of energy to spend on transaction")) <*>
    optional (option auto (long "expiry" <> metavar "EXPIRY" <> help "expiration time of a transaction, specified as a UNIX epoch timestamp"))

programOptions :: Parser Options
programOptions = Options <$>
                   (hsubparser
                     (transactionCmds <>
                      accountCmds <>
                      moduleCmds <>
                      contractCmds
                     ) <|> (LegacyCmd <$> legacyProgramOptions)) <*>
                   (optional backendParser) <*>
                   (switch (long "verbose" <> help "Make output verbose"))

transactionCmds :: Mod CommandFields Cmd
transactionCmds =
  command
    "transaction"
    (info
      (TransactionCmd <$>
        (hsubparser
          (transactionSubmitCmd <>
           transactionStatusCmd <>
           transactionSendGtuCmd)))
      (progDesc "commands for submitting and inspecting transactions"))

transactionSubmitCmd :: Mod CommandFields TransactionCmd
transactionSubmitCmd =
  command
    "submit"
    (info
      (TransactionSubmit <$>
        strArgument (metavar "TX-SOURCE" <> help "JSON file with the transaction"))
      (progDesc "parse transaction and send it to the baker"))

transactionStatusCmd :: Mod CommandFields TransactionCmd
transactionStatusCmd =
  command
    "status"
    (info
      (TransactionStatus <$>
        strArgument (metavar "TX-HASH" <> help "hash of the transaction"))
      (progDesc "get status of a transaction"))

transactionSendGtuCmd :: Mod CommandFields TransactionCmd
transactionSendGtuCmd =
  command
    "send-gtu"
    (info
      (TransactionSendGtu <$>
        strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "address of the receiver") <*>
        option auto (long "amount" <> metavar "GTU-AMOUNT" <> help "amount of GTUs to send") <*>
        transactionCfgParser)
      (progDesc "transfer GTU from one account to another account (sending to contracts is currently not supported with this method - use 'transaction submit')"))

accountCmds :: Mod CommandFields Cmd
accountCmds =
  command
    "account"
    (info
      (AccountCmd <$>
        (hsubparser
          (accountShowCmd <>
           accountListCmd)))
      (progDesc "commands for inspecting accounts"))

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
  command
    "show"
    (info
       (AccountShow <$>
         strArgument (metavar "ADDRESS" <> help "address of the account") <*>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
       (progDesc "display account details"))

accountListCmd :: Mod CommandFields AccountCmd
accountListCmd =
  command
    "list"
    (info
       (AccountList <$>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
       (progDesc "list all accounts"))

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
      (progDesc "commands for inspecting and deploying modules"))

moduleShowCmd :: Mod CommandFields ModuleCmd
moduleShowCmd =
  command
    "show"
    (info
      (ModuleShow <$>
        strArgument (metavar "REF" <> help "reference ID of the module") <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
      (progDesc "display module source code"))

moduleListCmd :: Mod CommandFields ModuleCmd
moduleListCmd =
  command
    "list"
    (info
      (ModuleList <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
      (progDesc "list all modules at given (default: \"best\") block"))

moduleDeployCmd :: Mod CommandFields ModuleCmd
moduleDeployCmd =
  command
    "deploy"
    (info
      (ModuleDeploy <$>
        strArgument (metavar "MODLE-NAME" <> help "name of the module to deploy") <*>
        transactionCfgParser
      )
      (progDesc "deploy module"))
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
      (progDesc "commands for inspecting and initializing smart contracts"))

contractShowCmd :: Mod CommandFields ContractCmd
contractShowCmd =
  command
    "show"
    (info
      (ContractShow <$>
        strArgument (metavar "ADDRESS" <> help "address of the contract") <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
      (progDesc "display contract state at given (default: \"best\") block"))

contractListCmd :: Mod CommandFields ContractCmd
contractListCmd =
  command
    "list"
    (info
      (ContractList <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "hash of the block")))
    (progDesc "list all contracts on a specific (default: \"best\") block"))

contractInitCmd :: Mod CommandFields ContractCmd
contractInitCmd =
  command
    "init"
    (info
      (ContractInit <$>
        strOption (long "module" <> metavar "MODULE" <> help "module containing the contract") <*>
        strOption (long "name" <> metavar "NAME" <> help "name of the contract in the module") <*>
        option auto (long "amount" <> metavar "AMOUNT" <> help "amount of GTU to transfer to the contract") <*>
        transactionCfgParser)
      (progDesc "initialize contract from already deployed module"))

-- TODO Add "consensus" and "node" commands.
