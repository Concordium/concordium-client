module Concordium.Client.Commands
  ( optsParser
  , hostParser
  , portParser
  , targetParser
  , Command(..)
  , Action(..)
  , Backend(..)
  ) where

import           Data.Text
import           Data.Version         (showVersion)
import           Network.HTTP2.Client
import           Options.Applicative
import           Paths_simple_client  (version)

data Command =
  Command
    { action  :: Action
    , backend :: Maybe Backend
    }

data Backend =
  GRPC
    { host :: HostName
    , port :: PortNumber
    , target :: Maybe String
    }

-- |Available commands
data Action
  = LoadModule
      { sourceFile :: !FilePath
      } -- ^ Loads a module into the local database
  | ListModules -- ^ List the local available modules
  | SendTransaction
      { sourceFile :: !FilePath
      , networkId  :: !Int
      } -- ^ Loads a transaction in the context of the local database and sends it to the specified RPC server
  | GetConsensusInfo -- ^ Queries the gRPC server for the consensus information
  | GetBlockInfo
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the information of a specific block
  | GetAccountList
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the list of accounts on a specific block
  | GetInstances
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the list of instances on a specific block
  | GetAccountInfo
      { blockHash      :: !Text
      , accountAddress :: !Text
      } -- ^ Queries the gRPC server for the information of an account on a specific block
  | GetInstanceInfo
      { blockHash       :: !Text
      , contractAddress :: !Text
      } -- ^ Queries the gRPC server for the information of an instance on a specific block
  | GetRewardStatus
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the reward status on a specific block
  | GetBirkParameters
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the Birk parameters on a specific block
  | GetModuleList
      { blockHash :: !Text
      } -- ^ Queries the gRPC server for the list of modules on a specific block
  | GetModuleSource
      { blockHash :: !Text
      , moduleRef :: !Text
      } -- ^ Queries the gRPC server for the source of a module on a specific block

-- |Parser for the command line arguments
optsParser :: ParserInfo Command
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "Simple Client" <>
     header "simple-client - a small client to interact with the p2p-client")

versionOption :: Parser (a -> a)
versionOption =
  infoOption (showVersion version) (long "version" <> help "Show version")

programOptions :: Parser Command
programOptions =
  Command <$>
  hsubparser
    (loadModuleCommand <> listModulesCommand <> sendTransactionCommand <>
     getConsensusInfoCommand <>
     getBlockInfoCommand <>
     getAccountListCommand <>
     getInstancesCommand <>
     getAccountInfoCommand <>
     getInstanceInfoCommand <>
     getRewardStatusCommand <>
     getBirkParametersCommand <>
     getModuleListCommand <>
     getModuleSourceCommand) <*>
  grpcBackend

grpcBackend :: Parser (Maybe Backend)
grpcBackend = optional $ GRPC <$> hostParser <*> portParser <*> targetParser

loadModuleCommand :: Mod CommandFields Action
loadModuleCommand =
  command
    "LoadModule"
    (info
       (LoadModule <$>
        strArgument
          (metavar "MODULE-SOURCE" <> help "File with the Acorn source code"))
       (progDesc "Parse module and add it to the local database."))

listModulesCommand :: Mod CommandFields Action
listModulesCommand =
  command
    "ListModules"
    (info
       (pure ListModules)
       (progDesc "List local modules and deployed modules."))

sendTransactionCommand :: Mod CommandFields Action
sendTransactionCommand =
  command
    "SendTransaction"
    (info
       (SendTransaction <$>
        strArgument
          (metavar "TX-SOURCE" <> help "JSON file with the transaction") <*>
        argument
          auto
          (metavar "NET-ID" <>
           help "Network ID for the transaction to be sent through" <>
           value 100 <>
           showDefault))
       (progDesc
          "Parse transaction in current context and send it to the baker."))

hostParser :: Parser HostName
hostParser =
  strOption
    (long "grpc-ip" <> metavar "GRPC-IP" <>
     help "IP where the gRPC server is listening.")

portParser :: Parser PortNumber
portParser =
  option
    auto
    (long "grpc-port" <> metavar "GRPC-PORT" <>
     help "Port where the gRPC server is listening.")

targetParser :: Parser (Maybe String)
targetParser =
  optional $ strOption
    (long "grpc-target" <> metavar "GRPC-TARGET" <>
     help "Target node name when using a proxy.")

getConsensusInfoCommand :: Mod CommandFields Action
getConsensusInfoCommand =
  command
    "GetConsensusInfo"
    (info
       (pure GetConsensusInfo)
       (progDesc "Query the gRPC server for the consensus information."))

getBlockInfoCommand :: Mod CommandFields Action
getBlockInfoCommand =
  command
    "GetBlockInfo"
    (info
       (GetBlockInfo <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for a specific block."))

getAccountListCommand :: Mod CommandFields Action
getAccountListCommand =
  command
    "GetAccountList"
    (info
       (GetAccountList <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for the list of accounts."))

getInstancesCommand :: Mod CommandFields Action
getInstancesCommand =
  command
    "GetInstances"
    (info
       (GetInstances <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for the list of instances."))

getAccountInfoCommand :: Mod CommandFields Action
getAccountInfoCommand =
  command
    "GetAccountInfo"
    (info
       (GetAccountInfo <$>
        strArgument
          (metavar "BLOCK-HASH" <>
           help "Hash of the block in which to do the query") <*>
        strArgument (metavar "ACCOUNT" <> help "Account to be queried about"))
       (progDesc "Query the gRPC server for the information of an account."))

getInstanceInfoCommand :: Mod CommandFields Action
getInstanceInfoCommand =
  command
    "GetInstanceInfo"
    (info
       (GetInstanceInfo <$>
        strArgument
          (metavar "BLOCK-HASH" <>
           help "Hash of the block in which to do the query") <*>
        strArgument
          (metavar "INSTANCE" <> help "Contract address to be queried about"))
       (progDesc "Query the gRPC server for the information of an instance."))

getRewardStatusCommand :: Mod CommandFields Action
getRewardStatusCommand =
  command
    "GetRewardStatus"
    (info
       (GetRewardStatus <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for the reward status."))

getBirkParametersCommand :: Mod CommandFields Action
getBirkParametersCommand =
  command
    "GetBirkParameters"
    (info
       (GetBirkParameters <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for the Birk parameters."))

getModuleListCommand :: Mod CommandFields Action
getModuleListCommand =
  command
    "GetModuleList"
    (info
       (GetModuleList <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query"))
       (progDesc "Query the gRPC server for the list of modules."))

getModuleSourceCommand :: Mod CommandFields Action
getModuleSourceCommand =
  command
    "GetModuleSource"
    (info
       (GetModuleSource <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query") <*>
        strArgument (metavar "MODULE-REF" <> help "Reference of the module"))
       (progDesc "Query the gRPC server for the source of a module."))
