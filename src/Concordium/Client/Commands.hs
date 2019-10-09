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
    { host   :: HostName
    , port   :: PortNumber
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
      , hookIt     :: !Bool
      } -- ^ Loads a transaction in the context of the local database and sends it to the specified RPC server
  | HookTransaction
      { transactionHash :: !Text
      } -- ^ Queries the gRPC for the information about the execution of a transaction
  | NodeInfo -- ^ Queries the gRPC server for node information
  | GetConsensusInfo -- ^ Queries the gRPC server for the consensus information
  | GetBlockInfo
      { blockHash :: !Text,
        every :: !Bool
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
  | GetNodeInfo -- ^Queries the gRPC server for the node information.
  | GetBakerPrivateData -- ^Queries the gRPC server for the private data of the baker.
  | GetPeerData
      { includeBootstrapper :: !Bool -- ^Whether to include bootstrapper node in the stats or not.
      } -- ^Get all data as pertaining to the node's role as a member of the P2P network.
  | StartBaker
  | StopBaker
  | PeerConnect
      { ip     :: !Text
      , portPC :: !Int
      }
  | MakeBakerPayload {
        -- |JSON file with baker keys (leadership election and signing).
        bakerKeysFile :: !FilePath,
        -- |JSON file with with account keys.
        accountKeysFile :: !FilePath,
        -- |File to output the payload, if desired.
        payloadFile :: !(Maybe FilePath)
      }
  | SendTransactionPayload {
      -- |JSON file with minimum header data
      headerFile :: !FilePath,
      -- |JSON file with serialized payload.
      payloadInputFile :: !FilePath,
      networkId :: !Int
      }
  | GetPeerUptime
  | SendMessage
      { nodeId    :: !Text
      , netId     :: !Int
      , message   :: !Text
      , broadcast :: !Bool
      }
  | SubscriptionStart
  | SubscriptionStop
  | SubscriptionPoll
  | BanNode
      { nodeId   :: !Text
      , nodePort :: !Int
      , nodeIp   :: !Text
      }
  | UnbanNode
      { nodeId   :: !Text
      , nodePort :: !Int
      , nodeIp   :: !Text
      }
  | JoinNetwork
      { netId :: !Int
      }
  | LeaveNetwork
      { netId :: !Int
      }
  | GetAncestors
      { blockHash :: !Text
      , amount    :: !Int
      }
  | GetBranches
  | GetBannedPeers
  | Shutdown
  | TpsTest
      { networkId :: !Int
      , nodeId    :: !Text
      , directory :: !Text
      }
  | DumpStart
  | DumpStop
  | RetransmitRequest
      { identifier  :: !Text
      , elementType :: !Int
      , since       :: !Int
      , networkId   :: !Int
      }
  | GetSkovStats

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
     hookTransactionCommand <>
     getConsensusInfoCommand <>
     getBlockInfoCommand <>
     getAccountListCommand <>
     getInstancesCommand <>
     getAccountInfoCommand <>
     getInstanceInfoCommand <>
     getRewardStatusCommand <>
     getBirkParametersCommand <>
     getModuleListCommand <>
     getModuleSourceCommand <>
     getNodeInfoCommand <>
     getBakerPrivateDataCommand <>
     getPeerDataCommand <>
     startBakerCommand <>
     stopBakerCommand <>
     peerConnectCommand <>
     getPeerUptimeCommand <>
     sendMessageCommand <>
     subscriptionStartCommand <>
     subscriptionStopCommand <>
     subscriptionPollCommand <>
     banNodeCommand <>
     unbanNodeCommand <>
     joinNetworkCommand <>
     leaveNetworkCommand <>
     getAncestorsCommand <>
     getBranchesCommand <>
     getBannedPeersCommand <>
     shutdownCommand <>
     tpsTestCommand <>
     dumpStartCommand <>
     dumpStopCommand <>
     retransmitRequestCommand <>
     getSkovStatsCommand <>
     getMakeBakerPayloadCommand <>
     sendTransactionPayloadCommand
    ) <*>
  grpcBackend

grpcBackend :: Parser (Maybe Backend)
grpcBackend = optional $ GRPC <$> hostParser <*> portParser <*> targetParser

getMakeBakerPayloadCommand :: Mod CommandFields Action
getMakeBakerPayloadCommand =
  command
    "MakeBaker"
    (info
       (MakeBakerPayload <$> 
        (strArgument
          (metavar "BAKER-KEYS" <> help "File with baker public and private keys")) <*>
        (strArgument
          (metavar "ACCOUNT-KEYS" <> help "File with desired baker account and private keys")) <*>
        (optional (strArgument
          (metavar "OUTPUT" <> help "File where the generated transaction is output."))))
       (progDesc "Make the transaction data necessary to become a baker. During beta only concordium genesis accounts can add bakers."))

sendTransactionPayloadCommand :: Mod CommandFields Action
sendTransactionPayloadCommand =
  command
    "SendTransactionPayload"
    (info
       (SendTransactionPayload <$> 
        (strArgument
          (metavar "METADATA" <> help "File with metadata of the transaction.")) <*>
        (strArgument
          (metavar "PAYLOAD" <> help "File with the payload.")) <*>
        (argument
          auto
          (metavar "NET-ID" <>
           help "Network ID for the transaction to be sent through" <>
           value 100 <>
           showDefault))
        )
       (progDesc "Make the transaction data necessary to become a baker. During beta only concordium genesis accounts can add bakers."))

getPeerDataCommand :: Mod CommandFields Action
getPeerDataCommand =
  command
    "GetPeerData"
    (info
       (GetPeerData <$> switch (long "bootstrapper"))
       (progDesc "Query the gRPC server for the node information."))

getNodeInfoCommand :: Mod CommandFields Action
getNodeInfoCommand =
  command
    "GetNodeInfo"
    (info
       (pure GetNodeInfo)
       (progDesc "Query the gRPC server for the node information."))

getBakerPrivateDataCommand :: Mod CommandFields Action
getBakerPrivateDataCommand =
  command
    "GetBakerPrivateData"
    (info
       (pure GetBakerPrivateData)
       (progDesc "Query the gRPC server for the private baker info."))

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
           showDefault) <*>
        flag False True (short 'h' <> long "hook"))
       (progDesc
          "Parse transaction in current context and send it to the baker."))

hookTransactionCommand :: Mod CommandFields Action
hookTransactionCommand =
  command
    "HookTransaction"
    (info
       (HookTransaction <$>
        strArgument
          (metavar "TX-HASH" <> help "Hash of the transaction to query for"))
       (progDesc
          "Query the gRPC for the information about the execution of a transaction."))

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
  optional $
  strOption
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
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query") <*>
        flag False True (short 'a' <> long "all"))
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

startBakerCommand :: Mod CommandFields Action
startBakerCommand =
    command
     "StartBaker"
    (info
       (pure StartBaker)
       (progDesc "Start the baker."))

stopBakerCommand :: Mod CommandFields Action
stopBakerCommand =
    command
     "StopBaker"
    (info
       (pure StopBaker)
       (progDesc "Stop the baker."))

peerConnectCommand :: Mod CommandFields Action
peerConnectCommand =
    command
     "PeerConnect"
    (info
       (GetModuleSource <$>
        strArgument (metavar "PEER-IP" <> help "IP of the peer we want to connect to") <*>
        argument
          auto
          (metavar "PEER-PORT" <> help "Port of the peer we want to connect to"))
       (progDesc "Connect to a specified peer."))

getPeerUptimeCommand :: Mod CommandFields Action
getPeerUptimeCommand =
    command
     "GetPeerUptime"
    (info
       (pure GetPeerUptime)
       (progDesc "Get the node uptime."))

sendMessageCommand :: Mod CommandFields Action
sendMessageCommand =
    command
     "SendMessage"
    (info
       (SendMessage <$>
        strArgument (metavar "NODE-ID" <> help "ID of the recipent") <*>
        argument
          auto
          (metavar "NET-ID" <> help "Network ID") <*>
        strArgument (metavar "MESSAGE" <> help "Message to be sent") <*>
        argument
          auto
          (metavar "BROADCAST" <> help "Is broadcast?"))
       (progDesc "Send a direct message."))

subscriptionStartCommand :: Mod CommandFields Action
subscriptionStartCommand =
    command
     "SubscriptionStart"
    (info
       (pure SubscriptionStart)
       (progDesc "Start the subscription."))

subscriptionStopCommand :: Mod CommandFields Action
subscriptionStopCommand =
    command
     "SubscriptionStop"
    (info
       (pure SubscriptionStop)
       (progDesc "Stop the subscription."))

subscriptionPollCommand :: Mod CommandFields Action
subscriptionPollCommand =
    command
     "SubscriptionPoll"
    (info
       (pure SubscriptionPoll)
       (progDesc "Poll the subscription."))

banNodeCommand :: Mod CommandFields Action
banNodeCommand =
    command
     "BanNode"
    (info
       (BanNode <$>
        strArgument (metavar "NODE-ID" <> help "ID of the node to be banned") <*>
        argument
          auto
          (metavar "NODE-PORT" <> help "Port of the node to be banned") <*>
        argument
          auto
          (metavar "NODE-IP" <> help "IP of the node to be banned"))
       (progDesc "Ban a node."))

unbanNodeCommand :: Mod CommandFields Action
unbanNodeCommand =
    command
     "UnbanNode"
    (info
       (UnbanNode <$>
        strArgument (metavar "NODE-ID" <> help "ID of the node to be unbanned") <*>
        argument
          auto
          (metavar "NODE-PORT" <> help "Port of the node to be unbanned") <*>
        argument
          auto
          (metavar "NODE-IP" <> help "IP of the node to be unbanned"))
       (progDesc "Unban a node."))

joinNetworkCommand :: Mod CommandFields Action
joinNetworkCommand =
    command
     "JoinNetwork"
    (info
       (JoinNetwork <$>
        argument auto (metavar "NET-ID" <> help "ID of the network"))
       (progDesc "Join a network."))

leaveNetworkCommand :: Mod CommandFields Action
leaveNetworkCommand =
    command
     "LeaveNetwork"
    (info
       (LeaveNetwork <$>
        argument auto (metavar "NET-ID" <> help "ID of the network"))
       (progDesc "Leave a network."))

getAncestorsCommand :: Mod CommandFields Action
getAncestorsCommand =
    command
     "GetAncestors"
    (info
       (GetAncestors <$>
        strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query") <*>
        argument
          auto
          (metavar "AMOUNT" <> help "How many ancestors"))
       (progDesc "Get the ancestors of a block."))

getBranchesCommand :: Mod CommandFields Action
getBranchesCommand =
    command
     "GetBranches"
    (info
       (pure GetBranches)
       (progDesc "Get branches in consensus."))

getBannedPeersCommand :: Mod CommandFields Action
getBannedPeersCommand =
    command
     "GetBannedPeers"
    (info
       (pure GetBannedPeers)
       (progDesc "Get banned peers."))

shutdownCommand :: Mod CommandFields Action
shutdownCommand =
    command
    "Shutdown"
    (info
       (pure Shutdown)
       (progDesc "Shutdown the node gracefully."))

tpsTestCommand :: Mod CommandFields Action
tpsTestCommand =
    command
     "TpsTest"
    (info
       (TpsTest <$>
        argument
          auto
          (metavar "NET_ID" <> help "Network ID") <*>
        strArgument (metavar "NODE-ID" <> help "ID of the node") <*>
        strArgument (metavar "DIR" <> help "Directory"))
       (progDesc "Tps test."))

dumpStartCommand :: Mod CommandFields Action
dumpStartCommand =
    command
    "DumpStart"
    (info
       (pure DumpStart)
       (progDesc "Start dumping the packages."))

dumpStopCommand :: Mod CommandFields Action
dumpStopCommand =
    command
    "DumpStop"
    (info
       (pure DumpStop)
       (progDesc "Stop dumping the packages."))

retransmitRequestCommand :: Mod CommandFields Action
retransmitRequestCommand =
    command
     "RetransmitRequest"
    (info
       (RetransmitRequest <$>
        strArgument (metavar "ID" <> help "ID") <*>
        argument
          auto
          (metavar "ELEMENT" <> help "Type of element to request for") <*>
        argument
          auto
          (metavar "SINCE" <> help "Time epoch") <*>
        argument
          auto
          (metavar "NET_ID" <> help "Network ID"))
       (progDesc "Request a retransmision of specific elements."))

getSkovStatsCommand :: Mod CommandFields Action
getSkovStatsCommand =
     command
    "GetSkovStats"
    (info
       (pure GetSkovStats)
       (progDesc "Get skov statistics."))
