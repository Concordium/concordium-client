module Concordium.Client.LegacyCommands (
    EpochSpecifier (..),
    LegacyCmd (..),
    legacyProgramOptions,
) where

import Concordium.Types
import Data.Text
import Options.Applicative

-- | Representation of the arguments to a command that expects an epoch to be specified as input.
data EpochSpecifier = EpochSpecifier
    { -- | The genesis index to query. Should be provided with 'esEpoch'.
      esGenesisIndex :: !(Maybe GenesisIndex),
      -- | The epoch number to query. Should be provided with 'esGenesisIndex'.
      esEpoch :: !(Maybe Epoch),
      -- | The block to use the epoch of. Should not be provided with any other fields.
      esBlock :: !(Maybe BlockHash)
    }
    deriving (Show)

-- | Helper function for parsing an 'EpochSpecifier' as command line options.
parseEpochSpecifier :: Parser EpochSpecifier
parseEpochSpecifier =
    EpochSpecifier
        <$> optional (option auto (long "genesis-index" <> metavar "GENINDEX" <> help "Genesis index (use with --epoch)"))
        <*> optional (option auto (long "epoch" <> metavar "EPOCH" <> help "Epoch number (use with --genesis-index)"))
        <*> optional (option auto (long "block" <> metavar "BLOCKHASH" <> help "Block hash"))

data LegacyCmd
    = -- | Queries the gRPC for the information about the execution of a transaction
      GetTransactionStatus
        { legacyTransactionHash :: !Text
        }
    | -- | Get non finalized transactions for a given account.
      GetAccountNonFinalized
        { legacyAddress :: !Text
        }
    | -- | Get non finalized transactions for a given account.
      GetNextAccountNonce
        { legacyAddress :: !Text
        }
    | -- | Queries the gRPC server for the consensus information
      GetConsensusInfo
    | -- | Queries the gRPC server for detailed consensus status
      GetConsensusDetailedStatus {legacyFromGenesisIndex :: !(Maybe GenesisIndex)}
    | -- | Queries the gRPC server for the information of a specific block
      GetBlockInfo
        { legacyEvery :: !Bool,
          legacyBlockHash :: !(Maybe Text)
        }
    | GetBlockPendingUpdates
        { legacyBlockHash :: !(Maybe Text)
        } --  ^Queries the gRPC server for the pending updates in a specific block.
    | GetBlockTransactionEvents
        { legacyBlockHash :: !(Maybe Text)
        } --  ^Queries the gRPC server for the transaction events in a specific block.
    | GetBlockSpecialEvents
        { legacyBlockHash :: !(Maybe Text)
        } --  ^Queries the gRPC server for the special events in a specific block.
    | GetBlockChainParameters
        { legacyBlockHash :: !(Maybe Text)
        } --  ^Queries the gRPC server for the chain parameters in a specific block.
    | GetBlockFinalizationSummary
        { legacyBlockHash :: !(Maybe Text)
        } --  ^Queries the gRPC server for the finalization summary in a specific block.
    | -- | Queries the gRPC server for the list of blocks with a given height.
      GetBlocksAtHeight
        { legacyBlockHeight :: !BlockHeight,
          legacyFromGenesisIndex :: !(Maybe GenesisIndex),
          legacyRestrictToGenesis :: !(Maybe Bool)
        }
    | -- | Queries the gRPC server for the list of accounts on a specific block
      GetAccountList
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the list of instances on a specific block
      GetInstances
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the information of an account on a specific block
      GetAccountInfo
        { legacyAddress :: !Text,
          legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the status of a pool on a specific block
      GetPoolStatus
        { legacyPool :: !(Maybe BakerId),
          legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the list of bakers on a specific block
      GetBakerList
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the information of an instance on a specific block
      GetInstanceInfo
        { legacyContractAddress :: !Text,
          legacyBlockHash :: !(Maybe Text)
        }
    | -- | Invokes a contract locally on the node.
      InvokeContract
        { legacyContextFile :: !FilePath,
          legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the reward status on a specific block
      GetRewardStatus
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the Birk parameters on a specific block
      GetBirkParameters
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the list of modules on a specific block
      GetModuleList
        { legacyBlockHash :: !(Maybe Text)
        }
    | -- | Queries the gRPC server for the node information.
      GetNodeInfo
    | GetPeerData
        { -- | Whether to include bootstrapper node in the stats or not.
          legacyIncludeBootstrapper :: !Bool
        }
    | -- \^ Get all data as pertaining to the node's role as a member of the P2P network.
      PeerConnect
        { legacyIp :: !Text,
          legacyPortPC :: !Int
        }
    | PeerDisconnect
        { legacyIp :: !Text,
          legacyPortPC :: !Int
        }
    | GetPeerUptime
    | BanNode
        { legacyNodeIp :: !Text
        }
    | UnbanNode
        { legacyNodeIp :: !Text
        }
    | GetAncestors
        { legacyAmount :: !Int,
          legacyBlockHash :: !(Maybe Text) -- defaults to last finalized block
        }
    | GetBranches
    | GetBannedPeers
    | Shutdown
    | DumpStart
        { -- | Path of the file to write the dumped packages to
          legacyFilepath :: !Text,
          -- | Dump raw packages if true
          legacyRaw :: !Bool
        }
    | DumpStop
    | GetIdentityProviders
        {legacyBlockHash :: !(Maybe Text)}
    | GetAnonymityRevokers
        {legacyBlockHash :: !(Maybe Text)}
    | GetCryptographicParameters
        {legacyBlockHash :: !(Maybe Text)}
    | GetNextUpdateSequenceNumbers
        {legacyBlockHash :: !(Maybe Text)}
    | GetBakersRewardPeriod
        {legacyBlockHash :: !(Maybe Text)}
    | GetBlockCertificates
        {legacyBlockHash :: !(Maybe Text)}
    | GetBakerEarliestWinTime
        { legacyBakerId :: !BakerId
        }
    | GetWinningBakersEpoch
        {legacyEpoch :: !EpochSpecifier}
    | GetFirstBlockEpoch
        {legacyEpoch :: !EpochSpecifier}
    | -- | Get the list of accounts with scheduled releases.
      GetScheduledReleaseAccounts
        {legacyBlockHash :: !(Maybe Text)}
    | -- | Get the list of accounts with stake in cooldown.
      GetCooldownAccounts
        {legacyBlockHash :: !(Maybe Text)}
    | -- | Get the list of accounts with stake in pre-cooldown.
      GetPreCooldownAccounts
        {legacyBlockHash :: !(Maybe Text)}
    | -- | Get the list of accounts with stake in pre-pre-cooldown.
      GetPrePreCooldownAccounts
        {legacyBlockHash :: !(Maybe Text)}
    deriving (Show)

legacyProgramOptions :: Parser LegacyCmd
legacyProgramOptions =
    hsubparser
        ( getTransactionStatusCommand
            <> getConsensusInfoCommand
            <> getConsensusDetailedStatusCommand
            <> getBlockInfoCommand
            <> getBlockPendingUpdatesCommand
            <> getBlockTransactionEventsCommand
            <> getBlockSpecialEventsCommand
            <> getBlockChainParametersCommand
            <> getBlockFinalizationSummaryCommand
            <> getBlocksAtHeightCommand
            <> getAccountListCommand
            <> getInstancesCommand
            <> getAccountInfoCommand
            <> getAccountNonFinalizedCommand
            <> getNextAccountNonceCommand
            <> getInstanceInfoCommand
            <> invokeContractCommand
            <> getPoolStatusCommand
            <> getBakerListCommand "GetValidatorList"
            <> getRewardStatusCommand
            <> getBirkParametersCommand
            <> getModuleListCommand
            <> getNodeInfoCommand
            <> getPeerDataCommand
            <> peerConnectCommand
            <> peerDisconnectCommand
            <> getPeerUptimeCommand
            <> banNodeCommand
            <> unbanNodeCommand
            <> getAncestorsCommand
            <> getBranchesCommand
            <> getBannedPeersCommand
            <> shutdownCommand
            <> dumpStartCommand
            <> dumpStopCommand
            <> getIdentityProvidersCommand
            <> getAnonymityRevokersCommand
            <> getCryptographicParametersCommand
            <> getNextUpdateSequenceNumbersCommand
            <> getScheduledReleaseAccountsCommand
            <> getCooldownAccountsCommand
            <> getPreCooldownAccountsCommand
            <> getPrePreCooldownAccountsCommand
            <> getBakersRewardPeriodCommand "GetValidatorsRewardPeriod"
            <> getBlockCertificatesCommand
            <> getBakerEarliestWinTimeCommand "GetValidatorEarliestWinTime"
            <> getWinningBakersEpochCommand "GetWinningValidatorsEpoch"
            <> getFirstBlockEpochCommand
        )
        <|> hsubparser
            ( internal
                <> getBakerListCommand "GetBakerList"
                <> getBakersRewardPeriodCommand "GetBakersRewardPeriod"
                <> getBakerEarliestWinTimeCommand "GetBakerEarliestWinTime"
                <> getWinningBakersEpochCommand "GetWinningBakersEpoch"
            )

getPeerDataCommand :: Mod CommandFields LegacyCmd
getPeerDataCommand =
    command
        "GetPeerData"
        ( info
            (GetPeerData <$> switch (long "bootstrapper" <> help "Include the bootstrapper in the peer data"))
            (progDesc "Query the gRPC server for the node information.")
        )

getNodeInfoCommand :: Mod CommandFields LegacyCmd
getNodeInfoCommand =
    command
        "GetNodeInfo"
        ( info
            (pure GetNodeInfo)
            (progDesc "Query the gRPC server for the node information.")
        )

getTransactionStatusCommand :: Mod CommandFields LegacyCmd
getTransactionStatusCommand =
    command
        "GetTransactionStatus"
        ( info
            ( GetTransactionStatus
                <$> strArgument
                    (metavar "TX-HASH" <> help "Hash of the transaction to query for")
            )
            ( progDesc
                "Query the gRPC for the information about the execution of a transaction."
            )
        )

getConsensusInfoCommand :: Mod CommandFields LegacyCmd
getConsensusInfoCommand =
    command
        "GetConsensusInfo"
        ( info
            (pure GetConsensusInfo)
            (progDesc "Query the gRPC server for the consensus information.")
        )

getConsensusDetailedStatusCommand :: Mod CommandFields LegacyCmd
getConsensusDetailedStatusCommand =
    command
        "GetConsensusDetailedStatus"
        ( info
            ( GetConsensusDetailedStatus
                <$> optional (option auto (long "genesis-index" <> metavar "GENINDEX" <> help "Consensus genesis index"))
            )
            (progDesc "Query the gRPC server for the detailed consensus status. If the genesis index is not specified, the current one is used.")
        )

getBlockInfoCommand :: Mod CommandFields LegacyCmd
getBlockInfoCommand =
    command
        "GetBlockInfo"
        ( info
            ( GetBlockInfo
                <$> switch (short 'a' <> long "all" <> help "Traverse all parent blocks and get their info as well")
                <*> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for a specific block.")
        )

getBlockPendingUpdatesCommand :: Mod CommandFields LegacyCmd
getBlockPendingUpdatesCommand =
    command
        "GetBlockPendingUpdates"
        ( info
            ( GetBlockPendingUpdates
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the pending updates in a specific block.")
        )

getBlockTransactionEventsCommand :: Mod CommandFields LegacyCmd
getBlockTransactionEventsCommand =
    command
        "GetBlockTransactionEvents"
        ( info
            ( GetBlockTransactionEvents
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the transaction events in a specific block.")
        )

getBlockSpecialEventsCommand :: Mod CommandFields LegacyCmd
getBlockSpecialEventsCommand =
    command
        "GetBlockSpecialEvents"
        ( info
            ( GetBlockSpecialEvents
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the special events in a specific block.")
        )

getBlockChainParametersCommand :: Mod CommandFields LegacyCmd
getBlockChainParametersCommand =
    command
        "GetBlockChainParameters"
        ( info
            ( GetBlockChainParameters
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the chain parameters at a specific block.")
        )

getBlockFinalizationSummaryCommand :: Mod CommandFields LegacyCmd
getBlockFinalizationSummaryCommand =
    command
        "GetBlockFinalizationSummary"
        ( info
            ( GetBlockFinalizationSummary
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the finalization summary in a specific block.")
        )

getBlocksAtHeightCommand :: Mod CommandFields LegacyCmd
getBlocksAtHeightCommand =
    command
        "GetBlocksAtHeight"
        ( info
            ( GetBlocksAtHeight
                <$> argument auto (metavar "HEIGHT" <> help "Height of the blocks to query")
                <*> optional (option auto (long "genesis-index" <> metavar "GENINDEX" <> help "Base genesis index"))
                <*> flag Nothing (Just True) (long "restrict" <> help "Restrict to specified genesis index")
            )
            (progDesc "Query the gRPC server for all blocks at the given height.")
        )

getAccountListCommand :: Mod CommandFields LegacyCmd
getAccountListCommand =
    command
        "GetAccountList"
        ( info
            ( GetAccountList
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the list of accounts.")
        )

getInstancesCommand :: Mod CommandFields LegacyCmd
getInstancesCommand =
    command
        "GetInstances"
        ( info
            ( GetInstances
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the list of instances.")
        )

getAccountInfoCommand :: Mod CommandFields LegacyCmd
getAccountInfoCommand =
    command
        "GetAccountInfo"
        ( info
            ( GetAccountInfo
                <$> strArgument (metavar "IDENTIFIER" <> help "Account address, account index or credential id to be queried about")
                <*> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the information of an account.")
        )

getAccountNonFinalizedCommand :: Mod CommandFields LegacyCmd
getAccountNonFinalizedCommand =
    command
        "GetAccountNonFinalized"
        ( info
            ( GetAccountNonFinalized
                <$> strArgument (metavar "ACCOUNT" <> help "Account to be queried about")
            )
            (progDesc "Query the gRPC server for the information on non-finalized transactions for an account.")
        )

getNextAccountNonceCommand :: Mod CommandFields LegacyCmd
getNextAccountNonceCommand =
    command
        "GetNextAccountNonce"
        ( info
            ( GetNextAccountNonce
                <$> strArgument (metavar "ACCOUNT" <> help "Account to be queried about")
            )
            (progDesc "Query the gRPC server for the best guess on the next account nonce.")
        )

getNextUpdateSequenceNumbersCommand :: Mod CommandFields LegacyCmd
getNextUpdateSequenceNumbersCommand =
    command
        "GetNextUpdateSequenceNumbers"
        ( info
            ( GetNextUpdateSequenceNumbers
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the next update sequence numbers for all update queues.")
        )

getScheduledReleaseAccountsCommand :: Mod CommandFields LegacyCmd
getScheduledReleaseAccountsCommand =
    command
        "GetScheduledReleaseAccounts"
        ( info
            ( GetScheduledReleaseAccounts
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for all accounts that have scheduled releases, with the timestamp of the first pending scheduled release for that account.")
        )

getCooldownAccountsCommand :: Mod CommandFields LegacyCmd
getCooldownAccountsCommand =
    command
        "GetCooldownAccounts"
        ( info
            ( GetCooldownAccounts
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for all accounts that have stake in cooldown, with the timestamp of the first pending cooldown expiry for each account.")
        )

getPreCooldownAccountsCommand :: Mod CommandFields LegacyCmd
getPreCooldownAccountsCommand =
    command
        "GetPreCooldownAccounts"
        ( info
            ( GetPreCooldownAccounts
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for all accounts that have stake in pre-cooldown.")
        )

getPrePreCooldownAccountsCommand :: Mod CommandFields LegacyCmd
getPrePreCooldownAccountsCommand =
    command
        "GetPrePreCooldownAccounts"
        ( info
            ( GetPrePreCooldownAccounts
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for all accounts that have stake in pre-pre-cooldown.")
        )

getBakersRewardPeriodCommand :: String -> Mod CommandFields LegacyCmd
getBakersRewardPeriodCommand name =
    command
        name
        ( info
            ( GetBakersRewardPeriod
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the validators of the reward period given by the block.")
        )

getBlockCertificatesCommand :: Mod CommandFields LegacyCmd
getBlockCertificatesCommand =
    command
        "GetBlockCertificates"
        ( info
            ( GetBlockCertificates
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the certificates of a block.")
        )

getBakerEarliestWinTimeCommand :: String -> Mod CommandFields LegacyCmd
getBakerEarliestWinTimeCommand name =
    command
        name
        ( info
            ( GetBakerEarliestWinTime
                <$> argument auto (metavar "VALIDATOR-ID" <> help "Validator ID of the validator.")
            )
            ( progDesc
                "Query the gRPC server for the earliest time that a given validator is expected \
                \to bake."
            )
        )

getInstanceInfoCommand :: Mod CommandFields LegacyCmd
getInstanceInfoCommand =
    command
        "GetInstanceInfo"
        ( info
            ( GetInstanceInfo
                <$> strArgument (metavar "INSTANCE" <> help "Contract address to be queried about")
                <*> optional
                    ( strArgument
                        ( metavar "BLOCK-HASH"
                            <> help "Hash of the block to query (default: Query the best block)"
                        )
                    )
            )
            (progDesc "Query the gRPC server for the information of an instance.")
        )

invokeContractCommand :: Mod CommandFields LegacyCmd
invokeContractCommand =
    command
        "InvokeContract"
        ( info
            ( InvokeContract
                <$> strArgument (metavar "CONTEXT" <> help "JSON file with the context")
                <*> optional
                    ( strArgument
                        ( metavar "BLOCK-HASH"
                            <> help "Hash of the block to query (default: Query the best block)"
                        )
                    )
            )
            (progDesc "Invoke a smart contract in the state of the given block.")
        )
getPoolStatusCommand :: Mod CommandFields LegacyCmd
getPoolStatusCommand =
    command
        "GetPoolStatus"
        ( info
            ( GetPoolStatus
                <$> optional (option auto (long "pool" <> metavar "POOL" <> help "Validator ID of pool. If not provided, status of passive delegation is queried."))
                <*> optional
                    ( strArgument
                        ( metavar "BLOCK-HASH"
                            <> help "Hash of the block to query (default: Query the best block)"
                        )
                    )
            )
            (progDesc "Query the gRPC server for the status of a validator pool or passive delegation.")
        )

getBakerListCommand :: String -> Mod CommandFields LegacyCmd
getBakerListCommand name =
    command
        name
        ( info
            ( GetBakerList
                <$> optional
                    ( strArgument
                        ( metavar "BLOCK-HASH"
                            <> help "Hash of the block to query (default: Query the best block)"
                        )
                    )
            )
            (progDesc "Query the gRPC server for the list of validators.")
        )

getRewardStatusCommand :: Mod CommandFields LegacyCmd
getRewardStatusCommand =
    command
        "GetRewardStatus"
        ( info
            ( GetRewardStatus
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the reward status.")
        )

getBirkParametersCommand :: Mod CommandFields LegacyCmd
getBirkParametersCommand =
    command
        "GetBirkParameters"
        ( info
            ( GetBirkParameters
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the Birk parameters.")
        )

getModuleListCommand :: Mod CommandFields LegacyCmd
getModuleListCommand =
    command
        "GetModuleList"
        ( info
            ( GetModuleList
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the list of modules.")
        )

peerConnectCommand :: Mod CommandFields LegacyCmd
peerConnectCommand =
    command
        "PeerConnect"
        ( info
            ( PeerConnect
                <$> strArgument (metavar "PEER-IP" <> help "IP of the peer we want to connect to")
                <*> argument
                    auto
                    (metavar "PEER-PORT" <> help "Port of the peer we want to connect to")
            )
            (progDesc "Connect to a specified peer.")
        )

peerDisconnectCommand :: Mod CommandFields LegacyCmd
peerDisconnectCommand =
    command
        "PeerDisconnect"
        ( info
            ( PeerDisconnect
                <$> strArgument (metavar "PEER-IP" <> help "IP of the peer we want to disconnect from")
                <*> argument
                    auto
                    (metavar "PEER-PORT" <> help "Port of the peer we want to disconnect from")
            )
            (progDesc "Disconnect from a specified peer.")
        )

getPeerUptimeCommand :: Mod CommandFields LegacyCmd
getPeerUptimeCommand =
    command
        "GetPeerUptime"
        ( info
            (pure GetPeerUptime)
            (progDesc "Get the node uptime in milliseconds.")
        )

banNodeCommand :: Mod CommandFields LegacyCmd
banNodeCommand =
    command
        "BanNode"
        ( info
            ( BanNode
                <$> strArgument
                    (metavar "NODE-IP" <> help "IP of the node to be banned")
            )
            (progDesc "Ban a node.")
        )

unbanNodeCommand :: Mod CommandFields LegacyCmd
unbanNodeCommand =
    command
        "UnbanNode"
        ( info
            ( UnbanNode
                <$> strArgument
                    (metavar "NODE-IP" <> help "IP of the node to be unbanned")
            )
            (progDesc "Unban a node.")
        )

getAncestorsCommand :: Mod CommandFields LegacyCmd
getAncestorsCommand =
    command
        "GetAncestors"
        ( info
            ( GetAncestors
                <$> argument
                    auto
                    (metavar "AMOUNT" <> help "How many ancestors")
                <*> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Get the ancestors of a block.")
        )

getBranchesCommand :: Mod CommandFields LegacyCmd
getBranchesCommand =
    command
        "GetBranches"
        ( info
            (pure GetBranches)
            (progDesc "Get branches in consensus.")
        )

getBannedPeersCommand :: Mod CommandFields LegacyCmd
getBannedPeersCommand =
    command
        "GetBannedPeers"
        ( info
            (pure GetBannedPeers)
            (progDesc "Get banned peers.")
        )

shutdownCommand :: Mod CommandFields LegacyCmd
shutdownCommand =
    command
        "Shutdown"
        ( info
            (pure Shutdown)
            (progDesc "Request the node to shut down its P2P network layer. This does not terminate the node process and the node will still respond to consensus queries.")
        )

dumpStartCommand :: Mod CommandFields LegacyCmd
dumpStartCommand =
    command
        "DumpStart"
        ( info
            ( DumpStart
                <$> strArgument
                    (metavar "FILE" <> help "Path of the file to write the dumped packages to")
                <*> flag False True (long "restrict" <> help "Dump raw packages")
            )
            (progDesc "Start dumping the packages.")
        )

dumpStopCommand :: Mod CommandFields LegacyCmd
dumpStopCommand =
    command
        "DumpStop"
        ( info
            (pure DumpStop)
            (progDesc "Stop dumping the packages.")
        )

getIdentityProvidersCommand :: Mod CommandFields LegacyCmd
getIdentityProvidersCommand =
    command
        "GetIdentityProviders"
        ( info
            ( GetIdentityProviders
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the identity providers in a specific block.")
        )

getAnonymityRevokersCommand :: Mod CommandFields LegacyCmd
getAnonymityRevokersCommand =
    command
        "GetAnonymityRevokers"
        ( info
            ( GetAnonymityRevokers
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the anonymity revokers in a specific block.")
        )

getCryptographicParametersCommand :: Mod CommandFields LegacyCmd
getCryptographicParametersCommand =
    command
        "GetCryptographicParameters"
        ( info
            ( GetCryptographicParameters
                <$> optional (strArgument (metavar "BLOCK-HASH" <> help "Hash of the block to query (default: Query the best block)"))
            )
            (progDesc "Query the gRPC server for the cryptographic parameters in a specific block.")
        )

getWinningBakersEpochCommand :: String -> Mod CommandFields LegacyCmd
getWinningBakersEpochCommand name =
    command
        name
        ( info
            (GetWinningBakersEpoch <$> parseEpochSpecifier)
            (progDesc "Query the winning validators for an epoch.")
        )

getFirstBlockEpochCommand :: Mod CommandFields LegacyCmd
getFirstBlockEpochCommand =
    command
        "GetFirstBlockEpoch"
        ( info
            (GetFirstBlockEpoch <$> parseEpochSpecifier)
            (progDesc "Query the first finalized block of an epoch. (Default: the epoch of the last finalized block.)")
        )
