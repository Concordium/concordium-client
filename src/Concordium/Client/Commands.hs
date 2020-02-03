module Concordium.Client.Commands
  ( optsParser
  , backendParser
  , Options(..)
  , Command(..)
  , Backend(..)
  , LegacyAction(..)
  ) where

import Data.Version (showVersion)
import Network.HTTP2.Client
import Options.Applicative
import Paths_simple_client (version)
import Concordium.Client.LegacyCommands

data Options =
  Options
  { cmd :: Command
  , backend :: Maybe Backend }
  deriving (Show)

data Backend =
  GRPC
    { host   :: HostName
    , port   :: PortNumber
    , target :: Maybe String }
  deriving (Show)

data Command
  = LegacyCommand
    { legacyAction  :: LegacyAction }
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

programOptions :: Parser Options
programOptions = Options <$>
                   (LegacyCommand <$> legacyProgramOptions) <*>
                   (optional backendParser)
