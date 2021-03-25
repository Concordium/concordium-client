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
  , Interval(..)
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

import Data.Text hiding (map, unlines)
import Data.Version (showVersion)
import Data.Word (Word64)
import Data.Time.Format.ISO8601
import Network.HTTP2.Client
import Options.Applicative
import Paths_concordium_client (version)
import Concordium.Client.LegacyCommands
import Concordium.Client.Types.Account
import Concordium.Client.Utils
import Concordium.Common.Time
import Concordium.ID.Types (CredentialIndex, KeyIndex, CredentialRegistrationID)
import Concordium.Types
import Text.Printf
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Control.Monad
import Options.Applicative.Help.Pretty (hang, softline, fillCat)

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
  deriving (Show)

data ConfigCmd
  = ConfigInit
  | ConfigShow
  | ConfigBackupExport
    { cbeFileName :: !FilePath }
  | ConfigBackupImport
    { cbiFileName :: !FilePath
    , cbiSkipExisting :: !Bool }
  | ConfigAccountCmd -- groups 'config account' commands
    { configAccountCmd :: ConfigAccountCmd }
  deriving (Show)

data ConfigAccountCmd
  = ConfigAccountName
    { caaAddr :: !Text
    , caaName :: !Text }
  | ConfigAccountRemove
    { carAddr :: !Text }
  | ConfigAccountImport
    { caiFile :: !FilePath
    , caiName :: !(Maybe Text)
    , caiFormat :: !AccountExportFormat 
    , caiSkipExisting :: !Bool }
  | ConfigAccountAddKeys
    { caakAddr :: !Text
    , caakKeysFile :: !FilePath }
  | ConfigAccountUpdateKeys
    { caukAddr :: !Text
    , caukKeysFile :: !FilePath }
  | ConfigAccountRemoveKeys
    { carkAddr :: !Text
    , carkCidx :: !CredentialIndex
    , carkKeys :: ![KeyIndex]
    }
  | ConfigAccountRemoveName
    { carnText :: !Text }
  deriving (Show)

data Interval = Minute -- 60 secs
              | Hour -- 60 mins
              | Day -- 24 hours
              | Week -- 7 days
              | Month -- 30 days
              | Year -- 365 days
              deriving (Show, Read)

data TransactionCmd
  = TransactionSubmit
    { tsFile :: !FilePath
    , tsInteractionOpts :: !InteractionOpts }
  | TransactionStatus
    { tsHash :: !Text }
  | TransactionSendGtu
    { tsgReceiver :: !Text
    , tsgAmount :: !Amount
    , tsgOpts :: !(TransactionOpts (Maybe Energy)) }
  | TransactionSendWithSchedule
    { twsReceiver :: !Text
    , twsSchedule :: !(Either (Amount, Interval, Int, Timestamp) [(Timestamp, Amount)]) -- ^Eiher total amount, interval, number of intervals and starting time or a raw list of timestamps and amounts.
    , twsOpts :: !(TransactionOpts (Maybe Energy)) }
  | TransactionDeployCredential
    { tdcFile :: !FilePath
    , tdcInteractionOpts :: !InteractionOpts }
  | TransactionEncryptedTransfer
    { tetTransactionOpts :: !(TransactionOpts (Maybe Energy)),
      -- | Address of the receiver.
      tetReceiver :: !Text,
      -- | Amount to send.
      tetAmount :: !Amount,
      -- | Which indices to use as inputs to the encrypted amount transfer.
      -- If none are provided all existing ones will be used.
      tetIndex :: !(Maybe Int) }
  -- | Register data on chain.
  | TransactionRegisterData
    { -- | File containing the data.
      trdFile :: !FilePath,
      -- | Options for transaction.
      trdTransactionOptions :: !(TransactionOpts (Maybe Energy))
    }

  deriving (Show)

data AccountCmd
  = AccountShow
    { asAddress :: !(Maybe Text)
    , asBlockHash :: !(Maybe Text)
    , asShowEncryptedBalance :: !Bool
    , asDecryptEncryptedBalance :: !Bool }
  | AccountList
    { alBlockHash :: !(Maybe Text) }
  | AccountUpdateKeys
    { aukKeys :: !FilePath
    , aukCredId :: !CredentialRegistrationID
    , aukTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  -- |Transfer part of the public balance to the encrypted balance of the
  -- account.
  | AccountEncrypt
    { aeTransactionOpts :: !(TransactionOpts (Maybe Energy)),
      -- | Amount to transfer from public to encrypted balance.
      aeAmount :: !Amount
    }
  -- |Transfer part of the encrypted balance to the public balance of the
  -- account.
  | AccountDecrypt
    { adTransactionOpts :: !(TransactionOpts (Maybe Energy)),
      -- |Amount to transfer from encrypted to public balance.
      adAmount :: !Amount,
      -- | Which indices of incoming amounts to use as inputs.
      -- If none are provided all existing ones will be used.
      adIndex :: !(Maybe Int)
    }
  deriving (Show)

data ModuleCmd
  -- |Deploy the provided smart contract module on chain.
  = ModuleDeploy
    { -- |Path to the module.
      mdModuleFile :: !FilePath
      -- |Local alias for the module reference.
    , mdName :: !(Maybe Text)
      -- |Options for transaction.
    , mdTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  -- |List all modules.
  | ModuleList
    { -- |Hash of the block (default "best").
      mlBlockHash :: !(Maybe Text) }
  -- |Output the binary source code of the module to the provided file.
  | ModuleShow
    { -- |Reference to the module OR a module name.
      msModuleRefOrName :: !Text
      -- |Output the module to this file.
      -- Use '-' to output to stdout.
    , msOutFile :: !FilePath
      -- |Hash of the block (default "best").
    , msBlockHash :: !(Maybe Text) }
  -- |Show the functions available in a module, including type signatures if schema is provided.
  | ModuleInspect
    { -- |Reference to the module OR a module name.
      miModuleRefOrName :: !Text
      -- |Path to a contract schema, used to display the type signatures.
    , miSchema :: !(Maybe FilePath)
      -- |Hash of the block (default "best").
    , miBlockHash :: !(Maybe Text) }
  -- |Add a local name to a module.
  | ModuleName
    { -- |Module reference OR path to the module (reference then calculated by hashing).
      mnModule :: !String
      -- |Name for the module.
    , mnName :: !Text
    }
  deriving (Show)

data ContractCmd
  -- |Show the state of specified contract.
  = ContractShow
    { -- |Index of the contract address OR a contract name.
      csAddressIndexOrName :: !Text
      -- |Subindex of the address for the contract (default: 0).
    , csAddressSubindex :: !(Maybe Word64)
      -- |Path to a contract schema, used to display the contract info.
    , csSchema :: !(Maybe FilePath)
      -- |Hash of the block (default "best").
    , csBlockHash :: !(Maybe Text) }
  -- |List all contracts on chain.
  | ContractList
    { -- |Hash of the block (default "best").
      clBlockHash :: !(Maybe Text) }
  -- |Initialize a contract from a module on chain.
  | ContractInit
    { -- |Module reference OR module name OR (if ciPath == True) path to the module (reference then calculated by hashing).
      ciModule :: !String
      -- |Name of the contract to initialize. This corresponds to a specific init function.
    , ciContractName :: !Text
      -- |Path to a JSON file containing parameters for the init function (only one type of parameter is allowed).
    , ciParameterFileJSON :: !(Maybe FilePath)
      -- |Path to a binary file containing parameters for the init function.
    , ciParameterFileBinary :: !(Maybe FilePath)
      -- |Path to a contract schema.
    , ciSchema :: !(Maybe FilePath)
      -- |Local alias for the contract address.
    , ciName :: !(Maybe Text)
      -- |Determines whether ciModule should be interpreted as a path.
    , ciPath :: !Bool
      -- |Amount to be send to contract (default: 0).
    , ciAmount :: !Amount
      -- |Options for transaction.
    , ciTransactionOpts :: !(TransactionOpts Energy) }
  -- |Update an existing contract, i.e. invoke a receive function.
  | ContractUpdate
    { -- |Index of the contract address OR a contract name.
      cuAddressIndexOrName :: !Text
      -- |Subindex of the address for the contract to invoke (default: 0).
    , cuAddressSubindex :: !(Maybe Word64)
      -- |Name of the receive function to use.
    , cuReceiveName :: !Text
      -- |Path to a JSON file containing parameters for the receive function (only one type of parameter is allowed).
    , cuParameterFileJSON :: !(Maybe FilePath)
      -- |Path to a binary file containing parameters for the receive function.
    , cuParameterFileBinary :: !(Maybe FilePath)
      -- |Path to a contract schema.
    , cuSchema :: !(Maybe FilePath)
      -- |Amount to invoke the receive function with (default: 0).
    , cuAmount :: !Amount
      -- |Options for transaction.
    , cuTransactionOpts :: !(TransactionOpts Energy) }
  -- |Add a local name to a contract.
  | ContractName
    { -- |Index of the address for the contract.
      cnAddressIndex :: !Word64
      -- |Subindex of the address for the contract (default: 0).
    , cnAddressSubindex :: !(Maybe Word64)
      -- |Name for the contract.
    , cnName :: !Text }
  deriving (Show)

-- | The type parameter 'energyOrMaybe' should be Energy or Maybe Energy.
data TransactionOpts energyOrMaybe =
  TransactionOpts
  { toSender :: !(Maybe Text)
  , toKeys :: !(Maybe FilePath)
  , toSigners :: !(Maybe Text)
  , toNonce :: !(Maybe Nonce)
  , toMaxEnergyAmount :: !energyOrMaybe
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
  | ConsensusChainUpdate
    { ccuUpdate :: !FilePath
    , ccuKeys :: ![FilePath]
    , ccuInteractionOpts :: !InteractionOpts }
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
    , baTransactionOpts :: !(TransactionOpts (Maybe Energy))
    , baStake :: !Amount
    , baAutoAddEarnings :: !Bool
    , outputFile :: Maybe FilePath }
  | BakerSetKeys
    { bsaKeysFile :: !FilePath
    , bsaTransactionOpts :: !(TransactionOpts (Maybe Energy))
    , bsaOutputFile :: Maybe FilePath }
  | BakerRemove
    { brTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerUpdateStake
    { busStake :: !Amount
    , busTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerUpdateRestakeEarnings
    { bursRestake :: !Bool
    , bursTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
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

type ShowAllOpts = Bool

internalUnless :: ShowAllOpts -> Mod f a
internalUnless showAllOpts = if showAllOpts then idm else internal

visibleHelper :: Parser (a -> a)
visibleHelper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short 'h'
  , help "Show detailed help text." ]

optsParser :: ShowAllOpts -> ParserInfo Options
optsParser showAllOpts =
  info
    (programOptions showAllOpts <**> versionOption <**> visibleHelper)
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

-- |Parse transactionOpts with an optional energy flag
transactionOptsParser :: Parser (TransactionOpts (Maybe Energy))
transactionOptsParser = transactionOptsParserBuilder $
    optional (option (eitherReader energyFromStringInform) (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction."))

-- |Parse transactionOpts with a required energy flag
requiredEnergyTransactionOptsParser :: Parser (TransactionOpts Energy)
requiredEnergyTransactionOptsParser = transactionOptsParserBuilder $
    option (eitherReader energyFromStringInform) (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction.")

-- |Helper function to build an transactionOptsParser with or without a required energy flag
transactionOptsParserBuilder :: Parser energyOrMaybe -> Parser (TransactionOpts energyOrMaybe)
transactionOptsParserBuilder energyOrMaybeParser =
  TransactionOpts <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender.")) <*>
    -- TODO Specify / refer to format of JSON file when new commands (e.g. account add-keys) that accept same format are
    -- added.
    optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file.")) <*>
    optional (strOption (long "signers" <> metavar "SIGNERS" <> help "Specification of which (local) keys to sign with. Example: \"0:1,0:2,3:0,3:1\" specifies that credential holder 0 signs with keys 1 and 2, while credential holder 3 signs with keys 0 and 1")) <*>
    optional (option auto (long "nonce" <> metavar "NONCE" <> help "Transaction nonce.")) <*>
    energyOrMaybeParser <*>
    optional (strOption (long "expiry" <> metavar "EXPIRY" <> help "Expiration time of a transaction, specified as a relative duration (\"30s\", \"5m\", etc.) or UNIX epoch timestamp.")) <*>
    interactionOptsParser

interactionOptsParser :: Parser InteractionOpts
interactionOptsParser =
  InteractionOpts <$>
    (not <$> switch (long "no-confirm" <> help "Do not ask for confirmation before proceeding to send the transaction.")) <*>
    (not <$> switch (long "no-wait" <> help "Exit right after sending the transaction without waiting for it to be committed and finalized."))

programOptions :: ShowAllOpts -> Parser Options
programOptions showAllOpts =
  Options <$>
    hsubparser
     (metavar "command" <>
      transactionCmds <>
      accountCmds <>
      moduleCmds <>
      contractCmds <>
      configCmds showAllOpts <>
      consensusCmds <>
      blockCmds <>
      bakerCmds <>
      identityCmds <>
      rawCmds
     ) <*>
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
           transactionWithScheduleCmd <>
           transactionDeployCredentialCmd <>
           transactionEncryptedTransferCmd <>
           transactionRegisterDataCmd))
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
       option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> help "Amount of GTUs to send.") <*>
       transactionOptsParser)
      (progDesc "Transfer GTU from one account to another."))

transactionWithScheduleCmd :: Mod CommandFields TransactionCmd
transactionWithScheduleCmd =
  command
   "send-gtu-scheduled"
   (info
     (let implicit = (\a b c d -> Left (a, b, c, d)) <$>
                     option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> help "Total amount of GTU to send.") <*>
                     option auto (long "every" <> metavar "INTERVAL" <> help "Interval between releases, one of 'Minute', 'Hour', 'Day', 'Week', 'Month' (30 days), or 'Year' (365 days). ") <*>
                     option auto (long "for" <> metavar "N" <> help "Number of releases.") <*>
                     option (eitherReader timeFromString) (long "starting" <> metavar "starting" <>
                                                           help "Start time of the first release, as a ISO8601 UTC time string, e.g., 2021-12-31T00:00:00Z.")
          explicit = Right <$>
                     option (eitherReader eitherParseScheduleInform)  (long "schedule" <> metavar "schedule" <> help "Explicit schedule in the form of a comma separated list of elements of the form '3.0 at 2020-12-13T23:35:59Z' (send 3 GTU on December 13, 2020). Timestamps must be given in UTC.")
       in
         TransactionSendWithSchedule <$>
         strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.") <*> (implicit <|> explicit) <*> transactionOptsParser)
     (progDescDoc . Just $ fillCat [
         "Transfer GTU from one account to another with the provided schedule of releases.",
         "Releases can be specified in one of two ways, either as regular releases via intervals," <>
         softline <> "or as an explicit schedule at specific timestamps.",
         "",
         "To specify the release via regular intervals use the options" <> softline <> "'--amount', '--every', '--for', and '--starting'.",
         "To specify an explicit schedule provide a list of releases in" <>
         softline <> "the form of a '--schedule' flag, which takes a comma separated list of releases.",
         "Each release must be of the form 100 at 2020-12-13T23:23:23Z.",
         "",
         "For example, to supply three releases, of 100, 150, and 200 GTU," <> softline <>
         "on January 1, 2021, February 15, 2021, and December 31, 2021," <> softline <>
         "the following input would be used. All releases are at the beginning of" <> softline <> "the day in the UTC time zone.",
         "",
         hang 4 "\"100 at 2021-01-01T00:00:00Z, 150 at 2021-02-15T00:00:00Z, 200 at 2021-12-31T00:00:00Z\"",
         "",
         "Times are parsed according to the ISO8601 standard for the UTC time zone."
         ]))
  where
    eitherParseScheduleInform :: String -> Either String [(Timestamp, Amount)]
    eitherParseScheduleInform s =
      let items = map Data.Text.words (splitOn "," (pack s))
      in forM items $ \case 
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

transactionEncryptedTransferCmd :: Mod CommandFields TransactionCmd
transactionEncryptedTransferCmd =
  command
    "send-gtu-encrypted"
    (info
      (TransactionEncryptedTransfer <$>
         transactionOptsParser <*>
         strOption (long "receiver" <> metavar "RECEIVER-ACCOUNT" <> help "Address of the receiver.") <*>
         option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> help "Amount of GTUs to send.") <*>
         optional (option auto (long "index" <> metavar "INDEX" <> help "Optionally specify the index up to which incoming encrypted amounts should be used.")))
      (progDesc "Transfer GTU from the encrypted balance of the account to the encrypted balance of another account."))

transactionRegisterDataCmd :: Mod CommandFields TransactionCmd
transactionRegisterDataCmd =
  command
    "register-data"
    (info
      (TransactionRegisterData <$>
        strArgument (metavar "FILE" <> help "File containing the data to register.") <*>
        transactionOptsParser)
      (progDesc "Register data on the chain."))

accountCmds :: Mod CommandFields Cmd
accountCmds =
  command
    "account"
    (info
      (AccountCmd <$>
        hsubparser
          (accountShowCmd <>
           accountListCmd <>
           accountUpdateKeysCmd <>
           accountEncryptCmd <>
           accountDecryptCmd))
      (progDesc "Commands for inspecting and modifying accounts."))

accountShowCmd :: Mod CommandFields AccountCmd
accountShowCmd =
  command
    "show"
    (info
       (AccountShow <$>
         optional (strArgument (metavar "ACCOUNT" <> help "Name or address of the account.")) <*>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")) <*>
         switch (long "encrypted" <> help "Show encrypted balance") <*>
         switch (long "decrypt-encrypted" <> help "Show the encrypted balance, but also decrypt all the amounts (potentially slow)."))
       (progDesc "Display account details."))

accountListCmd :: Mod CommandFields AccountCmd
accountListCmd =
  command
    "list"
    (info
       (AccountList <$>
         optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
       (progDesc "List all accounts."))

accountEncryptCmd :: Mod CommandFields AccountCmd
accountEncryptCmd =
  command
    "encrypt"
    (info
      (AccountEncrypt <$>
        transactionOptsParser <*>
        option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> help "The amount to transfer to encrypted balance."))
      (progDesc "Transfer an amount from public to encrypted balance of the account."))

accountDecryptCmd :: Mod CommandFields AccountCmd
accountDecryptCmd =
  command
    "decrypt"
    (info
      (AccountDecrypt <$>
        transactionOptsParser <*>
        option (maybeReader amountFromString) (long "amount" <> metavar "GTU-AMOUNT" <> help "The amount to transfer to public balance.") <*>
        optional (option auto (long "index" <> metavar "INDEX" <> help "Optionally specify the index up to which encrypted amounts should be combined.")))
      (progDesc "Transfer an amount from encrypted to public balance of the account."))

accountUpdateKeysCmd :: Mod CommandFields AccountCmd
accountUpdateKeysCmd =
  command
    "update-keys"
    (info
      (AccountUpdateKeys <$>
        strArgument (metavar "FILE" <> help "File containing the new account keys.") <*>
        option (eitherReader credIdFromStringInform) (long "credId" <> metavar "CRED-ID" <> help "The credential registration id of the credential whose keys we want to update.") <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Set keys for the credential. Expected format of the key file:"
        , "   {"
        , "     idx: {"
        , "       \"verifyKey\": ..."
        , "     },"
        , "     ..."
        , "   }"
        , "where idx is the key index associated to the corresponding verify key." ]))


moduleCmds :: Mod CommandFields Cmd
moduleCmds =
  command
    "module"
    (info
      (ModuleCmd <$>
        hsubparser
          (moduleDeployCmd <>
           moduleListCmd <>
           moduleShowCmd <>
           moduleInspectCmd <>
           moduleNameCmd))
      (progDesc "Commands for inspecting and deploying modules."))

moduleDeployCmd :: Mod CommandFields ModuleCmd
moduleDeployCmd =
  command
    "deploy"
    (info
      (ModuleDeploy <$>
        strArgument (metavar "FILE" <> help "Path to the smart contract module.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name for the module.")) <*>
        transactionOptsParser)
      (progDesc "Deploy a smart contract module on the chain, optionally naming the module."))

moduleListCmd :: Mod CommandFields ModuleCmd
moduleListCmd =
  command
    "list"
    (info
      (ModuleList <$>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "List all modules."))

moduleShowCmd :: Mod CommandFields ModuleCmd
moduleShowCmd =
  command
    "show"
    (info
      (ModuleShow <$>
        strArgument (metavar "MODULE-OR-NAME" <> help "Module reference OR a module name.") <*>
        strOption (long "out" <> metavar "FILE" <> help "File to output the source code to (use '-' for stdout).") <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Get the source code for a module."))

moduleInspectCmd :: Mod CommandFields ModuleCmd
moduleInspectCmd =
  command
    "inspect"
    (info
      (ModuleInspect <$>
        strArgument (metavar "MODULE-OR-NAME" <> help "Module reference OR a module name.") <*>
        optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to display the type signatures of the functions.")) <*>
        optional (strOption (long "block" <> metavar "BLOCK" <> help "Hash of the block (default: \"best\").")))
      (progDesc "Show the functions from a module, including type signatures if a schema is provided."))

moduleNameCmd :: Mod CommandFields ModuleCmd
moduleNameCmd =
  command
    "name"
    (info
      (ModuleName <$>
        strArgument (metavar "MODULE" <> help "Module reference OR path to the module.") <*>
        strOption (long "name" <> metavar "NAME" <> help "Name for the module."))
      (progDesc "Name a module."))

contractCmds :: Mod CommandFields Cmd
contractCmds =
  command
    "contract"
    (info
      (ContractCmd <$>
        hsubparser
          (contractShowCmd <>
           contractListCmd <>
           contractInitCmd <>
           contractUpdateCmd <>
           contractNameCmd))
      (progDesc "Commands for inspecting and initializing smart contracts."))

contractShowCmd :: Mod CommandFields ContractCmd
contractShowCmd =
  command
    "show"
    (info
      (ContractShow <$>
        strArgument (metavar "INDEX-OR-NAME" <> help "Index of the contract address OR a contract name.") <*>
        optional (option auto (long "subindex" <> metavar "SUBINDEX"
                            <> help "Subindex of address for the contract (default: 0)")) <*>
        optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to display the contract info.")) <*>
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
        strArgument (metavar "MODULE" <> help "Module reference OR module name OR (if --path is used) path to a module.") <*>
        strOption (long "contract" <> metavar "CONTRACT-NAME"
                             <> help "Name of the contract (i.e. init function) in the module.") <*>
        optional (strOption (long "parameter-json" <> metavar "FILE"
                             <> help "JSON file with parameters for init function. This parameter format should be used if a schema is supplied (default: no parameters).")) <*>
        optional (strOption (long "parameter-bin" <> metavar "FILE"
                             <> help "Binary file with parameters for init function. This should _not_ be used if a schema is supplied (default: no parameters).")) <*>
        optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to parse the params file.")) <*>
        optional (strOption (long "name" <> metavar "NAME" <> help "Name for the contract.")) <*>
        switch (long "path" <> help "Use when MODULE is a path to a module file.") <*>
        option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> value 0
                                                      <> help "Amount of GTU to transfer to the contract.") <*>
        requiredEnergyTransactionOptsParser)
      (progDesc "Initialize contract from already deployed module, optionally naming the contract."))

contractUpdateCmd :: Mod CommandFields ContractCmd
contractUpdateCmd =
  command
    "update"
    (info
      (ContractUpdate <$>
        strArgument (metavar "INDEX-OR-NAME" <> help "Index of the contract address OR a contract name.") <*>
        optional (option auto (long "subindex" <> metavar "SUBINDEX" <>
                     help "Subindex of address for the contract on chain (default: 0)")) <*>
        strOption (long "func" <> metavar "RECEIVE-NAME"
                             <> help "Name of the specific receive function in the module.") <*>
        optional (strOption (long "parameter-json" <> metavar "FILE"
                             <> help "JSON file with parameters for receive function. This parameter format should be used if a schema is supplied (default: no parameters).")) <*>
        optional (strOption (long "parameter-bin" <> metavar "FILE"
                             <> help "Binary file with parameters for receive function. This should _not_ be used if a schema is supplied (default: no parameters).")) <*>
        optional (strOption (long "schema" <> metavar "SCHEMA" <> help "Path to a schema file, used to parse the params file.")) <*>
        option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> value 0
                                                                <> help "Amount of GTU to transfer to the contract.") <*>
        requiredEnergyTransactionOptsParser)
      (progDesc "Update an existing contract."))

contractNameCmd :: Mod CommandFields ContractCmd
contractNameCmd =
  command
    "name"
    (info
      (ContractName <$>
        argument auto (metavar "INDEX" <> help "Index of the contract address to be named.") <*>
        optional (option auto (long "subindex" <> metavar "SUBINDEX"
                            <> help "Subindex of contract address to be named (default: 0)")) <*>
        strOption (long "name" <> metavar "NAME" <> help "Name for the contract."))
      (progDesc "Name a contract."))

configCmds :: ShowAllOpts -> Mod CommandFields Cmd
configCmds showAllOpts =
  command
    "config"
    (info
      (ConfigCmd <$>
        hsubparser
          (configInitCmd <>
           configShowCmd <>
           configBackupExportCmd <>
           configBackupImportCmd <>
           configAccountCmds showAllOpts))
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

configBackupExportCmd :: Mod CommandFields ConfigCmd
configBackupExportCmd = 
  command
    "export-backup"
    (info
      (ConfigBackupExport <$>
        strArgument (metavar "FILE" <> help "Name for backup file")
      )
      (progDesc "Save config to backup file, optionally encrypted with a password"))


configBackupImportCmd :: Mod CommandFields ConfigCmd
configBackupImportCmd = 
  command
    "import-backup"
    (info
      (ConfigBackupImport <$>
        strArgument (metavar "FILE" <> help "Backup file name") <*>
        switch (long "skip-existing" <> short 's' <> help "Automatically skip importing accounts when the keydirectory already exists")
      )
      (progDesc "Import config backup file, requires password if encrypted"))

configAccountCmds :: ShowAllOpts -> Mod CommandFields ConfigCmd
configAccountCmds showAllOpts =
  command
    "account"
    (info
      (ConfigAccountCmd <$>
        hsubparser
          (configAccountNameCmd <>
           configAccountRemove <>
           configAccountImportCmd showAllOpts <>
           configAccountAddKeysCmd <>
           configAccountUpdateKeysCmd <>
           configAccountRemoveKeysCmd <>
           configAccountRemoveNameCmd
           ))
      (progDesc "Commands for inspecting and changing account-specific configuration."))

configAccountNameCmd :: Mod CommandFields ConfigAccountCmd
configAccountNameCmd =
  command
    "name"
    (info
      (ConfigAccountName <$>
        strArgument (metavar "ADDRESS" <> help "Address of the account.") <*>
        strOption (long "name" <> metavar "NAME" <> help "Name of the account."))
      (progDesc "Adds a named account address to persistent config"))

configAccountRemove :: Mod CommandFields ConfigAccountCmd
configAccountRemove =
  command
    "remove"
    (info
      (ConfigAccountRemove <$>
        strArgument (metavar "ACCOUNT" <> help "Name or address of the account."))
      (progDesc "Remove the account from the persistent config."))

configAccountImportCmd :: ShowAllOpts -> Mod CommandFields ConfigAccountCmd
configAccountImportCmd showAllOpts =
  command
    "import"
    (info
      (ConfigAccountImport <$>
        strArgument (metavar "FILE" <> help "Account file exported from the wallet. By default all accounts will be imported.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help nameOptionHelp)) <*>
        option readAccountExportFormat (internalUnless showAllOpts <> long "format" <> metavar "FORMAT" <> value FormatMobile <> help "Export format. Supported values are 'mobile' and 'genesis' (default: 'mobile').") <*>
        switch (long "skip-existing" <> short 's' <> help "Automatically skip importing accounts when the keydirectory already exists")
      )
      (progDesc "Import an account to persistent config."))
  where nameOptionHelp =
          if showAllOpts then
             "Name of the account. For the 'genesis' format, this sets the name to assign to the account. For the 'mobile' format (which contains multiple already named accounts), it selects which account to import."
          else
             "Name of a single account in the wallet. This will cause the client to only import that account."

configAccountAddKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountAddKeysCmd =
  command
    "add-keys"
    (info
      (ConfigAccountAddKeys <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file."))
      (progDescDoc $ docFromLines $
       [ "Add one or several key pairs to a specific account configuration."
       , "Expected format of the key file:"
       ] ++ expectedAddOrUpdateKeysFileFormat))

configAccountUpdateKeysCmd :: Mod CommandFields ConfigAccountCmd
configAccountUpdateKeysCmd =
  command
    "update-keys"
    (info
      (ConfigAccountUpdateKeys <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file."))

      (progDescDoc $ docFromLines $
       [ "Update one or several key pairs to a specific account configuration."
       , "Expected format of the key file:"
       ] ++ expectedAddOrUpdateKeysFileFormat))

expectedAddOrUpdateKeysFileFormat :: [String]
expectedAddOrUpdateKeysFileFormat =
  ["   {"
  , "     idx: {"
  , "       \"encryptedSignKey\": {"
  , "         \"metadata\": {"
  , "           \"encryptionMethod\": \"AES-256\","
  , "           \"iterations\": ...,"
  , "           \"salt\": ...,"
  , "           \"initializationVector\": ...,"
  , "           \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\""
  , "         },"
  , "         \"cipherText\": ..."
  , "       },"
  , "       \"verifyKey\": ...,"
  , "       \"schemeId\": \"Ed25519\""
  , "     },"
  , "     ..."
  , "   }"
  , "where idx is the index of the respective key pair."
  ]

configAccountRemoveKeysCmd :: Mod CommandFields ConfigAccountCmd 
configAccountRemoveKeysCmd =
  command
    "remove-keys"
    (info
      (ConfigAccountRemoveKeys <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        option (eitherReader credentialIndexFromStringInform) (long "credential-index" <> metavar "CREDENTIALINDEX" <> help "Index of the credential containing the keys to remove") <*>
        some (argument auto (metavar "KEYINDICES" <> help "space-separated list of indices of the keys to remove."))
        )
      (progDescDoc $ docFromLines
        [ "Removes the keys from the account at the specified indices.",
          "The --threshold option may be used to update the signature threshold." ]))

readAccountExportFormat :: ReadM AccountExportFormat
readAccountExportFormat = str >>= \case
  "mobile" -> return FormatMobile
  "genesis" -> return FormatGenesis
  s -> readerError $ printf "invalid format: %s (supported values: 'mobile' and 'genesis')" (s :: String)

configAccountRemoveNameCmd :: Mod CommandFields ConfigAccountCmd
configAccountRemoveNameCmd =
  command
    "remove-name"
    (info
      (ConfigAccountRemoveName <$>
        strArgument (metavar "NAME" <> help "Name of the account"))
    (progDescDoc $ docFromLines
      [ "Removes the given name from the list of named accounts" ]))

consensusCmds :: Mod CommandFields Cmd
consensusCmds =
  command
    "consensus"
    (info
      (ConsensusCmd <$>
        (hsubparser
          (consensusStatusCmd <>
           consensusShowParametersCmd) <|>
        hsubparser
          (commandGroup "internal" <> consensusChainUpdateCmd <> internal)))
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

consensusChainUpdateCmd :: Mod CommandFields ConsensusCmd
consensusChainUpdateCmd =
  command
    "chain-update"
    (info
      (ConsensusChainUpdate <$>
        strArgument (metavar "UPDATE" <> help "File containing the update command in JSON format.") <*>
        some (strOption (long "key" <> metavar "FILE" <> help "File containing key-pair to sign the update command. This option can be provided multiple times, once for each key-pair to use.")) <*>
        interactionOptsParser
        )
      (progDesc "Send a chain-update command to the chain."))

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
           bakerSetKeysCmd <>
           bakerUpdateRestakeCmd <>
           bakerUpdateStakeCmd))
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
        transactionOptsParser <*>
        option (eitherReader amountFromStringInform) (long "stake" <> metavar "GTU-AMOUNT" <> help "The amount of GTU to stake.") <*>
        (not <$> switch (long "no-restake" <> help "If supplied, the earnings will not be added to the baker stake automatically.")) <*>
        optional (strOption (long "out" <> metavar "FILE" <> help "File to write the baker credentials to, in case of succesful transaction. These can be used to start the node."))
      )
      (progDesc "Deploy baker credentials to the chain."))

bakerSetKeysCmd :: Mod CommandFields BakerCmd
bakerSetKeysCmd =
  command
    "set-key"
    (info
      (BakerSetKeys <$>
        strArgument (metavar "FILE" <> help "File containing the new public and private keys.") <*>
        transactionOptsParser <*>
        optional (strOption (long "out" <> metavar "FILE" <> help "File to write the baker credentials to, in case of succesful transaction. These can be used to start the node.")))
      (progDescDoc $ docFromLines
        [ "Update the keys of a baker. Expected format of the key file:"
        , "   {"
        , "     \"signatureSignKey\": ...,"
        , "     \"signatureVerifyKey\": ..."
        , "     \"aggregationSignKey\": ...,"
        , "     \"aggregationVerifyKey\": ..."
        , "     \"electionPrivateKey\": ...,"
        , "     \"electionVerifyKey\": ...,"
        , "   }" ]))

bakerRemoveCmd :: Mod CommandFields BakerCmd
bakerRemoveCmd =
  command
    "remove"
    (info
      (BakerRemove <$>
        transactionOptsParser)
      (progDesc "Remove a baker from the chain."))

bakerUpdateRestakeCmd :: Mod CommandFields BakerCmd
bakerUpdateRestakeCmd =
  command
   "update-restake"
   (info
     (BakerUpdateRestakeEarnings <$>
       argument auto (metavar "UPDATE" <> help "True|False") <*>
       transactionOptsParser)
     (progDesc "Change whether to restake the earnings automatically or not"))

bakerUpdateStakeCmd :: Mod CommandFields BakerCmd
bakerUpdateStakeCmd =
  command
   "update-stake"
   (info
     (BakerUpdateStake <$>
       option (eitherReader amountFromStringInform) (long "stake" <> metavar "GTU-AMOUNT" <> help "The amount of GTU to stake.") <*>
       transactionOptsParser)
     (progDesc "Update the amount staked for the baker."))

identityCmds :: Mod CommandFields Cmd
identityCmds =
  command
    "identity"
    (info
      (IdentityCmd <$>
        hsubparser
          identityShowCmd)
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
