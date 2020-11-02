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
import Data.Word (Word64)
import Network.HTTP2.Client
import Options.Applicative
import Paths_simple_client (version)
import Concordium.Client.LegacyCommands
import Concordium.Client.Types.Account
import Concordium.Client.Utils
import Concordium.ID.Types (KeyIndex, SignatureThreshold)
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
    , caiFormat :: !AccountExportFormat }
  | ConfigAccountAddKeys
    { caakAddr :: !Text
    , caakKeysFile :: !FilePath }
  | ConfigAccountUpdateKeys
    { caukAddr :: !Text
    , caukKeysFile :: !FilePath }
  | ConfigAccountRemoveKeys
    { carkAddr :: !Text
    , carkKeys :: ![KeyIndex]
    , carkThreshold :: !(Maybe SignatureThreshold) }
  | ConfigAccountSetThreshold
    { cuatAddr :: !Text
    , cuatThreshold :: !SignatureThreshold }
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
    , tsgOpts :: !(TransactionOpts (Maybe Energy)) }
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
  deriving (Show)

data AccountCmd
  = AccountShow
    { asAddress :: !(Maybe Text)
    , asBlockHash :: !(Maybe Text)
    , asShowEncryptedBalance :: !Bool
    , asDecryptEncryptedBalance :: !Bool }
  | AccountList
    { alBlockHash :: !(Maybe Text) }
  | AccountDelegate
    { adBakerId :: !BakerId
    , adTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | AccountUndelegate
    { auTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | AccountUpdateKeys
    { aukKeys :: !FilePath
    , aukTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | AccountAddKeys
    { aakKeys :: !FilePath
    , aakThreshold :: !(Maybe SignatureThreshold)
    , aakTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | AccountRemoveKeys
    { arkKeys :: ![KeyIndex]
    , arkThreshold :: !(Maybe SignatureThreshold)
    , arkTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
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
      -- |Options for transaction.
    , mdTransactionOpts :: !(TransactionOpts (Maybe Energy))
    }
  -- |List all modules.
  | ModuleList
    { -- |Hash of the block (default "best").
      mlBlockHash :: !(Maybe Text)
    }
  -- |Output the binary source code of the module to the provided file.
  | ModuleShow
    { -- |Reference to the module.
      msModuleReference :: !Text
      -- |Output the module to this file.
      -- Use '-' to output to stdout.
    , msOutFile :: !FilePath
      -- |Hash of the block (default "best").
    , mlBlockHash :: !(Maybe Text) }
  deriving (Show)

data ContractCmd
  -- |Show the state of specified contract.
  = ContractShow
    { -- |Index of the address for the contract.
      cuAddressIndex :: !Word64
      -- |Subindex of the address for the contract (default: 0).
    , cuAddressSubindex :: !Word64
      -- |Hash of the block (default "best").
    , csBlockHash :: !(Maybe Text) }
  -- |List all contracts on chain.
  | ContractList
    { -- |Hash of the block (default "best").
      clBlockHash :: !(Maybe Text)
    }
  -- |Initialize a contract from a module on chain.
  | ContractInit
    { -- |Module reference OR path to the module (reference then calculated by hashing).
      ciModule :: !String
      -- |Name of the init function to use (default: "init").
    , ciInitName :: !Text
      -- |Path to a binary file containing parameters for the init function.
    , ciParameterFile :: !(Maybe FilePath)
      -- |Amount to be send to contract (default: 0).
    , ciAmount :: !Amount
      -- |Options for transaction.
    , ciTransactionOpts :: !(TransactionOpts Energy) }
  -- |Update an existing contract, i.e. invoke a receive function.
  | ContractUpdate
    { -- |Index of the address for the contract to invoke.
      cuAddressIndex :: !Word64
      -- |Subindex of the address for the contract to invoke (default: 0).
    , cuAddressSubindex :: !Word64
      -- |Name of the receive function to use (default: "receive").
    , cuReceiveName :: !Text
      -- |Path to a binary file containing paramaters for the receive method.
    , cuParameterFile :: !(Maybe FilePath)
      -- |Amount to invoke the receive function with (default: 0).
    , cuAmount :: !Amount
      -- |Options for transaction.
    , cuTransactionOpts :: !(TransactionOpts Energy) }
  deriving (Show)

-- | The type parameter 'energyOrMaybe' should be Energy or Maybe Energy.
data TransactionOpts energyOrMaybe =
  TransactionOpts
  { toSender :: !(Maybe Text)
  , toKeys :: !(Maybe FilePath)
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
    , ccuAuthorizations :: !FilePath
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
    , baTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerSetAccount
    { bsaBakerId :: !BakerId
    , bsaAccountRef :: !Text
    , bsaTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerSetKey
    { buskBakerId :: !BakerId
    , bsaSignatureKeysFile :: !FilePath
    , buskTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerRemove
    { brBakerId :: !BakerId
    , brTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerSetAggregationKey
    { bsakBakerId :: !BakerId
    , bsakBakerAggregationKeyFile :: !FilePath
    , bsakTransactionOpts :: !(TransactionOpts (Maybe Energy)) }
  | BakerSetElectionKey
    { bsekBakerId :: !BakerId
    , bsekBakerElectionKeyFile :: !FilePath
    , bsakTransactionOps :: !(TransactionOpts (Maybe Energy)) }
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
    optional (option auto (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction."))

-- |Parse transactionOpts with a required energy flag
requiredEnergyTransactionOptsParser :: Parser (TransactionOpts Energy)
requiredEnergyTransactionOptsParser = transactionOptsParserBuilder $
    option auto (long "energy" <> metavar "MAX-ENERGY" <> help "Maximum allowed amount of energy to spend on transaction.")

-- |Helper function to build an transactionOptsParser with or without a required energy flag
transactionOptsParserBuilder :: Parser energyOrMaybe -> Parser (TransactionOpts energyOrMaybe)
transactionOptsParserBuilder energyOrMaybeParser =
  TransactionOpts <$>
    optional (strOption (long "sender" <> metavar "SENDER" <> help "Name or address of the transaction sender.")) <*>
    -- TODO Specify / refer to format of JSON file when new commands (e.g. account add-keys) that accept same format are
    -- added.
    optional (strOption (long "keys" <> metavar "KEYS" <> help "Any number of sign/verify keys specified in a JSON file.")) <*>
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
    (hsubparser
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
           transactionDeployCredentialCmd <>
           transactionEncryptedTransferCmd))
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
      (progDesc "Transfer GTU from one account to another (sending to contracts is currently not supported with this method - use 'transaction submit')."))

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
           accountUndelegateCmd <>
           accountUpdateKeysCmd <>
           accountAddKeysCmd <>
           accountRemoveKeysCmd <>
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
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Update one or several keys of an account. Expected format of the key file:"
        , "   {"
        , "     idx: {"
        , "       \"verifyKey\": ..."
        , "     },"
        , "     ..."
        , "   }"
        , "where idx is the key index associated to the corresponding verify key." ]))

accountAddKeysCmd :: Mod CommandFields AccountCmd
accountAddKeysCmd =
  command
    "add-keys"
    (info
      (AccountAddKeys <$>
        strArgument (metavar "FILE" <> help "File containing the account keys.") <*>
        optional (option (eitherReader thresholdFromStringInform) (long "threshold" <> metavar "THRESHOLD" <>
            help "Update the signature threshold to this value. If not set, no changes are made to the threshold.")) <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Adds one or several additional keys to an account. Expected format of the key file:"
        , "   {"
        , "     idx: {"
        , "       \"verifyKey\": ..."
        , "     },"
        , "     ..."
        , "   }"
        , "where idx is the key index associated to the corresponding verify key. The --threshold option may be used to update the signature threshold." ]))

accountRemoveKeysCmd :: Mod CommandFields AccountCmd
accountRemoveKeysCmd =
  command
    "remove-keys"
    (info
      (AccountRemoveKeys <$>
        some (argument auto (metavar "KEYINDICES" <> help "space-separated list of indices of the keys to remove.")) <*>
        optional (option (eitherReader thresholdFromStringInform) (long "threshold" <> metavar "THRESHOLD" <>
            help "Update the signature threshold to this value. If not set, no changes are made to the threshold.")) <*>
        transactionOptsParser)
      (progDescDoc $ docFromLines
        [ "Removes the keys from the account at the specified indices. The --threshold option may be used to update the signature threshold." ]))

moduleCmds :: Mod CommandFields Cmd
moduleCmds =
  command
    "module"
    (info
      (ModuleCmd <$>
        hsubparser
          (moduleDeployCmd <>
           moduleListCmd <>
           moduleShowCmd))
      (progDesc "Commands for inspecting and deploying modules."))

moduleDeployCmd :: Mod CommandFields ModuleCmd
moduleDeployCmd =
  command
    "deploy"
    (info
      (ModuleDeploy <$>
        strArgument (metavar "FILE" <> help "Path to the smart contract module.") <*>
        transactionOptsParser)
      (progDesc "Deploy a smart contract module on the chain."))

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
        strArgument (metavar "MODULE-REFERENCE" <> help "Reference to the module (use 'module list' to find it).") <*>
        strOption (long "out" <> metavar "FILE" <> help "File to output the source code to (use '-' for stdout).") <*>
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
           contractInitCmd <>
           contractUpdateCmd))
      (progDesc "Commands for inspecting and initializing smart contracts."))

contractShowCmd :: Mod CommandFields ContractCmd
contractShowCmd =
  command
    "show"
    (info
      (ContractShow <$>
        argument auto (metavar "INDEX" <> help "Index of address for the contract on chain.") <*>
        option auto (long "subindex" <> metavar "SUBINDEX" <> value 0
                            <> help "Subindex of address for the contract on chain (default: 0)") <*>
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
        strArgument (metavar "MODULE" <> help "Module reference OR path to the module.") <*>
        strOption (long "func" <> metavar "INIT-NAME" <> value "init"
                             <> help "Name of the specific init function in the module (default: \"init\").") <*>
        optional (strOption (long "params" <> metavar "FILE"
                             <> help "Binary file with parameters for init function (default: no parameters).")) <*>
        option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> value 0
                                                                <> help "Amount of GTU to transfer to the contract.") <*>
        requiredEnergyTransactionOptsParser)
      (progDesc "Initialize contract from already deployed module."))

contractUpdateCmd :: Mod CommandFields ContractCmd
contractUpdateCmd =
  command
    "update"
    (info
      (ContractUpdate <$>
        argument auto (metavar "INDEX" <> help "Index of address for the contract on chain.") <*>
        option auto (long "subindex" <> metavar "SUBINDEX" <> value 0 <>
                     help "Subindex of address for the contract on chain (default: 0)") <*>
        strOption (long "func" <> metavar "RECEIVE-NAME" <> value "receive"
                             <> help "Name of the specific receive function in the module (default: \"receive\").") <*>
        optional (strOption (long "params" <> metavar "FILE"
                             <> help "Binary file with parameters for init function (default: no parameters).")) <*>
        option (eitherReader amountFromStringInform) (long "amount" <> metavar "GTU-AMOUNT" <> value 0
                                                                <> help "Amount of GTU to transfer to the contract.") <*>
        requiredEnergyTransactionOptsParser)
      (progDesc "Update an existing contract."))

configCmds :: ShowAllOpts -> Mod CommandFields Cmd
configCmds showAllOpts =
  command
    "config"
    (info
      (ConfigCmd <$>
        hsubparser
          (configInitCmd <>
           configShowCmd <>
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

configAccountCmds :: ShowAllOpts -> Mod CommandFields ConfigCmd
configAccountCmds showAllOpts =
  command
    "account"
    (info
      (ConfigAccountCmd <$>
        hsubparser
          (configAccountAddCmd <>
           configAccountImportCmd showAllOpts <>
           configAccountAddKeysCmd <>
           configAccountUpdateKeysCmd <>
           configAccountSetThresholdCmd <>
           configAccountRemoveKeysCmd))
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

configAccountImportCmd :: ShowAllOpts -> Mod CommandFields ConfigAccountCmd
configAccountImportCmd showAllOpts =
  command
    "import"
    (info
      (ConfigAccountImport <$>
        strArgument (metavar "FILE" <> help "Account file exported from the wallet.") <*>
        optional (strOption (long "name" <> metavar "NAME" <> help nameOptionHelp)) <*>
        option readAccountExportFormat (internalUnless showAllOpts <> long "format" <> metavar "FORMAT" <> value FormatMobile <> help "Export format. Supported values are 'mobile' and 'genesis' (default: 'mobile').")
      )
      (progDesc "Import an account to persistent config."))
  where nameOptionHelp =
          if showAllOpts then
             "Name of the account. For the 'genesis' format, this sets the name to assign to the account. For the 'mobile' format (which contains multiple already named accounts), it selects which account to import."
          else
             "Name of the account."

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
        some (argument auto (metavar "KEYINDICES" <> help "space-separated list of indices of the keys to remove.")) <*>
        optional (option (eitherReader thresholdFromStringInform) (long "threshold" <> metavar "THRESHOLD" <>
            help "Update the signature threshold to this value. If not set, no changes are made to the threshold.")))
      (progDescDoc $ docFromLines
        [ "Removes the keys from the account at the specified indices. The --threshold option may be used to update the signature threshold." ]))

configAccountSetThresholdCmd :: Mod CommandFields ConfigAccountCmd
configAccountSetThresholdCmd =
  command
    "set-threshold"
    (info
      (ConfigAccountSetThreshold <$>
        strOption (long "account" <> metavar "ACCOUNT" <> help "Name or address of the account.") <*>
        (option (eitherReader thresholdFromStringInform) (long "threshold" <> metavar "THRESHOLD" <>
            help "Sets the signature threshold to this value.")))
      (progDescDoc $ docFromLines
        [ "Sets the signature threshold of the account to the specified value."]))

readAccountExportFormat :: ReadM AccountExportFormat
readAccountExportFormat = str >>= \case
  "mobile" -> return FormatMobile
  "genesis" -> return FormatGenesis
  s -> readerError $ printf "invalid format: %s (supported values: 'mobile' and 'genesis')" (s :: String)

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
        strOption (long "authorizations" <> metavar "FILE" <> help "File containing the public update authorizations.") <*>
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
        [ "Update the signature keys of a baker. Expected format of the key file:"
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
        [ "Update the aggregation key of a baker. Expected format of the key file:"
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
        [ "Update the election key of a baker. Expected format of the key file:"
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
