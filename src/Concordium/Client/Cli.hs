{-# LANGUAGE DataKinds #-}

module Concordium.Client.Cli where

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.ID.Types as ID
import qualified Concordium.ID.Types as IDTypes
import Concordium.Types
import Concordium.Utils.Encryption (Password (..))

import Concordium.Client.Parse
import Concordium.Client.Types.Account
import Concordium.Client.Types.TransactionStatus

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Aeson as AE
import Data.Aeson.Types (Pair)
import qualified Data.Char as C
import Data.List (uncons)
import qualified Data.Map.Strict as OrdMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import System.Console.ANSI
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.PrettyPrint
import Text.Printf
import Prelude hiding (fail, log)

data Level = Info | Warn | Err deriving (Eq)

{- |Log a list of sentences. The sentences are pretty printed (capital first letter and dot at the end),
 so the input messages should only contain capital letters for names and have no dot suffix.
 Sentences will be joined on the same line as long as the resulting line doesn't exceed 90 chars.
 Depending on the log level, an appropriate prefix is added to the first line.
 All lines will be indented such that they align with the first line
 (i.e. as if they had all been prefixed).
-}
log :: MonadIO m => Level -> Maybe Color -> [String] -> m ()
log lvl color msgs = do
    let doc = prefix <+> fsep (expandLines $ map (prettyMsg ".") msgs)
        out = logStrLn $ renderStyle s doc
    case color of
        Nothing -> out
        Just c -> do
            liftIO $ setSGR [SetColor Foreground Vivid c]
            out
            liftIO $ setSGR [Reset]
  where
    s = Style{mode = PageMode, lineLength = 90, ribbonsPerLine = 1.0}
    prefix = case lvl of
        Info -> empty
        Warn -> text "Warning:"
        Err -> text "Error:"

logSuccess :: MonadIO m => [String] -> m ()
logSuccess = log Info $ Just Green

logInfo :: MonadIO m => [String] -> m ()
logInfo = log Info Nothing

logWarn :: MonadIO m => [String] -> m ()
logWarn = log Warn $ Just Yellow

logError :: MonadIO m => [String] -> m ()
logError = log Err $ Just Red

logFatal :: MonadIO m => [String] -> m a
logFatal msgs = logError msgs >> liftIO exitFailure

withLogFatal :: MonadIO m => Either e' a -> (e' -> String) -> m a
withLogFatal (Left x) f = logFatal [f x]
withLogFatal (Right x) _ = return x

logFatalOnError :: MonadIO m => Either String a -> m a
logFatalOnError x = x `withLogFatal` id

withLogFatalIO :: IO (Either e' a) -> (e' -> String) -> IO a
withLogFatalIO action f = do
    v <- action
    v `withLogFatal` f

withLogFatalIO' :: String -> IO (Maybe a) -> IO a
withLogFatalIO' e action =
    action >>= \case
        Nothing -> logFatal [e]
        Just x -> return x

logExit :: MonadIO m => [String] -> m a
logExit msgs = logInfo msgs >> liftIO exitSuccess

-- |Expand each string into a document of the string's lines joined together using vcat.
expandLines :: [String] -> [Doc]
expandLines = map $ vcat . map text . lines

{- |Ensure that a string is printable as a sentence by converting the first letter to upper case
 and, unless it already ends with "standard" punctuation, appending the provided punctionation.
-}
prettyMsg :: String -> String -> String
prettyMsg punctuation = \case
    "" -> ""
    (x : xs) ->
        let s =
                if null xs || last xs `elem` p
                    then xs
                    else xs ++ punctuation
         in (C.toUpper x) : s
  where
    p = ".,:;?!{}" :: String

logStr :: MonadIO m => String -> m ()
logStr = liftIO . hPutStr stderr

logStrLn :: MonadIO m => String -> m ()
logStrLn = liftIO . hPutStrLn stderr

{- |Ask the user to "confirm" on stdin and return the result.
 Note that this only appends " [yN]: " if the prompt does not end in
 one of ".,:;?!{}"
-}
askConfirmation :: MonadIO m => Maybe String -> m Bool
askConfirmation prompt = liftIO $ do
    putStr $ prettyMsg " [yN]: " $ fromMaybe defaultPrompt prompt
    input <- T.getLine
    T.putStrLn input
    return $ T.strip (T.toLower input) == "y"
  where
    defaultPrompt = "confirm"

-- | Ask for a password on standard input not showing what is typed.
askPassword ::
    -- | A text to display after which the password is typed (nothing is appended to this).
    String ->
    IO Password
askPassword descr = do
    putStr descr
    -- Get the password from command line, not showing what is typed by temporarily disabling echo.
    passwordInput <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) T.getLine
    let password = T.encodeUtf8 passwordInput
    putStrLn ""
    return (Password password)

-- | Ask the user to create a new password, requiring to retype the password for confirmation.
createPasswordInteractive ::
    -- | An optional description of the password, will be displayed after "... a new password to ".
    Maybe String ->
    -- | The password entered by the user or an error message on failure.
    IO (Either String Password)
createPasswordInteractive descr = runExceptT $ do
    pwd <- liftIO $ askPassword $ "Please enter a new password" ++ maybe "" (" to " ++) descr ++ ": "
    pwd' <- liftIO $ askPassword "Please retype the password: "
    unless (pwd' == pwd) $ throwError "the passwords do not match"
    return pwd

{- | Decrypt the given encrypted account keys. For each key, this asks for the respective password
 presenting the key index to the user.
-}
decryptAccountKeyMapInteractive ::
    EncryptedAccountKeyMap ->
    Maybe (OrdMap.Map ID.CredentialIndex [ID.KeyIndex]) ->
    -- | Optional text describing the account of which to decrypt keys. Will be shown in the format
    -- "Enter password for %s signing key"
    Maybe String ->
    -- | The decrypted 'AccountKeyMap' or an error message on failure.
    IO (Either String AccountKeyMap)
decryptAccountKeyMapInteractive encryptedKeyMap indexmap accDescr = runExceptT $ do
    -- let accText = maybe " " (\s -> " " ++ s ++ " ") accDescr
    let queryText credIndex keyIndex =
            -- if Map.size encryptedKeyMap <= 1
            -- then "Enter password for" ++ accText ++ "signing key: "
            -- else
            case accDescr of
                Nothing -> "Enter password for credential with index " ++ show credIndex ++ " and signing key with index " ++ show keyIndex ++ ": "
                Just descr -> "Enter password for signing key of " ++ descr ++ " with index " ++ show keyIndex ++ ": "
    -- In order to request passwords only for `threshold` number of accounts, we will map over the sub-map of the wanted size
    let inputMap = case indexmap of
            Nothing -> encryptedKeyMap -- no map provided, use the full map
            Just im ->
                let filterCredentials = OrdMap.filterWithKey (\k _ -> OrdMap.member k im) encryptedKeyMap
                    lookUpKey cidx kidx =
                        case OrdMap.lookup cidx im of
                            Nothing -> False
                            Just keyIndexList -> kidx `elem` keyIndexList
                    newmap = OrdMap.mapWithKey (\credIndex m -> OrdMap.filterWithKey (\k _ -> lookUpKey credIndex k) m) filterCredentials
                 in newmap
    sequence $
        OrdMap.mapWithKey
            ( \credIndex eKpMap ->
                ( sequence
                    . OrdMap.mapWithKey
                        ( \keyIndex eKp -> do
                            pwd <- liftIO $ askPassword $ queryText credIndex keyIndex
                            decryptAccountKeyPair pwd keyIndex eKp
                        )
                )
                    eKpMap
            )
            inputMap

decryptAccountEncryptionSecretKeyInteractive ::
    EncryptedAccountEncryptionSecretKey ->
    IO (Either String ElgamalSecretKey)
decryptAccountEncryptionSecretKeyInteractive secret = do
    pwd <- liftIO $ askPassword $ "Enter password for decrypting the secret encryption key: "
    decryptAccountEncryptionSecretKey pwd secret

-- |Standardized method of exiting the command because the transaction is cancelled.
exitTransactionCancelled :: MonadIO m => m a
exitTransactionCancelled = liftIO $ logExit ["transaction cancelled"]

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
    tz <- getCurrentTimeZone
    localTimeOfDay . utcToLocalTime tz <$> getCurrentTime

getCurrentTimeUnix :: IO TransactionExpiryTime
getCurrentTimeUnix = TransactionTime . round <$> getPOSIXTime

timeFromTransactionExpiryTime :: TransactionExpiryTime -> UTCTime
timeFromTransactionExpiryTime = posixSecondsToUTCTime . fromIntegral . ttsSeconds

----------------------------------------------------------------------------------------------------

data BirkParametersResult = BirkParametersResult
    { bprElectionNonce :: LeadershipElectionNonce
    , -- , bprElectionDifficulty :: ElectionDifficulty
      bprBakers :: [BirkParametersBakerResult]
    }

instance AE.FromJSON BirkParametersResult where
    parseJSON = withObject "Birk parameters" $ \v -> do
        bprElectionNonce <- v .: "electionNonce"
        -- bprElectionDifficulty <- v .: "electionDifficulty"
        bprBakers <- v .: "bakers"
        return $ BirkParametersResult{..}

data BirkParametersBakerResult = BirkParametersBakerResult
    { bpbrId :: BakerId
    , bpbrLotteryPower :: Double
    , bpbrAccount :: IDTypes.AccountAddress
    }

instance AE.FromJSON BirkParametersBakerResult where
    parseJSON = withObject "Baker" $ \v -> do
        bpbrId <- v .: "bakerId"
        bpbrLotteryPower <- v .: "bakerLotteryPower"
        bpbrAccount <- v .: "bakerAccount"
        return $ BirkParametersBakerResult{..}

data BakerKeys = BakerKeys
    { bkSigSignKey :: BlockSig.SignKey
    , bkSigVerifyKey :: BlockSig.VerifyKey
    , bkAggrSignKey :: Bls.SecretKey
    , bkAggrVerifyKey :: Bls.PublicKey
    , bkElectionSignKey :: VRF.SecretKey
    , bkElectionVerifyKey :: VRF.PublicKey
    , bkBakerId :: Maybe BakerId
    -- ^The id of the baker these keys belong to, if known.
    }

data BakerCredentials = BakerCredentials
    { bcKeys :: !BakerKeys
    , bcIdentity :: !BakerId
    }

instance AE.ToJSON BakerCredentials where
    toJSON BakerCredentials{..} = object (("bakerId" .= bcIdentity) : bakerKeysToPairs bcKeys)

instance AE.FromJSON BakerKeys where
    parseJSON = withObject "Baker keys" $ \v -> do
        bkAggrSignKey <- v .: "aggregationSignKey"
        bkAggrVerifyKey <- v .: "aggregationVerifyKey"
        bkElectionSignKey <- v .: "electionPrivateKey"
        bkElectionVerifyKey <- v .: "electionVerifyKey"
        bkSigSignKey <- v .: "signatureSignKey"
        bkSigVerifyKey <- v .: "signatureVerifyKey"
        bkBakerId <- v .:? "bakerId"
        return BakerKeys{..}

bakerKeysToPairs :: BakerKeys -> [Pair]
bakerKeysToPairs v =
    [ "aggregationSignKey" .= bkAggrSignKey v
    , "aggregationVerifyKey" .= bkAggrVerifyKey v
    , "electionPrivateKey" .= bkElectionSignKey v
    , "electionVerifyKey" .= bkElectionVerifyKey v
    , "signatureSignKey" .= bkSigSignKey v
    , "signatureVerifyKey" .= bkSigVerifyKey v
    ]
        ++ ["bakerId" .= bid | bid <- maybeToList (bkBakerId v)]

instance AE.ToJSON BakerKeys where
    toJSON = object . bakerKeysToPairs

-- Helper function for generating JSON containing only the public parts of the baker keys
bakerPublicKeysToPairs :: BakerKeys -> [Pair]
bakerPublicKeysToPairs v =
    [ "aggregationVerifyKey" .= bkAggrVerifyKey v
    , "electionVerifyKey" .= bkElectionVerifyKey v
    , "signatureVerifyKey" .= bkSigVerifyKey v
    ]
        ++ ["bakerId" .= bid | bid <- maybeToList (bkBakerId v)]

-- |Hardcoded network ID.
defaultNetId :: Int
defaultNetId = 100

{- |If the string starts with @ we assume the remaining characters are a file name
 and we try to read the contents of that file.
-}
decodeJsonArg :: FromJSON a => String -> Maybe (IO (Either String a))
decodeJsonArg s =
    Just $ do
        res <- case uncons s of
            Just ('@', rest) ->
                AE.eitherDecodeFileStrict rest
            _ -> return $ AE.eitherDecodeStrict $ T.encodeUtf8 $ T.pack s
        return $ case res of
            Left err -> Left $ printf "cannot parse '%s' as JSON: %s" s err
            Right r -> Right r

getExpiryArg :: String -> TransactionExpiryTime -> Maybe Text -> IO TransactionExpiryTime
getExpiryArg name now input =
    case input of
        Nothing -> return $ now + defaultExpiryDurationSecs
        Just v -> case parseExpiry now v of
            Left err -> logFatal [printf "invalid %s '%s': %s" name v err]
            Right e -> return e
  where
    defaultExpiryDurationSecs = 600 -- 10 mins

class (Monad m) => TransactionStatusQuery m where
    queryTransactionStatus :: TransactionHash -> m TransactionStatusResult
    wait :: Int -> m ()
