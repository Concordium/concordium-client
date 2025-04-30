{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Part of the implementation of the GRPC API node API V2. This module
--  contains client logic and API wrappers for the services exposed, and
--  a typeclass @FromProto@ with accompanying instances. The latter are
--  used to convert Protocol-buffer payloads into our "native" Haskell data-
--  type equivalents.
module Concordium.Client.GRPC2 where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Coerce
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.ProtoLens (defMessage)
import qualified Data.ProtoLens.Field
import Data.ProtoLens.Service.Types
import qualified Data.Ratio as Ratio
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform
import Network.GRPC.Client
import Network.GRPC.Client.Helpers hiding (Address)
import Network.GRPC.HTTP2.ProtoLens
import Network.GRPC.HTTP2.Types (GRPCStatusCode (DEADLINE_EXCEEDED, RESOURCE_EXHAUSTED))
import Network.HTTP2.Client (ClientError, ClientIO, ExceptT, HostName, PortNumber, TooMuchConcurrency, runExceptT)
import qualified Proto.V2.Concordium.ProtocolLevelTokens_Fields as PLTFields
import qualified Web.Cookie as Cookie

import Concordium.Client.Cli (TransactionStatusQuery (..), logFatal, wait)
import Concordium.Client.RWLock
import Concordium.Client.Runner.Helper
import Concordium.Client.Types.TransactionStatus (transactionStatusToTransactionStatusResult)
import Concordium.Common.Time
import Concordium.Common.Version
import Concordium.Crypto.EncryptedTransfers
import Concordium.Crypto.SHA256 (Hash)
import Concordium.Crypto.SignatureScheme (Signature (..), VerifyKey (..))
import Concordium.GRPC2 hiding (Output)
import Concordium.ID.AnonymityRevoker (createArInfo)
import qualified Concordium.ID.AnonymityRevoker as ArInfo
import Concordium.ID.IdentityProvider (createIpInfo)
import qualified Concordium.ID.IdentityProvider as IpInfo
import Concordium.ID.Parameters (createGlobalContext)
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Accounts
import qualified Concordium.Types.Accounts as Concordium.Types
import Concordium.Types.Accounts.Releases
import Concordium.Types.Block (AbsoluteBlockHeight (..))
import Concordium.Types.Execution
import qualified Concordium.Types.InvokeContract as InvokeContract
import qualified Concordium.Types.Parameters as Parameters
import Concordium.Types.Queries
import qualified Concordium.Types.Queries.KonsensusV1 as KonsensusV1
import qualified Concordium.Types.Queries.Tokens as Tokens
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.Updates as Updates
import qualified Concordium.Wasm as Wasm
import qualified Data.Text.Encoding as TE

import Concordium.Client.Types.ConsensusStatus
import qualified Concordium.Crypto.BlockSignature as BlockSignature
import qualified Proto.V2.Concordium.Kernel as ProtoKernel
import qualified Proto.V2.Concordium.ProtocolLevelTokens as ProtoPLT
import qualified Proto.V2.Concordium.ProtocolLevelTokens_Fields as ProtoFieldsPLT
import qualified Proto.V2.Concordium.Service as CS
import qualified Proto.V2.Concordium.Types as Proto
import qualified Proto.V2.Concordium.Types as ProtoFields
import qualified Proto.V2.Concordium.Types_Fields as Proto
import qualified Proto.V2.Concordium.Types_Fields as ProtoFields

-- | A helper function that serves as an inverse to @mkSerialize@,
--
--  Converts a protocol buffer message to a native Haskell type
--  that is a member of @Serialize@.
--
--  More concretely, the protocol buffer message should have the form:
--  @
--  > message Wrapper {
--  >    ..
--  >    bytes value = 1
--  >    ...
--  > }
--  @
--
--  where @Wrapper@ is an arbitrary message name that must contain a field
--  named @value@ of type @bytes@. Returns @Left@ if the bytestring contained
--  in the @value@-field could not be converted or if the entire bytestring was
--  not consumed while converting it. Returns a @Right@ wrapping the converted
--  value otherwise.
deMkSerialize ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        BS.ByteString,
      S.Serialize a
    ) =>
    b ->
    Either String a
deMkSerialize val =
    case decodeAndConsume (val ^. ProtoFields.value) of
        Left err -> Left $ "deMkSerialize error: " <> err
        Right v -> return v

-- | A helper function that serves as an inverse to @mkWord64@,
--  Like @deMkSerialize@, but the @value@ field must be a @Word64@,
--  and the output a type which can be coerced from a @Word64@.
--  Coercible here means that the output is a @Word64@ wrapped in
--  a (or possibly several) newtype wrapper(s).
deMkWord64 ::
    ( Coercible Word64 a,
      Data.ProtoLens.Field.HasField
        b
        "value"
        Word64
    ) =>
    b ->
    a
deMkWord64 val = coerce $ val ^. ProtoFields.value

-- | Like @deMkWord64@, but the value field should be a @Word32@
--  and the output should be coercible from this.
deMkWord32 ::
    ( Coercible Word32 a,
      Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    a
deMkWord32 val = coerce $ val ^. ProtoFields.value

-- | Like @deMkWord32@ but the output should be coercible from
--  a @Word16@. Returns @Left@ if the value of the @Word32@ can
--  not fit in a @Word16@ and a @Right@ wrapping the coerced value
--  otherwise.
deMkWord16 ::
    ( Coercible Word16 a,
      Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Either String a
deMkWord16 val =
    if v <= fromIntegral (maxBound :: Word16)
        then return $ coerce (fromIntegral v :: Word16)
        else
            Left $
                "deMkWord16: Input value is "
                    <> show v
                    <> " but can at most be "
                    <> show (maxBound :: Word16)
                    <> "."
  where
    v = val ^. ProtoFields.value

-- | Like @deMkWord32@ but the output should be coercible from
--  a @Word8@. Returns @Left@ if the value of the @Word32@ can
--  not fit in a @Word8@ and a @Right@ wrapping the coerced value
--  otherwise.
deMkWord8 ::
    ( Coercible Word8 a,
      Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Either String a
deMkWord8 val =
    if v <= fromIntegral (maxBound :: Word8)
        then return $ coerce (fromIntegral v :: Word8)
        else
            Left $
                "deMkWord8: Input value is "
                    <> show v
                    <> " but can at most be "
                    <> show (maxBound :: Word8)
                    <> "."
  where
    v = val ^. ProtoFields.value

-- | Validate a protocol buffer message with an @url@ Text field.
--  Returns @Left@ if the string is longer than the maxium permissible
--  length, and returns a @UrlText@ wrapped in a @Right@ otherwise.
deMkUrlText ::
    ( Data.ProtoLens.Field.HasField
        b
        "url"
        Text
    ) =>
    b ->
    Either String UrlText
deMkUrlText val =
    if Text.length v <= fromIntegral maxUrlTextLength
        then return $ UrlText v
        else
            Left $
                "deMkUrlText: URL text had "
                    <> show (Text.length v)
                    <> " characters but can at most be "
                    <> show maxUrlTextLength
                    <> "."
  where
    v = val ^. ProtoFields.url

-- | Decode a bytestring and return the decoded value if the
--  input was consumed. Returns @Left@ if the bytestring
--  could not be decoded or if the input was not consumed,
--  and the decoded value wrapped in a @Right@ otherwise.
decodeAndConsume :: (S.Serialize a) => ByteString -> Either String a
decodeAndConsume bs = do
    case S.runGet getter bs of
        Right val -> Right val
        Left err ->
            Left $
                "decodeAndConsume: Got "
                    <> err
                    <> " while attempting to decode '"
                    <> show bs
                    <> "'."
  where
    getter = do
        v <- S.get
        empty <- S.isEmpty
        if empty
            then return v
            else fail "Did not consume the input."

-- | The result of converting a protocol buffer message with @fromProto@.
--  A @Left@ wrapping an error string indicates that the conversion failed
--  and a @Right@ wrapping the converted value indicates that it succeeded.
type FromProtoResult a = Either String a

-- | A helper class analogous to something like Aeson's FromJSON.
--  It exists to make it more manageable to convert the Protobuf
--  types to their internal Haskell type equivalents.
class FromProto a where
    -- | The corresponding Haskell type.
    type Output a

    -- | A conversion function from the protobuf type to its Haskell
    --  equivalent.
    fromProto :: a -> FromProtoResult (Output a)

-- | A helper to be used to indicate that something went wrong in a
--  @FromProto@ instance.
fromProtoFail :: String -> FromProtoResult a
fromProtoFail msg = Left $ "fromProto: " <> msg

-- | Like @fromProtoM@ but maps from and into the @Maybe@ monad.
--  Returns @Nothing@ if the input is @Nothing@ or if the input
--  value wrapped in @Just@ could not be converted. Returns a
--  @Just@ wrapping the converted value otherwise
fromProtoMaybe :: (FromProto a) => Maybe a -> FromProtoResult (Maybe (Output a))
fromProtoMaybe Nothing = Right Nothing
fromProtoMaybe (Just m) =
    case fromProto m of
        Left err -> Left err
        Right val -> return $ Just val

instance FromProto Proto.AccountIndex where
    type Output Proto.AccountIndex = AccountIndex
    fromProto = return . deMkWord64

instance FromProto ProtoKernel.AccountAddress where
    type Output ProtoKernel.AccountAddress = AccountAddress
    fromProto aa =
        case deMkSerialize aa of
            Left err -> fromProtoFail $ "Unable to convert 'AccountAddress': " <> err
            Right v -> return v

instance FromProto Proto.CredentialRegistrationId where
    type Output Proto.CredentialRegistrationId = (CredentialRegistrationID, RawCredentialRegistrationID)
    fromProto crId = do
        raw <- case deMkSerialize crId of
            Left err -> fromProtoFail $ "Unable to convert 'CredentialRegistrationId': " <> err
            Right v -> Right v
        case credIdFromRaw raw of
            Left err -> fromProtoFail $ "Unable to convert 'CredentialRegistrationId'. " <> err
            Right nonRaw -> return (nonRaw, raw)

instance FromProto Proto.AccountIdentifierInput where
    type Output Proto.AccountIdentifierInput = AccountIdentifier
    fromProto a = do
        aii <- case a ^. ProtoFields.maybe'accountIdentifierInput of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'AccountIdentifierInput' due to missing field in response payload."
            Just v -> Right v
        case aii of
            Proto.AccountIdentifierInput'AccountIndex accIdx -> AccIndex <$> fromProto accIdx
            Proto.AccountIdentifierInput'Address addr -> AccAddress <$> fromProto addr
            Proto.AccountIdentifierInput'CredId cId -> CredRegID . snd <$> fromProto cId

instance FromProto Proto.SequenceNumber where
    type Output Proto.SequenceNumber = Nonce
    fromProto sn =
        -- The nonce cannot be 0.
        if word == 0
            then fromProtoFail "Unable to convert 'SequenceNumber', nonce was 0."
            else return $ Nonce word
      where
        word = deMkWord64 sn

instance FromProto Proto.ReleaseSchedule where
    type Output Proto.ReleaseSchedule = AccountReleaseSummary
    fromProto ars = do
        releaseTotal <- fromProto $ ars ^. ProtoFields.total
        releaseSchedule <- do
            let schedules = ars ^. ProtoFields.schedules
            mapM fromProto schedules
        return AccountReleaseSummary{..}

instance FromProto Proto.Timestamp where
    type Output Proto.Timestamp = Timestamp
    fromProto t = do
        let time = fromIntegral $ t ^. ProtoFields.value
        return $ Timestamp time

instance FromProto Proto.TransactionHash where
    type Output Proto.TransactionHash = TransactionHashV0
    fromProto th =
        case deMkSerialize th of
            Left err -> fromProtoFail $ "Unable to convert 'TransactionHash': " <> err
            Right v -> Right v

instance FromProto Proto.Release where
    type Output Proto.Release = ScheduledRelease
    fromProto r = do
        releaseAmount <- fromProto $ r ^. ProtoFields.amount
        releaseTimestamp <- do
            let time = r ^. ProtoFields.timestamp
            fromProto time
        releaseTransactions <- do
            let ts = r ^. ProtoFields.transactions
            mapM fromProto ts
        return ScheduledRelease{..}

instance FromProto Proto.AccountThreshold where
    type Output Proto.AccountThreshold = AccountThreshold
    fromProto threshold = do
        tr <- do
            case deMkWord8 threshold of
                Left err ->
                    fromProtoFail $
                        "Unable to convert 'AccountThreshold': " <> err
                Right v -> return v
        if tr == 0
            then fromProtoFail "Unable to convert 'AccountThreshold', threshold was 0."
            else return $ AccountThreshold tr

instance FromProto Proto.EncryptedAmount where
    type Output Proto.EncryptedAmount = EncryptedAmount
    fromProto ea =
        case deMkSerialize ea of
            Left err -> fromProtoFail $ "Unable to convert 'EncryptedAmount': " <> err
            Right v -> Right v

instance FromProto Proto.EncryptedBalance where
    type Output Proto.EncryptedBalance = AccountEncryptedAmount
    fromProto eb = do
        _selfAmount <- fromProto $ eb ^. ProtoFields.selfAmount
        _startIndex <- do
            let si = eb ^. ProtoFields.startIndex
            return $ EncryptedAmountAggIndex . fromIntegral $ si
        _aggregatedAmount <- do
            case ( eb ^. ProtoFields.maybe'aggregatedAmount,
                   eb ^. ProtoFields.maybe'numAggregated
                 ) of
                (Just aa, Just na) -> do
                    aa' <- fromProto aa
                    return $ Just (aa', na)
                (Just _, Nothing) -> fromProtoFail "Unable to convert 'EncryptedBalance'. 'numAggregated' was missing."
                (Nothing, Just _) -> fromProtoFail "Unable to convert 'EncryptedBalance'. 'aggregatedAmount' was missing."
                _ -> return Nothing
        _incomingEncryptedAmounts <- do
            let eAmounts = eb ^. ProtoFields.incomingAmounts
            Seq.fromList <$> mapM fromProto eAmounts
        return AccountEncryptedAmount{..}

instance FromProto Proto.EncryptionKey where
    type Output Proto.EncryptionKey = AccountEncryptionKey
    fromProto ek =
        case deMkSerialize ek of
            Left err -> fromProtoFail $ "Unable to convert 'EncryptionKey': " <> err
            Right v -> Right v

instance FromProto Proto.BakerId where
    type Output Proto.BakerId = BakerId
    fromProto = return . deMkWord64

instance FromProto Proto.DelegatorId where
    type Output Proto.DelegatorId = DelegatorId
    fromProto dId = fmap DelegatorId . fromProto $ dId ^. ProtoFields.id

instance FromProto Proto.DelegationTarget where
    type Output Proto.DelegationTarget = DelegationTarget
    fromProto dTarget = do
        dt <- case dTarget ^. Proto.maybe'target of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'DelegationTarget' due to missing field in response payload."
            Just v -> return v
        case dt of
            Proto.DelegationTarget'Passive _ -> return DelegatePassive
            Proto.DelegationTarget'Baker baker -> DelegateToBaker <$> fromProto baker

instance FromProto Proto.StakePendingChange where
    type Output Proto.StakePendingChange = StakePendingChange' Timestamp
    fromProto pc = do
        spc <- case pc ^. Proto.maybe'change of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'StakePendingChange' due to missing field in response payload."
            Just v -> return v
        case spc of
            Proto.StakePendingChange'Reduce' reduce -> do
                ns <- fromProto $ reduce ^. ProtoFields.newStake
                ts <- fromProto $ reduce ^. ProtoFields.effectiveTime
                return $ ReduceStake ns ts
            Proto.StakePendingChange'Remove remove -> do
                rm <- fromProto remove
                return $ RemoveStake rm

instance FromProto Proto.BakerInfo where
    type Output Proto.BakerInfo = BakerInfo
    fromProto bInfo = do
        _bakerIdentity <- fromProto $ bInfo ^. ProtoFields.bakerId
        _bakerElectionVerifyKey <- fromProto $ bInfo ^. ProtoFields.electionKey
        _bakerSignatureVerifyKey <- fromProto $ bInfo ^. ProtoFields.signatureKey
        _bakerAggregationVerifyKey <- fromProto $ bInfo ^. ProtoFields.aggregationKey
        return BakerInfo{..}

instance FromProto Proto.BakerSignatureVerifyKey where
    type Output Proto.BakerSignatureVerifyKey = BakerSignVerifyKey
    fromProto bsvk =
        case deMkSerialize bsvk of
            Left err -> fromProtoFail $ "Unable to convert 'BakerSignatureVerifyKey': " <> err
            Right v -> Right v

instance FromProto Proto.BakerElectionVerifyKey where
    type Output Proto.BakerElectionVerifyKey = BakerElectionVerifyKey
    fromProto bevk =
        case deMkSerialize bevk of
            Left err -> fromProtoFail $ "Unable to convert 'BakerElectionVerifyKey': " <> err
            Right v -> Right v

instance FromProto Proto.BakerAggregationVerifyKey where
    type Output Proto.BakerAggregationVerifyKey = BakerAggregationVerifyKey
    fromProto bavk =
        case deMkSerialize bavk of
            Left err -> fromProtoFail $ "Unable to convert 'BakerAggregationVerifyKey': " <> err
            Right v -> Right v

instance FromProto Proto.OpenStatus where
    type Output Proto.OpenStatus = OpenStatus
    fromProto = \case
        Proto.OPEN_STATUS_OPEN_FOR_ALL -> return OpenForAll
        Proto.OPEN_STATUS_CLOSED_FOR_NEW -> return ClosedForNew
        Proto.OPEN_STATUS_CLOSED_FOR_ALL -> return ClosedForAll
        ProtoFields.OpenStatus'Unrecognized _ ->
            fromProtoFail "Unable to convert 'OpenStatus'."

instance FromProto Proto.BakerPoolInfo where
    type Output Proto.BakerPoolInfo = BakerPoolInfo
    fromProto poolInfo = do
        _poolOpenStatus <- fromProto $ poolInfo ^. ProtoFields.openStatus
        _poolMetadataUrl <- deMkUrlText poolInfo
        _poolCommissionRates <- fromProto $ poolInfo ^. ProtoFields.commissionRates
        return BakerPoolInfo{..}

instance FromProto Proto.AmountFraction where
    type Output Proto.AmountFraction = AmountFraction

    -- This must not exceed 100000. We check it here since
    -- @makePartsPerHundredThousand@ referenced in
    -- @makeAmountFraction@ assumes this.
    fromProto amountFraction = do
        let af = amountFraction ^. ProtoFields.partsPerHundredThousand
        if af > 100_000
            then fromProtoFail "Unable to convert 'AmountFraction', partsPerHundredThousand exceeds 100000."
            else return $ makeAmountFraction af

instance FromProto Proto.CommissionRates where
    type Output Proto.CommissionRates = CommissionRates
    fromProto commissionRates = do
        _finalizationCommission <- fromProto $ commissionRates ^. ProtoFields.finalization
        _bakingCommission <- fromProto $ commissionRates ^. ProtoFields.baking
        _transactionCommission <- fromProto $ commissionRates ^. ProtoFields.transaction
        return CommissionRates{..}

instance FromProto Proto.AccountStakingInfo where
    type Output Proto.AccountStakingInfo = AccountStakingInfo
    fromProto si = do
        asi <- case si ^. Proto.maybe'stakingInfo of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'AccountStakingInfo' due to missing field in response payload."
            Just v -> return v
        case asi of
            Proto.AccountStakingInfo'Delegator' delegator -> do
                asiStakedAmount <- fromProto $ delegator ^. ProtoFields.stakedAmount
                let asiStakeEarnings = delegator ^. ProtoFields.restakeEarnings
                asiDelegationTarget <- fromProto $ delegator ^. ProtoFields.target
                asiDelegationPendingChange <-
                    case delegator ^. ProtoFields.maybe'pendingChange of
                        Nothing -> return NoChange
                        Just ch -> fmap timestampToUTCTime <$> fromProto ch
                return AccountStakingDelegated{..}
            Proto.AccountStakingInfo'Baker' baker -> do
                asiStakedAmount <- fromProto $ baker ^. ProtoFields.stakedAmount
                let asiStakeEarnings = baker ^. ProtoFields.restakeEarnings
                asiBakerInfo <- fromProto $ baker ^. ProtoFields.bakerInfo
                asiPendingChange <-
                    case baker ^. ProtoFields.maybe'pendingChange of
                        Nothing -> return NoChange
                        Just ch -> fmap timestampToUTCTime <$> fromProto ch
                asiPoolInfo <- fromProtoMaybe $ baker ^. ProtoFields.maybe'poolInfo
                let asiIsSuspended = baker ^. ProtoFields.isSuspended
                return AccountStakingBaker{..}

instance FromProto Proto.ArThreshold where
    type Output Proto.ArThreshold = Threshold
    fromProto t = do
        t' <- do
            case deMkWord8 t of
                Left err ->
                    fromProtoFail $
                        "Unable to convert 'ArThreshold': " <> err
                Right v -> return v
        -- The threshold cannot be 0.
        if t' == 0
            then fromProtoFail "Unable to convert 'ArThreshold'. Threshold was 0."
            else return $ Threshold t'

instance FromProto (Map.Map Word32 Proto.ChainArData) where
    type Output (Map.Map Word32 Proto.ChainArData) = Map.Map ArIdentity ChainArData
    fromProto m = do
        case mapM convert $ Map.toAscList m of
            Left err ->
                Left $
                    "Unable to convert 'Map.Map Word32 ChainArData': " <> err
            Right v -> Right $ Map.fromAscList v
      where
        convert :: (Word32, Proto.ChainArData) -> Either String (ArIdentity, ChainArData)
        convert (arId, chainArData) = do
            let arBytes = chainArData ^. ProtoFields.encIdCredPubShare
            encId <- case S.decode arBytes of
                Left _ ->
                    fromProtoFail
                        "Unable to convert 'ChainArData'. Could not decode share of the encryption of IdCredPub."
                Right val -> Right val
            return (ArIdentity arId, encId)

instance FromProto Proto.Commitment where
    type Output Proto.Commitment = Commitment
    fromProto c =
        case deMkSerialize c of
            Left err -> fromProtoFail $ "Unable to convert 'Commitment': " <> err
            Right v -> Right v

instance FromProto Proto.CredentialCommitments where
    type Output Proto.CredentialCommitments = CredentialDeploymentCommitments
    fromProto cdc = do
        cmmPrf <- fromProto $ cdc ^. ProtoFields.prf
        cmmCredCounter <- fromProto $ cdc ^. ProtoFields.credCounter
        cmmMaxAccounts <- fromProto $ cdc ^. ProtoFields.maxAccounts
        cmmAttributes <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                $ cdc ^. ProtoFields.attributes
        cmmIdCredSecSharingCoeff <- mapM fromProto $ cdc ^. ProtoFields.idCredSecSharingCoeff
        return CredentialDeploymentCommitments{..}
      where
        convert (aTag, comm) = do
            c <- fromProto comm
            return (AttributeTag $ fromIntegral aTag, c)

instance FromProto Proto.AccountCredential where
    type Output Proto.AccountCredential = RawAccountCredential
    fromProto ac = do
        a <- case ac ^. Proto.maybe'credentialValues of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'AccountCredential' due to missing field in response payload."
            Just v -> return v
        case a of
            Proto.AccountCredential'Initial initial -> do
                icdvAccount <- fromProto $ initial ^. ProtoFields.keys
                icdvRegId <- fmap snd . fromProto $ initial ^. ProtoFields.credId
                icdvIpId <- fromProto $ initial ^. ProtoFields.ipId
                icdvPolicy <- fromProto $ initial ^. ProtoFields.policy
                return $ InitialAC InitialCredentialDeploymentValues{..}
            Proto.AccountCredential'Normal normal -> do
                cdvPublicKeys <- fromProto $ normal ^. ProtoFields.keys
                cdvCredId <- fmap snd . fromProto $ normal ^. ProtoFields.credId
                cdvIpId <- fromProto $ normal ^. ProtoFields.ipId
                cdvPolicy <- fromProto $ normal ^. ProtoFields.policy
                cdvThreshold <- fromProto $ normal ^. ProtoFields.arThreshold
                cdvArData <- fromProto $ normal ^. ProtoFields.arData
                commitments <- fromProto $ normal ^. ProtoFields.commitments
                return $ NormalAC CredentialDeploymentValues{..} commitments

instance FromProto Proto.YearMonth where
    type Output Proto.YearMonth = YearMonth
    fromProto yearMonth = do
        let ymYear = fromIntegral $ yearMonth ^. ProtoFields.year
        let ymMonth = fromIntegral $ yearMonth ^. ProtoFields.month
        if 1_000 <= ymYear && ymYear <= 9_999 && 1 <= ymMonth && ymMonth <= 12
            then return YearMonth{..}
            else
                fromProtoFail $
                    "Unable to convert 'YearMonth', got year '"
                        <> show ymYear
                        <> "', month '"
                        <> show ymMonth
                        <> "'."

instance FromProto Proto.Policy where
    type Output Proto.Policy = Policy
    fromProto p = do
        pCreatedAt <- fromProto $ p ^. ProtoFields.createdAt
        pValidTo <- fromProto $ p ^. ProtoFields.validTo
        pItems <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                $ p ^. ProtoFields.attributes
        return Policy{..}
      where
        -- \|Convert a tag and a bytestring pair to an attribute tag and attribute value pair.
        -- The length of the bytestring should be at most 31 and the tag should fit in an @Word8@.
        convert (aTag, aVal) = do
            let fits = aTag <= fromIntegral (maxBound :: Word8)
            unless fits $ fromProtoFail $ "Unable to convert 'Policy'. Attribute tag exceeds " <> show (maxBound :: Word8) <> "."
            return (AttributeTag $ fromIntegral aTag, AttributeValue $ BSS.toShort aVal)

instance FromProto Proto.IdentityProviderIdentity where
    type Output Proto.IdentityProviderIdentity = IdentityProviderIdentity
    fromProto = return . deMkWord32

instance FromProto Proto.CredentialPublicKeys where
    type Output Proto.CredentialPublicKeys = CredentialPublicKeys
    fromProto cpk = do
        credKeys <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                $ cpk ^. ProtoFields.keys
        credThreshold <-
            fmap SignatureThreshold
                . decode
                $ cpk ^. ProtoFields.threshold
        -- The threshold cannot be 0.
        if credThreshold == 0
            then fromProtoFail "Unable to convert 'CredentialPublicKeys'. Threshold was 0."
            else return CredentialPublicKeys{..}
      where
        decode t = do
            case deMkWord8 t of
                Left err ->
                    fromProtoFail $
                        "Unable to convert 'CredentialPublicKeys'. Key index was out of range: " <> err
                Right v -> return v

        convert (ki, pKey) = do
            let key = pKey ^. ProtoFields.ed25519Key
            k <- case S.decode key of
                Left err -> fromProtoFail $ "Unable to convert 'CredentialPublicKeys'. Error converting key, got: " <> err
                Right k -> return k
            -- Ensure that the index fits in a Word8.
            if ki > fromIntegral (maxBound :: Word8)
                then fromProtoFail $ "Unable to convert 'CredentialPublicKeys'. Key index exceeds " <> show (maxBound :: Word8) <> "."
                else return (fromIntegral ki, VerifyKeyEd25519 k)

instance FromProto Proto.Cooldown'CooldownStatus where
    type Output Proto.Cooldown'CooldownStatus = CooldownStatus
    fromProto Proto.Cooldown'COOLDOWN = return StatusCooldown
    fromProto Proto.Cooldown'PRE_COOLDOWN = return StatusPreCooldown
    fromProto Proto.Cooldown'PRE_PRE_COOLDOWN = return StatusPrePreCooldown
    fromProto _ = fromProtoFail "Unable to convert 'CooldownStatus'. Unrecognized status."

instance FromProto Proto.Cooldown where
    type Output Proto.Cooldown = Cooldown
    fromProto c = do
        cooldownTimestamp <- fromProto $ c ^. ProtoFields.endTime
        cooldownAmount <- fromProto $ c ^. ProtoFields.amount
        cooldownStatus <- fromProto $ c ^. ProtoFields.status
        return Cooldown{..}

instance FromProto Proto.AccountInfo'Token where
    type Output Proto.AccountInfo'Token = Tokens.Token
    fromProto token = do
        tokenId <- fromProto $ token ^. ProtoFields.tokenId
        tokenAccountState <- fromProto $ token ^. ProtoFields.tokenAccountState
        return Tokens.Token{..}

instance FromProto ProtoPLT.TokenAccountState where
    type Output ProtoPLT.TokenAccountState = Tokens.TokenAccountState
    fromProto tokenAccountState = do
        let memberAllowList = tokenAccountState ^. ProtoFieldsPLT.memberAllowList
        let memberDenyList = tokenAccountState ^. ProtoFieldsPLT.memberDenyList
        balance <- fromProto $ tokenAccountState ^. ProtoFieldsPLT.balance
        return Tokens.TokenAccountState{..}

instance FromProto ProtoPLT.TokenAmount where
    type Output ProtoPLT.TokenAmount = Tokens.TokenAmount
    fromProto tokenAmount = do
        let digits = tokenAmount ^. ProtoFieldsPLT.digits
        let nrDecimals = tokenAmount ^. ProtoFieldsPLT.nrOfDecimals
        return Tokens.TokenAmount{..}

instance FromProto ProtoPLT.TokenId where
    type Output ProtoPLT.TokenId = TokenId
    fromProto tokenId = do
        let textSymbol = tokenId ^. ProtoFieldsPLT.symbol
        let byteString = TE.encodeUtf8 textSymbol
        let symbol = BSS.toShort byteString
        return $ TokenId symbol

instance FromProto Proto.AccountInfo where
    type Output Proto.AccountInfo = AccountInfo
    fromProto ai = do
        aiAccountNonce <- fromProto $ ai ^. ProtoFields.sequenceNumber
        aiAccountAmount <- fromProto $ ai ^. ProtoFields.amount
        aiAccountReleaseSchedule <- fromProto $ ai ^. ProtoFields.schedule
        aiAccountTokens <- mapM fromProto (ai ^. ProtoFields.tokens)
        aiAccountCredentials <-
            do
                fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                $ ai ^. ProtoFields.creds
        aiAccountThreshold <- fromProto $ ai ^. ProtoFields.threshold
        aiAccountEncryptedAmount <- fromProto $ ai ^. ProtoFields.encryptedBalance
        aiAccountEncryptionKey <- fromProto $ ai ^. ProtoFields.encryptionKey
        aiAccountIndex <- fromProto $ ai ^. ProtoFields.index
        aiStakingInfo <-
            case ai ^. ProtoFields.maybe'stake of
                Nothing -> return AccountStakingNone
                Just asi -> fromProto asi
        aiAccountAddress <- fromProto $ ai ^. ProtoFields.address
        aiAccountCooldowns <- mapM fromProto $ ai ^. ProtoFields.cooldowns
        -- The available balance is only provided by node version 7 onwards, so we compute it
        -- if it is not present.
        mAvailableBalance <- mapM fromProto $ ai ^. ProtoFields.maybe'availableBalance
        let stakedTotal =
                sum (cooldownAmount <$> aiAccountCooldowns) + case aiStakingInfo of
                    AccountStakingDelegated{..} -> asiStakedAmount
                    AccountStakingBaker{..} -> asiStakedAmount
                    AccountStakingNone -> 0
        let computedAvailable =
                aiAccountAmount - max stakedTotal (releaseTotal aiAccountReleaseSchedule)
        let aiAccountAvailableAmount = fromMaybe computedAvailable mAvailableBalance
        return AccountInfo{..}
      where
        versionTag = 0
        convert (key, val) = do
            v <- fromProto val
            -- Ensure that the index fits in a Word8.
            if key > fromIntegral (maxBound :: Word8)
                then fromProtoFail $ "Unable to convert 'AccountInfo'. Key exceeds " <> show (maxBound :: Word8) <> "."
                else return (CredentialIndex $ fromIntegral key, Versioned versionTag v)

instance FromProto Proto.BlockHash where
    type Output Proto.BlockHash = BlockHash
    fromProto bh =
        case deMkSerialize bh of
            Left err -> fromProtoFail $ "Unable to convert 'BlockHash': " <> err
            Right v -> return $ BlockHash v

instance FromProto Proto.ModuleRef where
    type Output Proto.ModuleRef = ModuleRef
    fromProto mr =
        case deMkSerialize mr of
            Left err -> fromProtoFail $ "Unable to convert 'ModuleRef': " <> err
            Right ModuleRef{..} -> return ModuleRef{..}

instance FromProto Proto.VersionedModuleSource where
    type Output Proto.VersionedModuleSource = Wasm.WasmModule
    fromProto vmSource = do
        vms <- case vmSource ^. Proto.maybe'module' of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'VersionedModuleSource' due to missing field in response payload."
            Just v -> return v
        case vms of
            Proto.VersionedModuleSource'V0 v0 ->
                return $ Wasm.WasmModuleV0 . Wasm.WasmModuleV . Wasm.ModuleSource $ v0 ^. Proto.value
            Proto.VersionedModuleSource'V1 v1 ->
                return $ Wasm.WasmModuleV1 . Wasm.WasmModuleV . Wasm.ModuleSource $ v1 ^. Proto.value

instance FromProto Proto.ContractStateV0 where
    type Output Proto.ContractStateV0 = Wasm.ContractState
    fromProto cs = return . Wasm.ContractState $ cs ^. ProtoFields.value

instance FromProto Proto.ReceiveName where
    type Output Proto.ReceiveName = Wasm.ReceiveName
    fromProto name = do
        let n = name ^. ProtoFields.value
        unless (Wasm.isValidReceiveName n) $
            fromProtoFail "Unable to convert 'ReceiveName'. Got invalid receive name."
        return $ Wasm.ReceiveName n

instance FromProto Proto.InitName where
    type Output Proto.InitName = Wasm.InitName
    fromProto name = do
        let n = name ^. ProtoFields.value
        unless (Wasm.isValidInitName n) $
            fromProtoFail "Unable to convert 'InitName'. Got invalid init name."
        return $ Wasm.InitName n

instance FromProto Proto.InstanceInfo where
    type Output Proto.InstanceInfo = Wasm.InstanceInfo
    fromProto iInfo = do
        ii <- case iInfo ^. Proto.maybe'version of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'InstanceInfo' due to missing field in response payload."
            Just v -> return v
        case ii of
            Proto.InstanceInfo'V0' v0 -> do
                iiModel <- fromProto $ v0 ^. ProtoFields.model
                iiOwner <- fromProto $ v0 ^. ProtoFields.owner
                iiAmount <- fromProto $ v0 ^. ProtoFields.amount
                iiMethods <- Set.fromList <$> mapM fromProto (v0 ^. ProtoFields.methods)
                iiName <- fromProto $ v0 ^. ProtoFields.name
                iiSourceModule <- fromProto $ v0 ^. ProtoFields.sourceModule
                return Wasm.InstanceInfoV0{..}
            Proto.InstanceInfo'V1' v1 -> do
                iiOwner <- fromProto $ v1 ^. ProtoFields.owner
                iiAmount <- fromProto $ v1 ^. ProtoFields.amount
                iiMethods <- Set.fromList <$> mapM fromProto (v1 ^. ProtoFields.methods)
                iiName <- fromProto $ v1 ^. ProtoFields.name
                iiSourceModule <- fromProto $ v1 ^. ProtoFields.sourceModule
                return Wasm.InstanceInfoV1{..}

instance FromProto Proto.NextAccountSequenceNumber where
    type Output Proto.NextAccountSequenceNumber = NextAccountNonce
    fromProto asn = do
        nanNonce <- fromProto $ asn ^. ProtoFields.sequenceNumber
        let nanAllFinal = asn ^. ProtoFields.allFinal
        return NextAccountNonce{..}

instance FromProto Proto.ProtocolVersion where
    type Output Proto.ProtocolVersion = ProtocolVersion
    fromProto = \case
        Proto.PROTOCOL_VERSION_1 -> return P1
        Proto.PROTOCOL_VERSION_2 -> return P2
        Proto.PROTOCOL_VERSION_3 -> return P3
        Proto.PROTOCOL_VERSION_4 -> return P4
        Proto.PROTOCOL_VERSION_5 -> return P5
        Proto.PROTOCOL_VERSION_6 -> return P6
        Proto.PROTOCOL_VERSION_7 -> return P7
        Proto.PROTOCOL_VERSION_8 -> return P8
        Proto.PROTOCOL_VERSION_9 -> return P9
        Proto.ProtocolVersion'Unrecognized _ ->
            fromProtoFail "Unable to convert 'ProtocolVersion'."

instance FromProto Proto.Duration where
    type Output Proto.Duration = Duration
    fromProto = return . deMkWord64

instance FromProto Proto.BlockHeight where
    type Output Proto.BlockHeight = BlockHeight
    fromProto = return . deMkWord64

instance FromProto Proto.AbsoluteBlockHeight where
    type Output Proto.AbsoluteBlockHeight = AbsoluteBlockHeight
    fromProto = return . deMkWord64

instance FromProto Proto.GenesisIndex where
    type Output Proto.GenesisIndex = GenesisIndex
    fromProto = return . deMkWord32

instance FromProto Proto.ConsensusInfo where
    type Output Proto.ConsensusInfo = ConsensusStatus
    fromProto ci = do
        csBestBlock <- fromProto $ ci ^. ProtoFields.bestBlock
        csGenesisBlock <- fromProto $ ci ^. ProtoFields.genesisBlock
        csGenesisTime <- fmap timestampToUTCTime . fromProto $ ci ^. ProtoFields.genesisTime
        csSlotDuration <- fromProtoMaybe $ ci ^. ProtoFields.maybe'slotDuration
        csEpochDuration <- fromProto $ ci ^. ProtoFields.epochDuration
        csLastFinalizedBlock <- fromProto $ ci ^. ProtoFields.lastFinalizedBlock
        csBestBlockHeight <- fromProto $ ci ^. ProtoFields.bestBlockHeight
        csLastFinalizedBlockHeight <- fromProto $ ci ^. ProtoFields.lastFinalizedBlockHeight
        let csBlocksReceivedCount = fromIntegral $ ci ^. ProtoFields.blocksReceivedCount
        let csBlocksVerifiedCount = fromIntegral $ ci ^. ProtoFields.blocksVerifiedCount
        csBlockLastReceivedTime <- do
            blrTime <- fromProtoMaybe $ ci ^. ProtoFields.maybe'blockLastReceivedTime
            return $ timestampToUTCTime <$> blrTime
        let csBlockReceiveLatencyEMA = ci ^. ProtoFields.blockReceiveLatencyEma
        let csBlockReceiveLatencyEMSD = ci ^. ProtoFields.blockReceiveLatencyEmsd
        let csBlockReceivePeriodEMA = ci ^. ProtoFields.maybe'blockReceivePeriodEma
        let csBlockReceivePeriodEMSD = ci ^. ProtoFields.maybe'blockReceivePeriodEmsd
        csBlockLastArrivedTime <- (fmap . fmap) timestampToUTCTime . fromProtoMaybe $ ci ^. ProtoFields.maybe'blockLastArrivedTime
        let csBlockArriveLatencyEMA = ci ^. ProtoFields.blockArriveLatencyEma
        let csBlockArriveLatencyEMSD = ci ^. ProtoFields.blockArriveLatencyEmsd
        let csBlockArrivePeriodEMA = ci ^. ProtoFields.maybe'blockArrivePeriodEma
        let csBlockArrivePeriodEMSD = ci ^. ProtoFields.maybe'blockArrivePeriodEmsd
        let csTransactionsPerBlockEMA = ci ^. ProtoFields.transactionsPerBlockEma
        let csTransactionsPerBlockEMSD = ci ^. ProtoFields.transactionsPerBlockEmsd
        let csFinalizationCount = fromIntegral $ ci ^. ProtoFields.finalizationCount
        csLastFinalizedTime <- (fmap . fmap) timestampToUTCTime . fromProtoMaybe $ ci ^. ProtoFields.maybe'lastFinalizedTime
        let csFinalizationPeriodEMA = ci ^. ProtoFields.maybe'finalizationPeriodEma
        let csFinalizationPeriodEMSD = ci ^. ProtoFields.maybe'finalizationPeriodEmsd
        csProtocolVersion <- fromProto $ ci ^. ProtoFields.protocolVersion
        csGenesisIndex <- fromProto $ ci ^. ProtoFields.genesisIndex
        csCurrentEraGenesisBlock <- fromProto $ ci ^. ProtoFields.currentEraGenesisBlock
        csCurrentEraGenesisTime <- fmap timestampToUTCTime . fromProto $ ci ^. ProtoFields.currentEraGenesisTime
        currentTimeoutDuration <- fromProtoMaybe $ ci ^. ProtoFields.maybe'currentTimeoutDuration
        currentRound <- fromProtoMaybe $ ci ^. ProtoFields.maybe'currentRound
        currentEpoch <- fromProtoMaybe $ ci ^. ProtoFields.maybe'currentEpoch
        triggerBlockTime <- (fmap . fmap) timestampToUTCTime . fromProtoMaybe $ ci ^. ProtoFields.maybe'triggerBlockTime
        let csConcordiumBFTStatus = case (currentTimeoutDuration, currentRound, currentEpoch, triggerBlockTime) of
                (Just cbftsCurrentTimeoutDuration, Just cbftsCurrentRound, Just cbftsCurrentEpoch, Just cbftsTriggerBlockTime) -> Just ConcordiumBFTStatus{..}
                _ -> Nothing
        return ConsensusStatus{..}

instance FromProto Proto.Slot where
    type Output Proto.Slot = Slot
    fromProto = return . deMkWord64

instance FromProto Proto.Round where
    type Output Proto.Round = Round
    fromProto = return . deMkWord64

instance FromProto Proto.StateHash where
    type Output Proto.StateHash = StateHash
    fromProto sh =
        case deMkSerialize sh of
            Left err -> fromProtoFail $ "Unable to convert 'StateHash': " <> err
            Right StateHashV0{..} -> return StateHashV0{..}

instance FromProto Proto.Energy where
    type Output Proto.Energy = Energy
    fromProto = return . deMkWord64

instance FromProto Proto.BlockInfo where
    type Output Proto.BlockInfo = BlockInfo
    fromProto bi = do
        biBlockHash <- fromProto $ bi ^. ProtoFields.hash
        biBlockHeight <- fromProto $ bi ^. ProtoFields.height
        biBlockParent <- fromProto $ bi ^. ProtoFields.parentBlock
        biBlockLastFinalized <- fromProto $ bi ^. ProtoFields.lastFinalizedBlock
        biGenesisIndex <- fromProto $ bi ^. ProtoFields.genesisIndex
        biEraBlockHeight <- fromProto $ bi ^. ProtoFields.eraBlockHeight
        biBlockReceiveTime <- fmap timestampToUTCTime . fromProto $ bi ^. ProtoFields.receiveTime
        biBlockArriveTime <- fmap timestampToUTCTime . fromProto $ bi ^. ProtoFields.arriveTime
        biBlockSlot <- fromProtoMaybe $ bi ^. ProtoFields.maybe'slotNumber
        biBlockSlotTime <- fmap timestampToUTCTime . fromProto $ bi ^. ProtoFields.slotTime
        biBlockBaker <- fromProtoMaybe $ bi ^. ProtoFields.maybe'baker
        let biFinalized = bi ^. ProtoFields.finalized
        let biTransactionCount = fromIntegral $ bi ^. ProtoFields.transactionCount
        biTransactionEnergyCost <- fromProto $ bi ^. ProtoFields.transactionsEnergyCost
        let biTransactionsSize = fromIntegral $ bi ^. ProtoFields.transactionsSize
        biBlockStateHash <- fromProto $ bi ^. ProtoFields.stateHash
        biProtocolVersion <- fromProto $ bi ^. ProtoFields.protocolVersion
        biRound <- fromProtoMaybe $ bi ^. ProtoFields.maybe'round
        biEpoch <- fromProtoMaybe $ bi ^. ProtoFields.maybe'epoch
        return BlockInfo{..}

instance FromProto Proto.Amount where
    type Output Proto.Amount = Amount
    fromProto = return . deMkWord64

instance FromProto Proto.PoolCurrentPaydayInfo where
    type Output Proto.PoolCurrentPaydayInfo = CurrentPaydayBakerPoolStatus
    fromProto cpi = do
        let bpsBlocksBaked = fromIntegral $ cpi ^. ProtoFields.blocksBaked
        let bpsFinalizationLive = cpi ^. ProtoFields.finalizationLive
        bpsTransactionFeesEarned <- fromProto $ cpi ^. ProtoFields.transactionFeesEarned
        bpsEffectiveStake <- fromProto $ cpi ^. ProtoFields.effectiveStake
        let bpsLotteryPower = cpi ^. ProtoFields.lotteryPower
        bpsBakerEquityCapital <- fromProto $ cpi ^. ProtoFields.bakerEquityCapital
        bpsDelegatedCapital <- fromProto $ cpi ^. ProtoFields.delegatedCapital
        bpsCommissionRates <- fromProto $ cpi ^. ProtoFields.commissionRates
        let bpsIsPrimedForSuspension = cpi ^. ProtoFields.maybe'isPrimedForSuspension
        let bpsMissedRounds = cpi ^. ProtoFields.maybe'missedRounds
        return CurrentPaydayBakerPoolStatus{..}

instance FromProto Proto.PoolInfoResponse where
    type Output Proto.PoolInfoResponse = BakerPoolStatus
    fromProto pir = do
        psBakerId <- fromProto $ pir ^. ProtoFields.baker
        psBakerAddress <- fromProto $ pir ^. ProtoFields.address
        psActiveStatus <- case pir ^. ProtoFields.maybe'poolInfo of
            Nothing -> return Nothing
            Just poolInfo -> do
                abpsPoolInfo <- fromProto poolInfo
                abpsBakerEquityCapital <- fromProto $ pir ^. ProtoFields.equityCapital
                abpsDelegatedCapital <- fromProto $ pir ^. ProtoFields.delegatedCapital
                abpsDelegatedCapitalCap <- fromProto $ pir ^. ProtoFields.delegatedCapitalCap
                let abpsIsSuspended = pir ^. ProtoFields.maybe'isSuspended
                abpsBakerStakePendingChange <-
                    case pir ^. ProtoFields.maybe'equityPendingChange of
                        Nothing -> return PPCNoChange
                        Just ppc -> fromProto ppc
                return $ Just ActiveBakerPoolStatus{..}
        psCurrentPaydayStatus <- fromProtoMaybe $ pir ^. ProtoFields.maybe'currentPaydayInfo
        psAllPoolTotalCapital <- fromProto $ pir ^. ProtoFields.allPoolTotalCapital
        return BakerPoolStatus{..}

instance FromProto Proto.PassiveDelegationInfo where
    type Output Proto.PassiveDelegationInfo = PassiveDelegationStatus
    fromProto pdi = do
        pdsDelegatedCapital <- fromProto $ pdi ^. ProtoFields.delegatedCapital
        pdsCommissionRates <- fromProto $ pdi ^. ProtoFields.commissionRates
        pdsCurrentPaydayTransactionFeesEarned <- fromProto $ pdi ^. ProtoFields.currentPaydayTransactionFeesEarned
        pdsCurrentPaydayDelegatedCapital <- fromProto $ pdi ^. ProtoFields.currentPaydayDelegatedCapital
        pdsAllPoolTotalCapital <- fromProto $ pdi ^. ProtoFields.allPoolTotalCapital
        return PassiveDelegationStatus{..}

instance FromProto Proto.PoolPendingChange where
    type Output Proto.PoolPendingChange = PoolPendingChange
    fromProto ppChange = do
        ppc <- case ppChange ^. Proto.maybe'change of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'PoolPendingChange' due to missing field in response payload."
            Just v -> return v
        case ppc of
            Proto.PoolPendingChange'Reduce' reduce -> do
                ppcBakerEquityCapital <- fromProto $ reduce ^. ProtoFields.reducedEquityCapital
                ppcEffectiveTime <- fmap timestampToUTCTime . fromProto $ reduce ^. ProtoFields.effectiveTime
                return PPCReduceBakerCapital{..}
            Proto.PoolPendingChange'Remove' remove -> do
                ppcEffectiveTime <- fmap timestampToUTCTime . fromProto $ remove ^. ProtoFields.effectiveTime
                return PPCRemovePool{..}

instance FromProto Proto.BlocksAtHeightResponse where
    type Output Proto.BlocksAtHeightResponse = [BlockHash]
    fromProto bahr = mapM fromProto $ bahr ^. ProtoFields.blocks

instance FromProto Proto.MintRate where
    type Output Proto.MintRate = MintRate
    fromProto mr = do
        let mrMantissa = mr ^. ProtoFields.mantissa
        let mrExponent = fromIntegral $ mr ^. ProtoFields.exponent
        -- Ensure that the exponent fits in a Word8.
        if mrExponent > (maxBound :: Word8)
            then
                fromProtoFail $
                    "Unable to convert 'MintRate'. 'mrExponent' exceeds "
                        <> show (maxBound :: Word8)
                        <> "."
            else return MintRate{..}

instance FromProto Proto.TokenomicsInfo where
    type Output Proto.TokenomicsInfo = RewardStatus
    fromProto tInfo = do
        ti <- case tInfo ^. Proto.maybe'tokenomics of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'TokenomicsInfo' due to missing field in response payload."
            Just v -> return v
        case ti of
            Proto.TokenomicsInfo'V0' v0 -> do
                rsTotalAmount <- fromProto $ v0 ^. ProtoFields.totalAmount
                rsTotalEncryptedAmount <- fromProto $ v0 ^. ProtoFields.totalEncryptedAmount
                rsBakingRewardAccount <- fromProto $ v0 ^. ProtoFields.bakingRewardAccount
                rsFinalizationRewardAccount <- fromProto $ v0 ^. ProtoFields.finalizationRewardAccount
                rsGasAccount <- fromProto $ v0 ^. ProtoFields.gasAccount
                rsProtocolVersion <- fromProto $ v0 ^. ProtoFields.protocolVersion
                return RewardStatusV0{..}
            Proto.TokenomicsInfo'V1' v1 -> do
                rsTotalAmount <- fromProto $ v1 ^. ProtoFields.totalAmount
                rsTotalEncryptedAmount <- fromProto $ v1 ^. ProtoFields.totalEncryptedAmount
                rsBakingRewardAccount <- fromProto $ v1 ^. ProtoFields.bakingRewardAccount
                rsFinalizationRewardAccount <- fromProto $ v1 ^. ProtoFields.finalizationRewardAccount
                rsGasAccount <- fromProto $ v1 ^. ProtoFields.gasAccount
                rsFoundationTransactionRewards <- fromProto $ v1 ^. ProtoFields.foundationTransactionRewards
                rsNextPaydayTime <- fmap timestampToUTCTime . fromProto $ v1 ^. ProtoFields.nextPaydayTime
                rsNextPaydayMintRate <- fromProto $ v1 ^. ProtoFields.nextPaydayMintRate
                rsTotalStakedCapital <- fromProto $ v1 ^. ProtoFields.totalStakedCapital
                rsProtocolVersion <- fromProto $ v1 ^. ProtoFields.protocolVersion
                return RewardStatusV1{..}

instance FromProto Proto.ContractEvent where
    type Output Proto.ContractEvent = Wasm.ContractEvent
    fromProto ce = return . Wasm.ContractEvent . BSS.toShort $ ce ^. ProtoFields.value

instance FromProto Proto.Parameter where
    type Output Proto.Parameter = Wasm.Parameter
    fromProto parameter = do
        -- Note that Wasm.maxParameterLen SPi <= Wasm.maxParameterLen SP6
        -- for all i < 6. This check should be changed if strictly larger
        -- upper limits are introduced with newer prototols.
        let p = parameter ^. ProtoFields.value
        unless (BS.length p <= fromIntegral (Wasm.maxParameterLen SP6)) $
            fromProtoFail $
                "Unable to convert 'Parameter'. Parameter exceeds "
                    <> show (Wasm.maxParameterLen SP6)
                    <> "."
        return . Wasm.Parameter $ BSS.toShort p

instance FromProto Proto.Address where
    type Output Proto.Address = Address
    fromProto a = do
        addr <- case a ^. Proto.maybe'type' of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'Address' due to missing field in response payload."
            Just v -> return v
        case addr of
            Proto.Address'Account aAddr -> AddressAccount <$> fromProto aAddr
            Proto.Address'Contract cAddr -> AddressContract <$> fromProto cAddr

instance FromProto Proto.ContractAddress where
    type Output Proto.ContractAddress = ContractAddress
    fromProto ca = do
        let contractIndex = ContractIndex $ ca ^. ProtoFields.index
        let contractSubindex = ContractSubindex $ ca ^. ProtoFields.subindex
        return ContractAddress{..}

instance FromProto Proto.ContractVersion where
    type Output Proto.ContractVersion = Wasm.WasmVersion
    fromProto cv = case cv of
        Proto.V0 -> return Wasm.V0
        Proto.V1 -> return Wasm.V1
        Proto.ContractVersion'Unrecognized _ ->
            fromProtoFail "Unable to convert 'ContractVersion'."

instance FromProto Proto.ContractTraceElement where
    type Output Proto.ContractTraceElement = SupplementedEvent
    fromProto ctElement = do
        cte <- case ctElement ^. Proto.maybe'element of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'ContractTraceElement' due to missing field in response payload."
            Just v -> return v
        case cte of
            Proto.ContractTraceElement'Updated updated -> do
                euContractVersion <- fromProto $ updated ^. ProtoFields.contractVersion
                euAddress <- fromProto $ updated ^. ProtoFields.address
                euInstigator <- fromProto $ updated ^. ProtoFields.instigator
                euAmount <- fromProto $ updated ^. ProtoFields.amount
                euMessage <- fromProto $ updated ^. ProtoFields.parameter
                euReceiveName <- fromProto $ updated ^. ProtoFields.receiveName
                euEvents <- mapM fromProto $ updated ^. ProtoFields.events
                return Updated{..}
            Proto.ContractTraceElement'Transferred' transferred -> do
                etFrom <- fmap AddressContract $ fromProto $ transferred ^. ProtoFields.sender
                etAmount <- fromProto $ transferred ^. ProtoFields.amount
                etTo <- fmap AddressAccount $ fromProto $ transferred ^. ProtoFields.receiver
                return Transferred{..}
            Proto.ContractTraceElement'Interrupted' interrupted -> do
                iAddress <- fromProto $ interrupted ^. ProtoFields.address
                iEvents <- mapM fromProto $ interrupted ^. ProtoFields.events
                return Interrupted{..}
            Proto.ContractTraceElement'Resumed' resumed -> do
                rAddress <- fromProto $ resumed ^. ProtoFields.address
                let rSuccess = resumed ^. ProtoFields.success
                return Resumed{..}
            Proto.ContractTraceElement'Upgraded' upgraded -> do
                euAddress <- fromProto $ upgraded ^. ProtoFields.address
                euFrom <- fromProto $ upgraded ^. ProtoFields.from
                euTo <- fromProto $ upgraded ^. ProtoFields.to
                return Upgraded{..}

instance FromProto Proto.RejectReason where
    type Output Proto.RejectReason = RejectReason
    fromProto rReason = do
        r <- case rReason ^. Proto.maybe'reason of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'RejectReason' due to missing field in response payload."
            Just v -> return v
        case r of
            Proto.RejectReason'ModuleNotWf _ ->
                return ModuleNotWF
            Proto.RejectReason'ModuleHashAlreadyExists mr ->
                ModuleHashAlreadyExists <$> fromProto mr
            Proto.RejectReason'InvalidAccountReference addr ->
                InvalidAccountReference <$> fromProto addr
            Proto.RejectReason'InvalidInitMethod' rr -> do
                moduleRef <- fromProto $ rr ^. ProtoFields.moduleRef
                initName <- fromProto $ rr ^. ProtoFields.initName
                return $ InvalidInitMethod moduleRef initName
            Proto.RejectReason'InvalidReceiveMethod' rr -> do
                moduleRef <- fromProto $ rr ^. ProtoFields.moduleRef
                receiveName <- fromProto $ rr ^. ProtoFields.receiveName
                return $ InvalidReceiveMethod moduleRef receiveName
            Proto.RejectReason'InvalidModuleReference mr ->
                InvalidModuleReference <$> fromProto mr
            Proto.RejectReason'InvalidContractAddress cAddr ->
                InvalidContractAddress <$> fromProto cAddr
            Proto.RejectReason'RuntimeFailure _ ->
                return RuntimeFailure
            Proto.RejectReason'AmountTooLarge' rr -> do
                addr <- fromProto $ rr ^. ProtoFields.address
                amt <- fromProto $ rr ^. ProtoFields.amount
                return $ AmountTooLarge addr amt
            Proto.RejectReason'SerializationFailure _ ->
                return SerializationFailure
            Proto.RejectReason'OutOfEnergy _ ->
                return OutOfEnergy
            Proto.RejectReason'RejectedInit' rr ->
                return . RejectedInit $ rr ^. ProtoFields.rejectReason
            Proto.RejectReason'RejectedReceive' rr -> do
                let rejectReason = rr ^. ProtoFields.rejectReason
                contractAddress <- fromProto $ rr ^. ProtoFields.contractAddress
                receiveName <- fromProto $ rr ^. ProtoFields.receiveName
                parameter <- fromProto $ rr ^. ProtoFields.parameter
                return RejectedReceive{..}
            Proto.RejectReason'InvalidProof _ ->
                return InvalidProof
            Proto.RejectReason'AlreadyABaker bakerId ->
                AlreadyABaker <$> fromProto bakerId
            Proto.RejectReason'NotABaker aAddr ->
                NotABaker <$> fromProto aAddr
            Proto.RejectReason'InsufficientBalanceForBakerStake _ ->
                return InsufficientBalanceForBakerStake
            Proto.RejectReason'StakeUnderMinimumThresholdForBaking _ ->
                return StakeUnderMinimumThresholdForBaking
            Proto.RejectReason'BakerInCooldown _ ->
                return BakerInCooldown
            Proto.RejectReason'DuplicateAggregationKey key -> do
                case deMkSerialize key of
                    Left err ->
                        fromProtoFail $
                            "Unable to convert 'RejectReason'DuplicateAggregationKey': " <> err
                    Right k -> return $ DuplicateAggregationKey k
            Proto.RejectReason'NonExistentCredentialId _ ->
                return NonExistentCredentialID
            Proto.RejectReason'KeyIndexAlreadyInUse _ ->
                return KeyIndexAlreadyInUse
            Proto.RejectReason'InvalidAccountThreshold _ ->
                return InvalidAccountThreshold
            Proto.RejectReason'InvalidCredentialKeySignThreshold _ ->
                return InvalidCredentialKeySignThreshold
            Proto.RejectReason'InvalidEncryptedAmountTransferProof _ ->
                return InvalidEncryptedAmountTransferProof
            Proto.RejectReason'InvalidTransferToPublicProof _ ->
                return InvalidTransferToPublicProof
            Proto.RejectReason'EncryptedAmountSelfTransfer aAddr ->
                EncryptedAmountSelfTransfer <$> fromProto aAddr
            Proto.RejectReason'InvalidIndexOnEncryptedTransfer _ ->
                return InvalidIndexOnEncryptedTransfer
            Proto.RejectReason'ZeroScheduledAmount _ ->
                return ZeroScheduledAmount
            Proto.RejectReason'NonIncreasingSchedule _ ->
                return NonIncreasingSchedule
            Proto.RejectReason'FirstScheduledReleaseExpired _ ->
                return FirstScheduledReleaseExpired
            Proto.RejectReason'ScheduledSelfTransfer aAddr ->
                ScheduledSelfTransfer <$> fromProto aAddr
            Proto.RejectReason'InvalidCredentials _ ->
                return InvalidCredentials
            Proto.RejectReason'DuplicateCredIds' rr -> do
                raw <- mapM (fmap snd . fromProto) $ rr ^. ProtoFields.ids
                credIds <- do
                    case mapM credIdFromRaw raw of
                        Left err ->
                            fromProtoFail $
                                "Unable to convert 'RejectReason'DuplicateCredIds''. " <> err
                        Right v -> return v
                return $ DuplicateCredIDs credIds
            Proto.RejectReason'NonExistentCredIds' necIds -> do
                ids <- mapM (fmap fst . fromProto) $ necIds ^. ProtoFields.ids
                return $ NonExistentCredIDs ids
            Proto.RejectReason'RemoveFirstCredential _ -> do
                return RemoveFirstCredential
            Proto.RejectReason'CredentialHolderDidNotSign _ -> do
                return CredentialHolderDidNotSign
            Proto.RejectReason'NotAllowedMultipleCredentials _ -> do
                return NotAllowedMultipleCredentials
            Proto.RejectReason'NotAllowedToReceiveEncrypted _ -> do
                return NotAllowedToReceiveEncrypted
            Proto.RejectReason'NotAllowedToHandleEncrypted _ -> do
                return NotAllowedToHandleEncrypted
            Proto.RejectReason'MissingBakerAddParameters _ -> do
                return MissingBakerAddParameters
            Proto.RejectReason'FinalizationRewardCommissionNotInRange _ -> do
                return FinalizationRewardCommissionNotInRange
            Proto.RejectReason'BakingRewardCommissionNotInRange _ -> do
                return BakingRewardCommissionNotInRange
            Proto.RejectReason'TransactionFeeCommissionNotInRange _ -> do
                return TransactionFeeCommissionNotInRange
            Proto.RejectReason'AlreadyADelegator _ -> do
                return AlreadyADelegator
            Proto.RejectReason'InsufficientBalanceForDelegationStake _ -> do
                return InsufficientBalanceForDelegationStake
            Proto.RejectReason'MissingDelegationAddParameters _ -> do
                return MissingDelegationAddParameters
            Proto.RejectReason'InsufficientDelegationStake _ -> do
                return InsufficientDelegationStake
            Proto.RejectReason'DelegatorInCooldown _ -> do
                return DelegatorInCooldown
            Proto.RejectReason'NotADelegator del -> do
                NotADelegator <$> fromProto del
            Proto.RejectReason'DelegationTargetNotABaker bId -> do
                DelegationTargetNotABaker <$> fromProto bId
            Proto.RejectReason'StakeOverMaximumThresholdForPool _ -> do
                return StakeOverMaximumThresholdForPool
            Proto.RejectReason'PoolWouldBecomeOverDelegated _ -> do
                return PoolWouldBecomeOverDelegated
            Proto.RejectReason'PoolClosed _ -> do
                return PoolClosed
            Proto.RejectReason'NonExistentTokenId tokenId -> do
                NonExistentTokenId <$> fromProto tokenId

instance FromProto Proto.InvokeInstanceResponse where
    type Output Proto.InvokeInstanceResponse = InvokeContract.InvokeContractResult
    fromProto icResult = do
        icr <- case icResult ^. Proto.maybe'result of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'InvokeInstanceResponse' due to missing field in response payload."
            Just v -> return v
        case icr of
            Proto.InvokeInstanceResponse'Success' success -> do
                let rcrReturnValue = success ^. ProtoFields.maybe'returnValue
                rcrUsedEnergy <- fromProto $ success ^. ProtoFields.usedEnergy
                rcrEvents <- mapM fromProto $ success ^. ProtoFields.effects
                return InvokeContract.Success{..}
            Proto.InvokeInstanceResponse'Failure' failure -> do
                let rcrReturnValue = failure ^. ProtoFields.maybe'returnValue
                rcrUsedEnergy <- fromProto $ failure ^. ProtoFields.usedEnergy
                rcrReason <- fromProto $ failure ^. ProtoFields.reason
                return InvokeContract.Failure{..}

instance FromProto Proto.Branch where
    type Output Proto.Branch = Branch
    fromProto branch = do
        branchBlockHash <- fromProto $ branch ^. ProtoFields.blockHash
        branchChildren <- mapM fromProto $ branch ^. ProtoFields.children
        return Branch{..}

instance FromProto Proto.ElectionInfo'Baker where
    type Output Proto.ElectionInfo'Baker = BakerSummary
    fromProto baker = do
        bsBakerId <- fromProto $ baker ^. ProtoFields.baker
        bsBakerAccount <- fromProtoMaybe $ baker ^. ProtoFields.maybe'account
        let bsBakerLotteryPower = baker ^. ProtoFields.lotteryPower
        return BakerSummary{..}

instance FromProto Proto.ElectionDifficulty where
    type Output Proto.ElectionDifficulty = ElectionDifficulty
    fromProto eDiff = do
        let af = eDiff ^. (ProtoFields.value . ProtoFields.partsPerHundredThousand)
        -- This must be strictly less than 100_000.
        if af >= 100_000
            then
                fromProtoFail
                    "Unable to convert 'ElectionDifficulty'. partsPerHundredThousand should be strictly less than 100000."
            else return $ makeElectionDifficultyUnchecked af

instance FromProto Proto.LeadershipElectionNonce where
    type Output Proto.LeadershipElectionNonce = LeadershipElectionNonce
    fromProto len =
        case deMkSerialize len of
            Left err ->
                fromProtoFail $
                    "Unable to convert 'LeadershipElectionNonce': " <> err
            Right nonce -> return nonce

instance FromProto Proto.ElectionInfo where
    type Output Proto.ElectionInfo = BlockBirkParameters
    fromProto eInfo = do
        bbpBakers <-
            mapM fromProto
                . Vec.fromList
                $ eInfo ^. ProtoFields.bakerElectionInfo
        bbpElectionDifficulty <- fromProtoMaybe $ eInfo ^. ProtoFields.maybe'electionDifficulty
        bbpElectionNonce <- fromProto $ eInfo ^. ProtoFields.electionNonce
        return BlockBirkParameters{..}

instance FromProto Proto.NextUpdateSequenceNumbers where
    type Output Proto.NextUpdateSequenceNumbers = NextUpdateSequenceNumbers
    fromProto nums = do
        _nusnRootKeys <- fromProto $ nums ^. ProtoFields.rootKeys
        _nusnLevel1Keys <- fromProto $ nums ^. ProtoFields.level1Keys
        _nusnLevel2Keys <- fromProto $ nums ^. ProtoFields.level2Keys
        _nusnProtocol <- fromProto $ nums ^. ProtoFields.protocol
        _nusnElectionDifficulty <- fromProto $ nums ^. ProtoFields.electionDifficulty
        _nusnEuroPerEnergy <- fromProto $ nums ^. ProtoFields.euroPerEnergy
        _nusnMicroCCDPerEuro <- fromProto $ nums ^. ProtoFields.microCcdPerEuro
        _nusnFoundationAccount <- fromProto $ nums ^. ProtoFields.foundationAccount
        _nusnMintDistribution <- fromProto $ nums ^. ProtoFields.mintDistribution
        _nusnTransactionFeeDistribution <- fromProto $ nums ^. ProtoFields.transactionFeeDistribution
        _nusnGASRewards <- fromProto $ nums ^. ProtoFields.gasRewards
        _nusnPoolParameters <- fromProto $ nums ^. ProtoFields.poolParameters
        _nusnAddAnonymityRevoker <- fromProto $ nums ^. ProtoFields.addAnonymityRevoker
        _nusnAddIdentityProvider <- fromProto $ nums ^. ProtoFields.addIdentityProvider
        _nusnCooldownParameters <- fromProto $ nums ^. ProtoFields.cooldownParameters
        _nusnTimeParameters <- fromProto $ nums ^. ProtoFields.timeParameters
        _nusnTimeoutParameters <- fromProto $ nums ^. ProtoFields.timeoutParameters
        _nusnMinBlockTime <- fromProto $ nums ^. ProtoFields.minBlockTime
        _nusnBlockEnergyLimit <- fromProto $ nums ^. ProtoFields.blockEnergyLimit
        _nusnFinalizationCommitteeParameters <- fromProto $ nums ^. ProtoFields.finalizationCommitteeParameters
        _nusnValidatorScoreParameters <- fromProto $ nums ^. ProtoFields.validatorScoreParameters
        _nusnProtocolLevelTokensParameters <- fromProto $ nums ^. ProtoFields.protocolLevelTokens
        return NextUpdateSequenceNumbers{..}

instance FromProto Proto.IpAddress where
    type Output Proto.IpAddress = IpAddress
    fromProto peer = return . IpAddress $ peer ^. ProtoFields.value

instance FromProto Proto.Port where
    type Output Proto.Port = IpPort
    fromProto port = do
        -- Ensure that the value fits into a Word16.
        let p = port ^. ProtoFields.value
        let fits = p <= fromIntegral (maxBound :: Word16)
        unless fits $
            fromProtoFail $
                "Unable to convert 'Port'. 'port' value exceeds "
                    <> show (maxBound :: Word16)
                    <> "."
        return . IpPort $ fromIntegral p

instance FromProto Proto.BannedPeer where
    type Output Proto.BannedPeer = Peer
    fromProto peer = fromProto $ peer ^. ProtoFields.ipAddress

instance FromProto Proto.BannedPeers where
    type Output Proto.BannedPeers = [Peer]
    fromProto peers = mapM fromProto $ peers ^. ProtoFields.peers

instance FromProto Proto.InstanceStateValueAtKey where
    type Output Proto.InstanceStateValueAtKey = ByteString
    fromProto value = return $ value ^. ProtoFields.value

instance FromProto Proto.Signature where
    type Output Proto.Signature = Signature
    fromProto value = return . Signature $ BSS.toShort $ value ^. ProtoFields.value

instance FromProto Proto.SignatureMap where
    type Output Proto.SignatureMap = Updates.UpdateInstructionSignatures
    fromProto uis = do
        signatures <-
            fmap Map.fromAscList
                . mapM deMk
                . Map.toAscList
                $ uis ^. ProtoFields.signatures
        return Updates.UpdateInstructionSignatures{..}
      where
        deMk (k, s) = do
            -- Ensure that the value fits into a Word16.
            if k <= fromIntegral (maxBound :: Word16)
                then do
                    sig <- fromProto s
                    return (fromIntegral k, sig)
                else
                    fromProtoFail $
                        "Unable to convert 'SignatureMap'. Index exceeds "
                            <> show (maxBound :: Word16)
                            <> "."

instance FromProto Proto.TransactionTime where
    type Output Proto.TransactionTime = TransactionTime
    fromProto = return . deMkWord64

instance FromProto Proto.UpdateSequenceNumber where
    type Output Proto.UpdateSequenceNumber = Nonce
    fromProto = return . deMkWord64

instance FromProto Proto.CredentialType where
    type Output Proto.CredentialType = CredentialType
    fromProto Proto.CREDENTIAL_TYPE_INITIAL = return Initial
    fromProto Proto.CREDENTIAL_TYPE_NORMAL = return Normal
    fromProto (Proto.CredentialType'Unrecognized _) =
        fromProtoFail "Unable to convert 'CredentialType'."

instance FromProto Proto.UpdateType where
    type Output Proto.UpdateType = Updates.UpdateType
    fromProto Proto.UPDATE_PROTOCOL = return Updates.UpdateProtocol
    fromProto Proto.UPDATE_ELECTION_DIFFICULTY = return Updates.UpdateElectionDifficulty
    fromProto Proto.UPDATE_EURO_PER_ENERGY = return Updates.UpdateEuroPerEnergy
    fromProto Proto.UPDATE_MICRO_CCD_PER_EURO = return Updates.UpdateMicroGTUPerEuro
    fromProto Proto.UPDATE_FOUNDATION_ACCOUNT = return Updates.UpdateFoundationAccount
    fromProto Proto.UPDATE_MINT_DISTRIBUTION = return Updates.UpdateMintDistribution
    fromProto Proto.UPDATE_TRANSACTION_FEE_DISTRIBUTION = return Updates.UpdateTransactionFeeDistribution
    fromProto Proto.UPDATE_GAS_REWARDS = return Updates.UpdateGASRewards
    fromProto Proto.UPDATE_POOL_PARAMETERS = return Updates.UpdatePoolParameters
    fromProto Proto.ADD_ANONYMITY_REVOKER = return Updates.UpdateAddAnonymityRevoker
    fromProto Proto.ADD_IDENTITY_PROVIDER = return Updates.UpdateAddIdentityProvider
    fromProto Proto.UPDATE_ROOT_KEYS = return Updates.UpdateRootKeys
    fromProto Proto.UPDATE_LEVEL1_KEYS = return Updates.UpdateLevel1Keys
    fromProto Proto.UPDATE_LEVEL2_KEYS = return Updates.UpdateLevel2Keys
    fromProto Proto.UPDATE_COOLDOWN_PARAMETERS = return Updates.UpdateCooldownParameters
    fromProto Proto.UPDATE_TIME_PARAMETERS = return Updates.UpdateTimeParameters
    fromProto ProtoFields.UPDATE_TIMEOUT_PARAMETERS = return Updates.UpdateTimeoutParameters
    fromProto ProtoFields.UPDATE_MIN_BLOCK_TIME = return Updates.UpdateMinBlockTime
    fromProto ProtoFields.UPDATE_BLOCK_ENERGY_LIMIT = return Updates.UpdateBlockEnergyLimit
    fromProto ProtoFields.UPDATE_FINALIZATION_COMMITTEE_PARAMETERS = return Updates.UpdateFinalizationCommitteeParameters
    fromProto ProtoFields.UPDATE_VALIDATOR_SCORE_PARAMETERS = return Updates.UpdateValidatorScoreParameters
    fromProto ProtoFields.UPDATE_CREATE_PLT = return Updates.UpdateCreatePLT
    fromProto (Proto.UpdateType'Unrecognized variant) =
        fromProtoFail $
            "Unable to convert 'InvokeInstanceResponse': "
                <> "Got unknown variant: '"
                <> show variant
                <> "'."

instance FromProto Proto.TransactionType where
    type Output Proto.TransactionType = TransactionType
    fromProto Proto.DEPLOY_MODULE = return TTDeployModule
    fromProto Proto.INIT_CONTRACT = return TTInitContract
    fromProto Proto.UPDATE = return TTUpdate
    fromProto Proto.TRANSFER = return TTTransfer
    fromProto Proto.ADD_BAKER = return TTAddBaker
    fromProto Proto.REMOVE_BAKER = return TTRemoveBaker
    fromProto Proto.UPDATE_BAKER_STAKE = return TTUpdateBakerStake
    fromProto Proto.UPDATE_BAKER_RESTAKE_EARNINGS = return TTUpdateBakerRestakeEarnings
    fromProto Proto.UPDATE_BAKER_KEYS = return TTUpdateBakerKeys
    fromProto Proto.UPDATE_CREDENTIAL_KEYS = return TTUpdateCredentialKeys
    fromProto Proto.ENCRYPTED_AMOUNT_TRANSFER = return TTEncryptedAmountTransfer
    fromProto Proto.TRANSFER_TO_ENCRYPTED = return TTTransferToEncrypted
    fromProto Proto.TRANSFER_TO_PUBLIC = return TTTransferToPublic
    fromProto Proto.TRANSFER_WITH_SCHEDULE = return TTTransferWithSchedule
    fromProto Proto.UPDATE_CREDENTIALS = return TTUpdateCredentials
    fromProto Proto.REGISTER_DATA = return TTRegisterData
    fromProto Proto.TRANSFER_WITH_MEMO = return TTTransferWithMemo
    fromProto Proto.ENCRYPTED_AMOUNT_TRANSFER_WITH_MEMO = return TTEncryptedAmountTransferWithMemo
    fromProto Proto.TRANSFER_WITH_SCHEDULE_AND_MEMO = return TTTransferWithScheduleAndMemo
    fromProto Proto.CONFIGURE_BAKER = return TTConfigureBaker
    fromProto Proto.CONFIGURE_DELEGATION = return TTConfigureDelegation
    fromProto Proto.TOKEN_HOLDER = return TTTokenHolder
    fromProto (ProtoFields.TransactionType'Unrecognized variant) =
        fromProtoFail $
            "Unable to convert 'InvokeInstanceResponse': "
                <> "Got unknown variant: '"
                <> show variant
                <> "'."

instance FromProto Proto.AccountCreationDetails where
    type Output Proto.AccountCreationDetails = SupplementedValidResult
    fromProto acDetails = do
        ecdAccount <- fromProto $ acDetails ^. ProtoFields.address
        ecdRegId <- fmap fst . fromProto $ acDetails ^. ProtoFields.regId
        let vrEvents = [AccountCreated ecdAccount, CredentialDeployed{..}]
        return TxSuccess{..}

instance FromProto Proto.BakerEvent'BakerAdded where
    type Output Proto.BakerEvent'BakerAdded = BakerAddedEvent
    fromProto baEvent = do
        keysEvent <- fromProto $ baEvent ^. ProtoFields.keysEvent
        stake <- fromProto $ baEvent ^. ProtoFields.stake
        let restakeEarnings = baEvent ^. ProtoFields.restakeEarnings
        return (keysEvent, stake, restakeEarnings)

instance FromProto Proto.BakerKeysEvent where
    type Output Proto.BakerKeysEvent = BakerKeysEvent
    fromProto bkEvent = do
        bakerId <- fromProto $ bkEvent ^. ProtoFields.bakerId
        addr <- fromProto $ bkEvent ^. ProtoFields.account
        signKey <- fromProto $ bkEvent ^. ProtoFields.signKey
        electionKey <- fromProto $ bkEvent ^. ProtoFields.electionKey
        aggregationKey <- fromProto $ bkEvent ^. ProtoFields.aggregationKey
        return (bakerId, addr, signKey, electionKey, aggregationKey)

instance FromProto Proto.RegisteredData where
    type Output Proto.RegisteredData = RegisteredData
    fromProto rData = return . RegisteredData . BSS.toShort $ rData ^. ProtoFields.value

instance FromProto Proto.NewRelease where
    type Output Proto.NewRelease = (Timestamp, Amount)
    fromProto nRelease = do
        tStamp <- fromProto $ nRelease ^. ProtoFields.timestamp
        amount <- fromProto $ nRelease ^. ProtoFields.amount
        return (tStamp, amount)

instance FromProto ProtoKernel.Memo where
    type Output ProtoKernel.Memo = Memo
    fromProto memo = return . Memo . BSS.toShort $ memo ^. ProtoFields.value

instance FromProto Proto.ArInfo'ArIdentity where
    type Output Proto.ArInfo'ArIdentity = ArIdentity
    fromProto arIdentity = do
        let arId = deMkWord32 arIdentity
        -- The ID cannot be 0.
        if arId == 0
            then fromProtoFail $ "Unable to convert 'ArInfo'ArIdentity'. AR ID was 0."
            else return $ ArIdentity arId

instance FromProto Proto.IpIdentity where
    type Output Proto.IpIdentity = IdentityProviderIdentity
    fromProto = return . deMkWord32

instance FromProto Proto.ArInfo where
    type Output Proto.ArInfo = ArInfo.ArInfo
    fromProto arInfo = do
        arIdentity <- fromProto $ arInfo ^. ProtoFields.identity
        let arPubKey = arInfo ^. ProtoFields.publicKey . ProtoFields.value
        let arD = arInfo ^. ProtoFields.description
        let arName = arD ^. ProtoFields.name
        let arUrl = arD ^. ProtoFields.url
        let arDescription = arD ^. ProtoFields.description
        case createArInfo arIdentity arPubKey arName arUrl arDescription of
            Nothing ->
                fromProtoFail $
                    "Unable to convert 'ArInfo'. Could not create foreign instance "
                        <> "due to malformed payload data."
            Just v -> return v
instance FromProto Proto.IpInfo where
    type Output Proto.IpInfo = IpInfo.IpInfo
    fromProto ipInfo = do
        let ipVerifyKey = ipInfo ^. ProtoFields.verifyKey . ProtoFields.value
        let ipCdiVerifyKey = ipInfo ^. ProtoFields.cdiVerifyKey . ProtoFields.value
        let ipD = ipInfo ^. ProtoFields.description
        ipIdentity <- fromProto $ ipInfo ^. ProtoFields.identity
        let ipName = ipD ^. ProtoFields.name
        let ipUrl = ipD ^. ProtoFields.url
        let ipDescription = ipD ^. ProtoFields.description
        case createIpInfo ipIdentity ipVerifyKey ipCdiVerifyKey ipName ipUrl ipDescription of
            Nothing ->
                fromProtoFail $
                    "Unable to convert 'IpInfo'. Could not create foreign instance "
                        <> "due to malformed payload data."
            Just v -> return v

instance FromProto Proto.BakerStakeThreshold where
    type Output Proto.BakerStakeThreshold = Parameters.PoolParameters 'ChainParametersV0
    fromProto pParams = fmap Parameters.PoolParametersV0 . fromProto $ pParams ^. ProtoFields.bakerStakeThreshold

instance FromProto Proto.DurationSeconds where
    type Output Proto.DurationSeconds = DurationSeconds
    fromProto = return . deMkWord64

instance FromProto Proto.CooldownParametersCpv1 where
    type Output Proto.CooldownParametersCpv1 = Parameters.CooldownParameters 'ChainParametersV1
    fromProto cdParams = do
        _cpPoolOwnerCooldown <- fromProto $ cdParams ^. ProtoFields.poolOwnerCooldown
        _cpDelegatorCooldown <- fromProto $ cdParams ^. ProtoFields.delegatorCooldown
        return Parameters.CooldownParametersV1{..}

instance FromProto Proto.TimeParametersCpv1 where
    type Output Proto.TimeParametersCpv1 = Parameters.TimeParameters
    fromProto tParams = do
        _tpRewardPeriodLength <- fromProto $ tParams ^. ProtoFields.rewardPeriodLength
        _tpMintPerPayday <- fromProto $ tParams ^. ProtoFields.mintPerPayday
        return Parameters.TimeParametersV1{..}

instance FromProto Proto.RewardPeriodLength where
    type Output Proto.RewardPeriodLength = RewardPeriodLength
    fromProto rpl = do
        let t = rpl ^. ProtoFields.value
        return $ deMkWord64 t

instance FromProto Proto.Ratio where
    type Output Proto.Ratio = (Ratio.Ratio Word64)
    fromProto ratio = do
        let numerator = ratio ^. ProtoFields.numerator
        let denominator = ratio ^. ProtoFields.denominator
        -- Ensure we do not divide by zero.
        if denominator == 0
            then fromProtoFail "Unable to convert 'Ratio'. Denominator was 0."
            else return $ numerator Ratio.% denominator

instance FromProto Proto.ExchangeRate where
    type Output Proto.ExchangeRate = ExchangeRate
    fromProto er = fmap ExchangeRate . fromProto $ er ^. ProtoFields.value

instance FromProto Proto.GasRewards where
    type Output Proto.GasRewards = Parameters.GASRewards (Parameters.GasRewardsVersionFor 'ChainParametersV0)
    fromProto gRewards = do
        _gasBaker <- fromProto $ gRewards ^. ProtoFields.baker
        _gasFinalizationProof <- fmap Parameters.CTrue $ fromProto $ gRewards ^. ProtoFields.finalizationProof
        _gasAccountCreation <- fromProto $ gRewards ^. ProtoFields.accountCreation
        _gasChainUpdate <- fromProto $ gRewards ^. ProtoFields.chainUpdate
        return Parameters.GASRewards{..}

instance FromProto Proto.GasRewardsCpv2 where
    type Output Proto.GasRewardsCpv2 = Parameters.GASRewards (Parameters.GasRewardsVersionFor 'ChainParametersV2)
    fromProto gRewards = do
        _gasBaker <- fromProto $ gRewards ^. ProtoFields.baker
        let _gasFinalizationProof = Parameters.CFalse
        _gasAccountCreation <- fromProto $ gRewards ^. ProtoFields.accountCreation
        _gasChainUpdate <- fromProto $ gRewards ^. ProtoFields.chainUpdate
        return Parameters.GASRewards{..}

instance FromProto Proto.AccessStructure where
    type Output Proto.AccessStructure = Updates.AccessStructure
    fromProto aStructure = do
        accessPublicKeys <-
            Set.fromList
                <$> mapM fromProtoUpdateKeysIndex (aStructure ^. ProtoFields.accessPublicKeys)
        accessThreshold <- fromProto $ aStructure ^. ProtoFields.accessThreshold
        return Updates.AccessStructure{..}
      where
        fromProtoUpdateKeysIndex i = do
            let i' = i ^. ProtoFields.value
            if i' > fromIntegral (maxBound :: Word16)
                then
                    fromProtoFail $
                        "Unable to convert 'AccessStructure'. Index exceeds "
                            <> show (maxBound :: Word16)
                            <> "."
                else return $ fromIntegral i'

instance FromProto Proto.AuthorizationsV0 where
    type Output Proto.AuthorizationsV0 = Updates.Authorizations 'Parameters.AuthorizationsVersion0
    fromProto auth = do
        asKeys <- Vec.fromList <$> mapM fromProto (auth ^. ProtoFields.keys)
        asEmergency <- fromProto $ auth ^. ProtoFields.emergency
        asProtocol <- fromProto $ auth ^. ProtoFields.protocol
        asParamConsensusParameters <- fromProto $ auth ^. ProtoFields.parameterConsensus
        asParamEuroPerEnergy <- fromProto $ auth ^. ProtoFields.parameterEuroPerEnergy
        asParamMicroGTUPerEuro <- fromProto $ auth ^. ProtoFields.parameterMicroCCDPerEuro
        asParamFoundationAccount <- fromProto $ auth ^. ProtoFields.parameterFoundationAccount
        asParamMintDistribution <- fromProto $ auth ^. ProtoFields.parameterMintDistribution
        asParamTransactionFeeDistribution <- fromProto $ auth ^. ProtoFields.parameterTransactionFeeDistribution
        asParamGASRewards <- fromProto $ auth ^. ProtoFields.parameterGasRewards
        asPoolParameters <- fromProto $ auth ^. ProtoFields.poolParameters
        asAddAnonymityRevoker <- fromProto $ auth ^. ProtoFields.addAnonymityRevoker
        asAddIdentityProvider <- fromProto $ auth ^. ProtoFields.addIdentityProvider
        let asCooldownParameters = Parameters.CFalse
        let asTimeParameters = Parameters.CFalse
        return Updates.Authorizations{..}

instance FromProto Proto.AuthorizationsV1 where
    type Output Proto.AuthorizationsV1 = Updates.Authorizations 'Parameters.AuthorizationsVersion1
    fromProto auth = do
        let v0 = auth ^. ProtoFields.v0
        asKeys <- Vec.fromList <$> mapM fromProto (v0 ^. ProtoFields.keys)
        asEmergency <- fromProto $ v0 ^. ProtoFields.emergency
        asProtocol <- fromProto $ v0 ^. ProtoFields.protocol
        asParamConsensusParameters <- fromProto $ v0 ^. ProtoFields.parameterConsensus
        asParamEuroPerEnergy <- fromProto $ v0 ^. ProtoFields.parameterEuroPerEnergy
        asParamMicroGTUPerEuro <- fromProto $ v0 ^. ProtoFields.parameterMicroCCDPerEuro
        asParamFoundationAccount <- fromProto $ v0 ^. ProtoFields.parameterFoundationAccount
        asParamMintDistribution <- fromProto $ v0 ^. ProtoFields.parameterMintDistribution
        asParamTransactionFeeDistribution <- fromProto $ v0 ^. ProtoFields.parameterTransactionFeeDistribution
        asParamGASRewards <- fromProto $ v0 ^. ProtoFields.parameterGasRewards
        asPoolParameters <- fromProto $ v0 ^. ProtoFields.poolParameters
        asAddAnonymityRevoker <- fromProto $ v0 ^. ProtoFields.addAnonymityRevoker
        asAddIdentityProvider <- fromProto $ v0 ^. ProtoFields.addIdentityProvider
        asCooldownParameters <- fmap Parameters.CTrue $ fromProto $ auth ^. ProtoFields.parameterCooldown
        asTimeParameters <- fmap Parameters.CTrue $ fromProto $ auth ^. ProtoFields.parameterTime
        return Updates.Authorizations{..}

instance FromProto Proto.Level1Update where
    type Output Proto.Level1Update = Updates.Level1Update
    fromProto l1Update = do
        u <- case l1Update ^. Proto.maybe'updateType of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'Level1Update' due to missing field in response payload."
            Just v -> return v
        case u of
            ProtoFields.Level1Update'Level1KeysUpdate l1kUpdate -> do
                l1kl1uKeys <- fst <$> fromProto l1kUpdate
                return Updates.Level1KeysLevel1Update{..}
            ProtoFields.Level1Update'Level2KeysUpdateV0 l2kUpdateV0 -> do
                l2kl1uAuthorizations <- fromProto l2kUpdateV0
                return Updates.Level2KeysLevel1Update{..}
            ProtoFields.Level1Update'Level2KeysUpdateV1 l2kUpdateV1 -> do
                l2kl1uAuthorizationsV1 <- fromProto l2kUpdateV1
                return Updates.Level2KeysLevel1UpdateV1{..}

instance FromProto Proto.UpdatePublicKey where
    type Output Proto.UpdatePublicKey = Updates.UpdatePublicKey
    fromProto key = do
        let keyBytes = key ^. ProtoFields.value
        case S.decode keyBytes of
            Left err -> fromProtoFail $ "Unable to convert 'UpdatePublicKey': " <> err
            Right decoded -> return $ VerifyKeyEd25519 decoded

instance FromProto Proto.UpdateKeysThreshold where
    type Output Proto.UpdateKeysThreshold = Updates.UpdateKeysThreshold
    fromProto ukTreshold = do
        -- Ensure that the value fits into a Word16.
        treshold <- do
            case deMkWord16 ukTreshold of
                Left err ->
                    fromProtoFail $
                        "Unable to convert 'UpdateKeysThreshold': " <> err
                Right v -> return v
        return $ Updates.UpdateKeysThreshold treshold

instance FromProto Proto.RootUpdate where
    type Output Proto.RootUpdate = Updates.RootUpdate
    fromProto rUpdate = do
        ru <- case rUpdate ^. Proto.maybe'updateType of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'RootUpdate' due to missing field in response payload."
            Just v -> return v
        case ru of
            ProtoFields.RootUpdate'RootKeysUpdate _ ->
                fmap (Updates.RootKeysRootUpdate . snd)
                    . fromProto
                    $ rUpdate ^. ProtoFields.rootKeysUpdate
            ProtoFields.RootUpdate'Level1KeysUpdate _ ->
                fmap (Updates.Level1KeysRootUpdate . fst)
                    . fromProto
                    $ rUpdate ^. ProtoFields.level1KeysUpdate
            ProtoFields.RootUpdate'Level2KeysUpdateV0 _ ->
                fmap Updates.Level2KeysRootUpdate
                    . fromProto
                    $ rUpdate ^. ProtoFields.level2KeysUpdateV0
            ProtoFields.RootUpdate'Level2KeysUpdateV1 _ ->
                fmap Updates.Level2KeysRootUpdateV1
                    . fromProto
                    $ rUpdate ^. ProtoFields.level2KeysUpdateV1

instance FromProto Proto.HigherLevelKeys where
    type
        Output Proto.HigherLevelKeys =
            ( Updates.HigherLevelKeys Updates.Level1KeysKind,
              Updates.HigherLevelKeys Updates.RootKeysKind
            )
    fromProto keys = do
        hlkKeys <- Vec.fromList <$> mapM fromProto (keys ^. ProtoFields.keys)
        hlkThreshold <- fromProto $ keys ^. ProtoFields.threshold
        return (Updates.HigherLevelKeys{..}, Updates.HigherLevelKeys{..})

instance FromProto Proto.MintDistributionCpv0 where
    type Output Proto.MintDistributionCpv0 = Parameters.MintDistribution (Parameters.MintDistributionVersionFor 'ChainParametersV0)
    fromProto mDistribution = do
        _mdBakingReward <- fromProto $ mDistribution ^. ProtoFields.bakingReward
        _mdFinalizationReward <- fromProto $ mDistribution ^. ProtoFields.finalizationReward
        -- Ensure that @_mdBakingReward _mdFinalizationReward <= 1@
        let atMostOne = isJust $ addAmountFraction _mdBakingReward _mdFinalizationReward
        unless
            atMostOne
            (fromProtoFail "Unable to convert 'MintDistributionCpv0': Sum of baking and finalization reward ratios exceeds 1.")
        _mdMintPerSlot <- fmap Parameters.CTrue . fromProto $ mDistribution ^. ProtoFields.mintPerSlot
        return Parameters.MintDistribution{..}

instance FromProto Proto.MintDistributionCpv1 where
    type Output Proto.MintDistributionCpv1 = Parameters.MintDistribution (Parameters.MintDistributionVersionFor 'ChainParametersV1)
    fromProto mDistribution = do
        _mdBakingReward <- fromProto $ mDistribution ^. ProtoFields.bakingReward
        _mdFinalizationReward <- fromProto $ mDistribution ^. ProtoFields.finalizationReward
        -- Ensure that @_mdBakingReward _mdFinalizationReward <= 1@
        let atMostOne = isJust $ addAmountFraction _mdBakingReward _mdFinalizationReward
        unless
            atMostOne
            (fromProtoFail "Unable to convert 'MintDistributionCpv0': Sum of baking and finalization reward ratios exceeds 1.")
        let _mdMintPerSlot = Parameters.CFalse
        return Parameters.MintDistribution{..}

instance FromProto Proto.InclusiveRangeAmountFraction where
    type Output Proto.InclusiveRangeAmountFraction = Parameters.InclusiveRange AmountFraction
    fromProto iRange = do
        irMin <- fromProto $ iRange ^. ProtoFields.min
        irMax <- fromProto $ iRange ^. ProtoFields.max
        return Parameters.InclusiveRange{..}

instance FromProto Proto.CapitalBound where
    type Output Proto.CapitalBound = Parameters.CapitalBound
    fromProto cBound = fmap Parameters.CapitalBound . fromProto $ cBound ^. ProtoFields.value

instance FromProto Proto.LeverageFactor where
    type Output Proto.LeverageFactor = Parameters.LeverageFactor
    fromProto lFactor = fmap Parameters.LeverageFactor . fromProto $ lFactor ^. ProtoFields.value

instance FromProto Proto.PoolParametersCpv1 where
    type Output Proto.PoolParametersCpv1 = (Parameters.PoolParameters 'ChainParametersV1)
    fromProto pParams = do
        _ppPassiveCommissions <- do
            _finalizationCommission <- fromProto $ pParams ^. ProtoFields.passiveFinalizationCommission
            _bakingCommission <- fromProto $ pParams ^. ProtoFields.passiveBakingCommission
            _transactionCommission <- fromProto $ pParams ^. ProtoFields.passiveTransactionCommission
            return CommissionRates{..}
        _ppCommissionBounds <- do
            let cBounds = pParams ^. ProtoFields.commissionBounds
            _finalizationCommissionRange <- fromProto $ cBounds ^. ProtoFields.finalization
            _bakingCommissionRange <- fromProto $ cBounds ^. ProtoFields.baking
            _transactionCommissionRange <- fromProto $ cBounds ^. ProtoFields.transaction
            return Parameters.CommissionRanges{..}
        _ppMinimumEquityCapital <- fromProto $ pParams ^. ProtoFields.minimumEquityCapital
        _ppCapitalBound <- fromProto $ pParams ^. ProtoFields.capitalBound
        _ppLeverageBound <- fromProto $ pParams ^. ProtoFields.leverageBound
        return Parameters.PoolParametersV1{..}

instance FromProto Proto.Sha256Hash where
    type Output Proto.Sha256Hash = Hash
    fromProto h =
        case deMkSerialize h of
            Left err ->
                fromProtoFail $
                    "Unable to convert 'Sha256Hash': " <> err
            Right hash -> return hash

instance FromProto Proto.ProtocolUpdate where
    type Output Proto.ProtocolUpdate = Updates.ProtocolUpdate
    fromProto pUpdate = do
        let puMessage = pUpdate ^. ProtoFields.message
        let puSpecificationURL = pUpdate ^. ProtoFields.specificationUrl
        puSpecificationHash <- fromProto $ pUpdate ^. ProtoFields.specificationHash
        let puSpecificationAuxiliaryData = pUpdate ^. ProtoFields.specificationAuxiliaryData
        return Updates.ProtocolUpdate{..}

instance FromProto ProtoPLT.CBor where
    type Output ProtoPLT.CBor = TokenParameter
    fromProto protoCbor = do
        let bs = protoCbor ^. PLTFields.value
        pure $ TokenParameter (BSS.toShort bs)

instance FromProto ProtoPLT.TokenModuleRef where
    type Output ProtoPLT.TokenModuleRef = TokenModuleRef
    fromProto th =
        case deMkSerialize th of
            Left err -> fromProtoFail $ "Unable to convert 'TokenModuleRef': " <> err
            Right v -> return (TokenModuleRef v)

instance FromProto ProtoPLT.CreatePLT where
    type Output ProtoPLT.CreatePLT = CreatePLT
    fromProto cpUpdate = do
        _cpltTokenSymbol <- fromProto $ cpUpdate ^. PLTFields.tokenSymbol
        _cpltGovernanceAccount <- fromProto $ cpUpdate ^. PLTFields.governanceAccount
        _cpltDecimals <- case toIntegralSized (cpUpdate ^. PLTFields.decimals) of
            Nothing -> fromProtoFail $ "CreatePLT: decimals out of range"
            Just converted -> return converted
        _cpltTokenModule <- fromProto $ cpUpdate ^. PLTFields.tokenModule
        _cpltInitializationParameters <- fromProto $ cpUpdate ^. PLTFields.initializationParameters
        return CreatePLT{..}

instance FromProto Proto.TransactionFeeDistribution where
    type Output Proto.TransactionFeeDistribution = Parameters.TransactionFeeDistribution
    fromProto tfDistribution = do
        _tfdBaker <- fromProto $ tfDistribution ^. ProtoFields.baker
        _tfdGASAccount <- fromProto $ tfDistribution ^. ProtoFields.gasAccount
        return Parameters.TransactionFeeDistribution{..}

instance FromProto Proto.TimeoutParameters where
    type Output Proto.TimeoutParameters = Parameters.TimeoutParameters
    fromProto tParams = do
        _tpTimeoutBase <- fromProto $ tParams ^. ProtoFields.timeoutBase
        _tpTimeoutIncrease <- do
            toIncrease <- fromProto $ tParams ^. ProtoFields.timeoutIncrease
            unless
                (Ratio.numerator toIncrease > Ratio.denominator toIncrease)
                (fromProtoFail "Unable to convert 'TimeoutParameters', 'toIncrease' must be greater than 1.")
            return toIncrease
        _tpTimeoutDecrease <- do
            toDecrease <- fromProto $ tParams ^. ProtoFields.timeoutDecrease
            unless
                (Ratio.numerator toDecrease <= Ratio.denominator toDecrease)
                (fromProtoFail "Unable to convert 'TimeoutParameters', 'toDecrease' must be at most 1.")
            return toDecrease
        return Parameters.TimeoutParameters{..}

instance FromProto Proto.UpdatePayload where
    type Output Proto.UpdatePayload = Updates.UpdatePayload
    fromProto uPayload = do
        pl <- case uPayload ^. Proto.maybe'payload of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'UpdatePayload' due to missing field in response payload."
            Just v -> return v
        case pl of
            ProtoFields.UpdatePayload'AddAnonymityRevokerUpdate aarUpdate -> do
                ai <- fromProto aarUpdate
                return $ Updates.AddAnonymityRevokerUpdatePayload ai
            ProtoFields.UpdatePayload'AddIdentityProviderUpdate aipUpdate -> do
                ip <- fromProto aipUpdate
                return $ Updates.AddIdentityProviderUpdatePayload ip
            ProtoFields.UpdatePayload'BakerStakeThresholdUpdate bstUpdate -> do
                pp <- fromProto bstUpdate
                return $ Updates.BakerStakeThresholdUpdatePayload pp
            ProtoFields.UpdatePayload'CooldownParametersCpv1Update cdpc1Update -> do
                cp <- fromProto cdpc1Update
                return $ Updates.CooldownParametersCPV1UpdatePayload cp
            ProtoFields.UpdatePayload'ElectionDifficultyUpdate edUpdate -> do
                ed <- fromProto edUpdate
                return $ Updates.ElectionDifficultyUpdatePayload ed
            ProtoFields.UpdatePayload'EuroPerEnergyUpdate epeUpdate -> do
                er <- fromProto epeUpdate
                return $ Updates.EuroPerEnergyUpdatePayload er
            ProtoFields.UpdatePayload'FoundationAccountUpdate faUpdate -> do
                addr <- fromProto faUpdate
                return $ Updates.FoundationAccountUpdatePayload addr
            ProtoFields.UpdatePayload'GasRewardsUpdate grUpdate -> do
                gr <- fromProto grUpdate
                return $ Updates.GASRewardsUpdatePayload gr
            ProtoFields.UpdatePayload'GasRewardsCpv2Update grUpdate -> do
                gr <- fromProto grUpdate
                return $ Updates.GASRewardsCPV2UpdatePayload gr
            ProtoFields.UpdatePayload'Level1Update l1Update -> do
                u <- case l1Update ^. Proto.maybe'updateType of
                    Nothing ->
                        fromProtoFail
                            "Unable to convert 'UpdatePayload'Level1Update' due to missing field in response payload."
                    Just v -> return v
                case u of
                    ProtoFields.Level1Update'Level1KeysUpdate l1kUpdate -> do
                        l1kl1uKeys <- fst <$> fromProto l1kUpdate
                        return $ Updates.Level1UpdatePayload Updates.Level1KeysLevel1Update{..}
                    ProtoFields.Level1Update'Level2KeysUpdateV0 l2kUpdateV0 -> do
                        l2kl1uAuthorizations <- fromProto l2kUpdateV0
                        return $ Updates.Level1UpdatePayload Updates.Level2KeysLevel1Update{..}
                    ProtoFields.Level1Update'Level2KeysUpdateV1 l2kUpdateV1 -> do
                        l2kl1uAuthorizationsV1 <- fromProto l2kUpdateV1
                        return $ Updates.Level1UpdatePayload Updates.Level2KeysLevel1UpdateV1{..}
            ProtoFields.UpdatePayload'MicroCcdPerEuroUpdate mcpeUpdate -> do
                er <- fromProto mcpeUpdate
                return $ Updates.MicroGTUPerEuroUpdatePayload er
            ProtoFields.UpdatePayload'MintDistributionUpdate mdUpdate -> do
                md <- fromProto mdUpdate
                return $ Updates.MintDistributionUpdatePayload md
            ProtoFields.UpdatePayload'MintDistributionCpv1Update mdcv1Update -> do
                md <- fromProto mdcv1Update
                return $ Updates.MintDistributionCPV1UpdatePayload md
            ProtoFields.UpdatePayload'PoolParametersCpv1Update ppcv1Update -> do
                pp <- fromProto ppcv1Update
                return $ Updates.PoolParametersCPV1UpdatePayload pp
            ProtoFields.UpdatePayload'ProtocolUpdate pUpdate -> do
                pu <- fromProto pUpdate
                return $ Updates.ProtocolUpdatePayload pu
            ProtoFields.UpdatePayload'RootUpdate rUpdate -> do
                update <- fromProto rUpdate
                return $ Updates.RootUpdatePayload update
            ProtoFields.UpdatePayload'TimeParametersCpv1Update tpcv1Update -> do
                tp <- fromProto tpcv1Update
                return $ Updates.TimeParametersCPV1UpdatePayload tp
            ProtoFields.UpdatePayload'TransactionFeeDistributionUpdate tfdUpdate -> do
                tfd <- fromProto tfdUpdate
                return $ Updates.TransactionFeeDistributionUpdatePayload tfd
            ProtoFields.UpdatePayload'TimeoutParametersUpdate tpUpdate -> do
                tp <- fromProto tpUpdate
                return $ Updates.TimeoutParametersUpdatePayload tp
            ProtoFields.UpdatePayload'MinBlockTimeUpdate mbtUpdate -> do
                mbt <- fromProto mbtUpdate
                return $ Updates.MinBlockTimeUpdatePayload mbt
            ProtoFields.UpdatePayload'BlockEnergyLimitUpdate belUpdate -> do
                bel <- fromProto belUpdate
                return $ Updates.BlockEnergyLimitUpdatePayload bel
            ProtoFields.UpdatePayload'FinalizationCommitteeParametersUpdate fcpUpdate -> do
                fcp <- fromProto fcpUpdate
                return $ Updates.FinalizationCommitteeParametersUpdatePayload fcp
            ProtoFields.UpdatePayload'ValidatorScoreParametersUpdate vspUpdate -> do
                vsp <- fromProto vspUpdate
                return $ Updates.ValidatorScoreParametersUpdatePayload vsp
            ProtoFields.UpdatePayload'CreatePltUpdate cpUpdate -> do
                cp <- fromProto cpUpdate
                return $ Updates.CreatePLTUpdatePayload cp

instance FromProto Proto.BlockItemSummary where
    type Output Proto.BlockItemSummary = SupplementedTransactionSummary
    fromProto biSummary = do
        -- Common block item summary fields
        let tsIndex = deMkWord64 $ biSummary ^. ProtoFields.index
        tsEnergyCost <- fromProto $ biSummary ^. ProtoFields.energyCost
        tsHash <- fromProto $ biSummary ^. ProtoFields.hash
        -- Discern between transactions
        bis <- case biSummary ^. Proto.maybe'details of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'BlockItemSummary' due to missing field in response payload."
            Just v -> return v
        case bis of
            -- Account creation
            ProtoFields.BlockItemSummary'AccountCreation aCreation -> do
                let tsSender = Nothing
                let tsCost = Amount 0
                credType <- fromProto $ aCreation ^. ProtoFields.credentialType
                let tsType = TSTCredentialDeploymentTransaction credType
                tsResult <- fromProto aCreation
                return TransactionSummary{..}
            -- Account transaction
            ProtoFields.BlockItemSummary'AccountTransaction aTransaction -> do
                let sender = aTransaction ^. ProtoFields.sender
                tsSender <- Just <$> fromProto sender
                tsCost <- fromProto $ aTransaction ^. ProtoFields.cost
                (tType, tsResult) <- fromProto aTransaction
                let tsType = TSTAccountTransaction tType
                return TransactionSummary{..}
            ProtoFields.BlockItemSummary'Update update -> do
                let tsSender = Nothing
                let tsCost = Amount 0
                (tType, tsResult) <- do
                    ueEffectiveTime <- fromProto $ update ^. ProtoFields.effectiveTime
                    uePayload <- fromProto $ update ^. ProtoFields.payload
                    let ueType = Updates.updateType uePayload
                    return (ueType, TxSuccess [UpdateEnqueued{..}])
                let tsType = TSTUpdateTransaction tType
                return TransactionSummary{..}

instance FromProto Proto.AccountTransactionDetails where
    type Output Proto.AccountTransactionDetails = (Maybe TransactionType, SupplementedValidResult)
    fromProto atDetails = do
        let senderAcc = atDetails ^. ProtoFields.sender
        sender <- fromProto senderAcc
        ate <- case atDetails ^. Proto.effects . Proto.maybe'effect of
            Nothing -> fromProtoFail "Unable to convert 'AccountTransactionDetails'."
            Just v -> return v
        case ate of
            ProtoFields.AccountTransactionEffects'AccountTransfer' aTransfer -> do
                etAmount <- fromProto $ aTransfer ^. ProtoFields.amount
                receiver <- fromProto $ aTransfer ^. ProtoFields.receiver
                let etTo = AddressAccount receiver
                let etFrom = AddressAccount sender
                memo <- fromProtoMaybe $ aTransfer ^. ProtoFields.maybe'memo
                -- Discern between event types based on whether a memo is present.
                let (tType, vrEvents) = case memo of
                        Nothing ->
                            (TTTransfer, [Transferred{..}])
                        Just tmMemo ->
                            (TTTransferWithMemo, [Transferred{..}, TransferMemo{..}])
                return (Just tType, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'BakerAdded bAdded -> do
                (kEvent, ebaStake, ebaRestakeEarnings) <- fromProto bAdded
                let (ebaBakerId, ebaAccount, ebaSignKey, ebaElectionKey, ebaAggregationKey) = kEvent
                return (Just TTAddBaker, TxSuccess [BakerAdded{..}])
            ProtoFields.AccountTransactionEffects'BakerConfigured' bConfigured -> do
                vrEvents <- mapM (\e -> fromProto (senderAcc, e)) $ bConfigured ^. ProtoFields.events
                return (Just TTConfigureBaker, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'BakerKeysUpdated bkUpdated -> do
                (ebkuBakerId, ebkuAccount, ebkuSignKey, ebkuElectionKey, ebkuAggregationKey) <- fromProto bkUpdated
                return (Just TTUpdateBakerKeys, TxSuccess [BakerKeysUpdated{..}])
            ProtoFields.AccountTransactionEffects'BakerRemoved bRemoved -> do
                let ebrAccount = sender
                ebrBakerId <- fromProto bRemoved
                return (Just TTRemoveBaker, TxSuccess [BakerRemoved{..}])
            ProtoFields.AccountTransactionEffects'BakerRestakeEarningsUpdated breUpdated -> do
                let ebsreAccount = sender
                ebsreBakerId <- fromProto $ breUpdated ^. ProtoFields.bakerId
                let ebsreRestakeEarnings = breUpdated ^. ProtoFields.restakeEarnings
                return (Just TTUpdateBakerRestakeEarnings, TxSuccess [BakerSetRestakeEarnings{..}])
            ProtoFields.AccountTransactionEffects'BakerStakeUpdated' bsUpdated -> do
                vrEvents <- case bsUpdated ^. ProtoFields.maybe'update of
                    Nothing -> return []
                    Just update -> do
                        let increased = update ^. ProtoFields.increased
                        let ebsiAccount = sender
                        ebsiBakerId <- fromProto $ update ^. ProtoFields.bakerId
                        ebsiNewStake <- fromProto $ update ^. ProtoFields.newStake
                        if increased
                            then return [BakerStakeIncreased{..}]
                            else return [BakerStakeDecreased{..}]
                return (Just TTUpdateBakerStake, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'ContractInitialized cInitialized -> do
                ecContractVersion <- fromProto $ cInitialized ^. ProtoFields.contractVersion
                ecRef <- fromProto $ cInitialized ^. ProtoFields.originRef
                ecAddress <- fromProto $ cInitialized ^. ProtoFields.address
                ecAmount <- fromProto $ cInitialized ^. ProtoFields.amount
                ecInitName <- fromProto $ cInitialized ^. ProtoFields.initName
                ecEvents <- mapM fromProto $ cInitialized ^. ProtoFields.events
                ecParameter <- fmap Parameters.CTrue . fromProto $ cInitialized ^. ProtoFields.parameter
                return (Just TTInitContract, TxSuccess [ContractInitialized{..}])
            ProtoFields.AccountTransactionEffects'ContractUpdateIssued' cuIssued -> do
                vrEvents <- mapM fromProto $ cuIssued ^. ProtoFields.effects
                return (Just TTUpdate, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'CredentialKeysUpdated ckUpdated -> do
                ckuCredId <- fst <$> fromProto ckUpdated
                return (Just TTUpdateCredentialKeys, TxSuccess [CredentialKeysUpdated ckuCredId])
            ProtoFields.AccountTransactionEffects'CredentialsUpdated' cUpdated -> do
                let cuAccount = sender
                cuNewCredIds <- mapM (fmap fst . fromProto) $ cUpdated ^. ProtoFields.newCredIds
                cuRemovedCredIds <- mapM (fmap fst . fromProto) $ cUpdated ^. ProtoFields.removedCredIds
                cuNewThreshold <- fromProto $ cUpdated ^. ProtoFields.newThreshold
                return (Just TTUpdateCredentials, TxSuccess [CredentialsUpdated{..}])
            ProtoFields.AccountTransactionEffects'DataRegistered dRegistered -> do
                drData <- fromProto dRegistered
                return (Just TTRegisterData, TxSuccess [DataRegistered{..}])
            ProtoFields.AccountTransactionEffects'DelegationConfigured' dConfigured -> do
                vrEvents <- mapM (\e -> fromProto (senderAcc, e)) $ dConfigured ^. ProtoFields.events
                return (Just TTConfigureDelegation, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'EncryptedAmountTransferred' eaTransferred -> do
                -- Removed encrypted amounts.
                let removed = eaTransferred ^. ProtoFields.removed
                earAccount <- fromProto $ removed ^. ProtoFields.account
                earNewAmount <- fromProto $ removed ^. ProtoFields.newAmount
                earInputAmount <- fromProto $ removed ^. ProtoFields.inputAmount
                let earUpToIndex = EncryptedAmountAggIndex $ removed ^. ProtoFields.upToIndex
                -- Added encrypted amounts.
                let added = eaTransferred ^. ProtoFields.added
                neaAccount <- fromProto $ added ^. ProtoFields.receiver
                let neaNewIndex = EncryptedAmountIndex $ added ^. ProtoFields.newIndex
                neaEncryptedAmount <- fromProto $ added ^. ProtoFields.encryptedAmount
                -- The memo.
                memo <- fromProtoMaybe $ eaTransferred ^. ProtoFields.maybe'memo
                -- Discern between event types based on whether a memo is present.
                let (tType, vrEvents) = case memo of
                        Nothing ->
                            ( TTEncryptedAmountTransfer,
                              [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}]
                            )
                        Just tmMemo ->
                            ( TTEncryptedAmountTransferWithMemo,
                              [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}, TransferMemo{..}]
                            )
                return (Just tType, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'ModuleDeployed mDeployed -> do
                mRef <- fromProto mDeployed
                return (Just TTDeployModule, TxSuccess [ModuleDeployed mRef])
            ProtoFields.AccountTransactionEffects'None' none -> do
                vrRejectReason <- fromProto $ none ^. ProtoFields.rejectReason
                transactionType <- fromProtoMaybe $ none ^. ProtoFields.maybe'transactionType
                return (transactionType, TxReject{..})
            ProtoFields.AccountTransactionEffects'TransferredToEncrypted ttEncrypted -> do
                eaaAccount <- fromProto $ ttEncrypted ^. ProtoFields.account
                eaaNewAmount <- fromProto $ ttEncrypted ^. ProtoFields.newAmount
                eaaAmount <- fromProto $ ttEncrypted ^. ProtoFields.amount
                return (Just TTTransferToEncrypted, TxSuccess [EncryptedSelfAmountAdded{..}])
            ProtoFields.AccountTransactionEffects'TransferredToPublic' ttPublic -> do
                -- Amount added by decryption.
                aabdAmount <- fromProto $ ttPublic ^. ProtoFields.amount
                -- Removed encrypted amounts.
                let removed = ttPublic ^. ProtoFields.removed
                aabdAccount <- fromProto $ removed ^. ProtoFields.account
                earAccount <- fromProto $ removed ^. ProtoFields.account
                earNewAmount <- fromProto $ removed ^. ProtoFields.newAmount
                earInputAmount <- fromProto $ removed ^. ProtoFields.inputAmount
                let earUpToIndex = EncryptedAmountAggIndex $ removed ^. ProtoFields.upToIndex
                return (Just TTTransferToPublic, TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}])
            ProtoFields.AccountTransactionEffects'TransferredWithSchedule' twSchedule -> do
                let etwsFrom = sender
                etwsTo <- fromProto $ twSchedule ^. ProtoFields.receiver
                etwsAmount <- mapM fromProto $ twSchedule ^. ProtoFields.amount
                memo <- fromProtoMaybe $ twSchedule ^. ProtoFields.maybe'memo
                -- Discern between event types based on whether a memo is present.
                let (tType, vrEvents) = case memo of
                        Nothing ->
                            ( TTTransferWithSchedule,
                              [TransferredWithSchedule{..}]
                            )
                        Just tmMemo ->
                            ( TTTransferWithScheduleAndMemo,
                              [TransferredWithSchedule{..}, TransferMemo{..}]
                            )
                return (Just tType, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'TokenGovernanceEffect _pltTokenGovernanceEvent -> do
                fromProtoFail "TODO: Implement"
            ProtoFields.AccountTransactionEffects'TokenHolderEffect _pltTokenHolderEvent -> do
                fromProtoFail "TODO: Implement"

instance FromProto (ProtoKernel.AccountAddress, Proto.DelegationEvent) where
    type Output (ProtoKernel.AccountAddress, Proto.DelegationEvent) = SupplementedEvent
    fromProto (senderAcc, dEvent) = do
        sender <- fromProto senderAcc
        de <- case dEvent ^. Proto.maybe'event of
            Nothing ->
                fromProtoFail
                    "Unable to convert '(AccountAddress, DelegationEvent)'' due to missing field in response payload."
            Just v -> return v
        case de of
            Proto.DelegationEvent'DelegationAdded dAdded -> do
                let edaAccount = sender
                edaDelegatorId <- fromProto dAdded
                return DelegationAdded{..}
            ProtoFields.DelegationEvent'DelegationStakeIncreased' dsIncreased -> do
                let edsiAccount = sender
                edsiDelegatorId <- fromProto $ dsIncreased ^. ProtoFields.delegatorId
                edsiNewStake <- fromProto $ dsIncreased ^. ProtoFields.newStake
                return DelegationStakeIncreased{..}
            ProtoFields.DelegationEvent'DelegationStakeDecreased' dsDecreased -> do
                let edsdAccount = sender
                edsdDelegatorId <- fromProto $ dsDecreased ^. ProtoFields.delegatorId
                edsdNewStake <- fromProto $ dsDecreased ^. ProtoFields.newStake
                return DelegationStakeDecreased{..}
            ProtoFields.DelegationEvent'DelegationSetRestakeEarnings' dsrEarnings -> do
                let edsreAccount = sender
                edsreDelegatorId <- fromProto $ dsrEarnings ^. ProtoFields.delegatorId
                let edsreRestakeEarnings = dsrEarnings ^. ProtoFields.restakeEarnings
                return DelegationSetRestakeEarnings{..}
            ProtoFields.DelegationEvent'DelegationSetDelegationTarget' dsdTarget -> do
                let edsdtAccount = sender
                edsdtDelegatorId <- fromProto $ dsdTarget ^. ProtoFields.delegatorId
                edsdtDelegationTarget <- fromProto $ dsdTarget ^. ProtoFields.delegationTarget
                return DelegationSetDelegationTarget{..}
            ProtoFields.DelegationEvent'DelegationRemoved dRemoved -> do
                let edrAccount = sender
                edrDelegatorId <- fromProto dRemoved
                return DelegationRemoved{..}
            ProtoFields.DelegationEvent'BakerRemoved' bkrRemoved -> do
                let ebrAccount = sender
                ebrBakerId <- fromProto $ bkrRemoved ^. ProtoFields.bakerId
                return BakerRemoved{..}

instance FromProto (ProtoKernel.AccountAddress, Proto.BakerEvent) where
    type Output (ProtoKernel.AccountAddress, Proto.BakerEvent) = SupplementedEvent
    fromProto (senderAcc, bEvent) = do
        sender <- fromProto senderAcc
        be <- case bEvent ^. Proto.maybe'event of
            Nothing ->
                fromProtoFail
                    "Unable to convert '(AccountAddress, BakerEvent)' due to missing field in response payload."
            Just v -> return v
        case be of
            Proto.BakerEvent'BakerAdded' bAdded -> do
                (kEvent, ebaStake, ebaRestakeEarnings) <- fromProto bAdded
                let (ebaBakerId, ebaAccount, ebaSignKey, ebaElectionKey, ebaAggregationKey) = kEvent
                return BakerAdded{..}
            ProtoFields.BakerEvent'BakerRemoved bRemoved -> do
                let ebrAccount = sender
                ebrBakerId <- fromProto bRemoved
                return BakerRemoved{..}
            ProtoFields.BakerEvent'BakerStakeIncreased' bsIncreased -> do
                let ebsiAccount = sender
                ebsiBakerId <- fromProto $ bsIncreased ^. ProtoFields.bakerId
                ebsiNewStake <- fromProto $ bsIncreased ^. ProtoFields.newStake
                return BakerStakeIncreased{..}
            ProtoFields.BakerEvent'BakerStakeDecreased' bsDecreased -> do
                let ebsiAccount = sender
                ebsiBakerId <- fromProto $ bsDecreased ^. ProtoFields.bakerId
                ebsiNewStake <- fromProto $ bsDecreased ^. ProtoFields.newStake
                return BakerStakeDecreased{..}
            ProtoFields.BakerEvent'BakerRestakeEarningsUpdated' breUpdated -> do
                let ebsreAccount = sender
                ebsreBakerId <- fromProto $ breUpdated ^. ProtoFields.bakerId
                let ebsreRestakeEarnings = breUpdated ^. ProtoFields.restakeEarnings
                return BakerSetRestakeEarnings{..}
            ProtoFields.BakerEvent'BakerKeysUpdated bkUpdated -> do
                (ebkuBakerId, ebkuAccount, ebkuSignKey, ebkuElectionKey, ebkuAggregationKey) <- fromProto bkUpdated
                return BakerKeysUpdated{..}
            ProtoFields.BakerEvent'BakerSetOpenStatus' bsoStatus -> do
                let ebsosAccount = sender
                ebsosBakerId <- fromProto $ bsoStatus ^. ProtoFields.bakerId
                ebsosOpenStatus <- fromProto $ bsoStatus ^. ProtoFields.openStatus
                return BakerSetOpenStatus{..}
            ProtoFields.BakerEvent'BakerSetMetadataUrl' bsmUrl -> do
                let ebsmuAccount = sender
                ebsmuBakerId <- fromProto $ bsmUrl ^. ProtoFields.bakerId
                ebsmuMetadataURL <- deMkUrlText bsmUrl
                return BakerSetMetadataURL{..}
            ProtoFields.BakerEvent'BakerSetTransactionFeeCommission' bstfCommission -> do
                let ebstfcAccount = sender
                ebstfcBakerId <- fromProto $ bstfCommission ^. ProtoFields.bakerId
                ebstfcTransactionFeeCommission <- fromProto $ bstfCommission ^. ProtoFields.transactionFeeCommission
                return BakerSetTransactionFeeCommission{..}
            ProtoFields.BakerEvent'BakerSetBakingRewardCommission' bsbCommission -> do
                let ebsbrcAccount = sender
                ebsbrcBakerId <- fromProto $ bsbCommission ^. ProtoFields.bakerId
                ebsbrcBakingRewardCommission <- fromProto $ bsbCommission ^. ProtoFields.bakingRewardCommission
                return BakerSetBakingRewardCommission{..}
            ProtoFields.BakerEvent'BakerSetFinalizationRewardCommission' bsfrCommission -> do
                let ebsfrcAccount = sender
                ebsfrcBakerId <- fromProto $ bsfrCommission ^. ProtoFields.bakerId
                ebsfrcFinalizationRewardCommission <- fromProto $ bsfrCommission ^. ProtoFields.finalizationRewardCommission
                return BakerSetFinalizationRewardCommission{..}
            ProtoFields.BakerEvent'DelegationRemoved' delRemoved -> do
                let edrAccount = sender
                edrDelegatorId <- fromProto $ delRemoved ^. ProtoFields.delegatorId
                return DelegationRemoved{..}
            ProtoFields.BakerEvent'BakerSuspended' bkrSuspended -> do
                ebsBakerId <- fromProto $ bkrSuspended ^. ProtoFields.bakerId
                let ebsAccount = sender
                return BakerSuspended{..}
            ProtoFields.BakerEvent'BakerResumed' bkrResumed -> do
                let ebrAccount = sender
                ebrBakerId <- fromProto $ bkrResumed ^. ProtoFields.bakerId
                return BakerResumed{..}

instance FromProto Proto.BlockItemStatus where
    type Output Proto.BlockItemStatus = SupplementedTransactionStatus
    fromProto biStatus = do
        bis <- case biStatus ^. Proto.maybe'status of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'BlockItemStatus' due to missing field in response payload."
            Just v -> return v
        case bis of
            Proto.BlockItemStatus'Received _ -> return Received
            Proto.BlockItemStatus'Finalized' finalized -> do
                let outcome = finalized ^. ProtoFields.outcome
                (tHash, tSumm) <- fromOutcome outcome
                return $ Finalized tHash tSumm
            Proto.BlockItemStatus'Committed' committed -> do
                outcomes <- mapM fromOutcome $ committed ^. ProtoFields.outcomes
                return $ Committed (Map.fromList outcomes)
      where
        fromOutcome outcome = do
            bHash <- fromProto $ outcome ^. ProtoFields.blockHash
            tSumm <- fromProto $ outcome ^. ProtoFields.outcome
            return (bHash, Just tSumm)

instance FromProto Proto.FinalizationIndex where
    type Output Proto.FinalizationIndex = FinalizationIndex
    fromProto = return . deMkWord64

instance FromProto Proto.FinalizationSummaryParty where
    type Output Proto.FinalizationSummaryParty = FinalizationSummaryParty
    fromProto fsParty = do
        fspBakerId <- fromProto $ fsParty ^. ProtoFields.baker
        let fspWeight = fromIntegral $ fsParty ^. ProtoFields.weight
        let fspSigned = fsParty ^. ProtoFields.signed
        return FinalizationSummaryParty{..}

instance FromProto Proto.BlockFinalizationSummary where
    type Output Proto.BlockFinalizationSummary = Maybe FinalizationSummary
    fromProto bfSummary = do
        bfs <- case bfSummary ^. Proto.maybe'summary of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'BlockFinalizationSummary' due to missing field in response payload."
            Just v -> return v
        case bfs of
            ProtoFields.BlockFinalizationSummary'None _ -> return Nothing
            ProtoFields.BlockFinalizationSummary'Record record -> do
                fsFinalizationBlockPointer <- fromProto $ record ^. ProtoFields.block
                fsFinalizationIndex <- fromProto $ record ^. ProtoFields.index
                fsFinalizationDelay <- fromProto $ record ^. ProtoFields.delay
                fsFinalizers <- fmap Vec.fromList . mapM fromProto $ record ^. ProtoFields.finalizers
                return $ Just FinalizationSummary{..}

instance FromProto Proto.IpSocketAddress where
    type Output Proto.IpSocketAddress = IpSocketAddress
    fromProto ipsAddress = do
        ip <- fromProto $ ipsAddress ^. ProtoFields.ip
        port <- fromProto $ ipsAddress ^. ProtoFields.port
        return (ip, port)

instance FromProto Proto.PeersInfo'Peer'NetworkStats where
    type Output Proto.PeersInfo'Peer'NetworkStats = NetworkStats
    fromProto nStats = do
        let packetsSent = fromIntegral $ nStats ^. ProtoFields.packetsSent
        let packetsReceived = fromIntegral $ nStats ^. ProtoFields.packetsReceived
        let latency = fromIntegral $ nStats ^. ProtoFields.latency
        return NetworkStats{..}

instance FromProto Proto.PeersInfo'Peer'CatchupStatus where
    type Output Proto.PeersInfo'Peer'CatchupStatus = PeerCatchupStatus
    fromProto pcStatus = do
        case pcStatus of
            ProtoFields.PeersInfo'Peer'UPTODATE -> return UpToDate
            ProtoFields.PeersInfo'Peer'CATCHINGUP -> return CatchingUp
            ProtoFields.PeersInfo'Peer'PENDING -> return Pending
            ProtoFields.PeersInfo'Peer'CatchupStatus'Unrecognized variant ->
                fromProtoFail $
                    "Unable to convert 'PeersInfo'Peer'CatchupStatus': "
                        <> "Got unknown variant: '"
                        <> show variant
                        <> "'."

instance FromProto Proto.PeersInfo'Peer where
    type Output Proto.PeersInfo'Peer = PeerInfo
    fromProto pInfo = do
        let peerId = pInfo ^. ProtoFields.peerId . ProtoFields.value
        socketAddress <- fromProto $ pInfo ^. ProtoFields.socketAddress
        networkStats <- fromProto $ pInfo ^. ProtoFields.networkStats
        consensusInfo <- do
            c <- case pInfo ^. ProtoFields.maybe'consensusInfo of
                Nothing ->
                    fromProtoFail
                        "Unable to convert 'PeersInfo'Peer' due to missing field in response payload."
                Just v -> return v
            case c of
                ProtoFields.PeersInfo'Peer'Bootstrapper _ -> return Bootstrapper
                ProtoFields.PeersInfo'Peer'NodeCatchupStatus ncStatus -> fromProto ncStatus
        return PeerInfo{..}

instance FromProto Proto.PeersInfo where
    type Output Proto.PeersInfo = [PeerInfo]
    fromProto pInfo = mapM fromProto $ pInfo ^. Proto.peers

instance FromProto Proto.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo where
    type Output Proto.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo = PassiveCommitteeInfo
    fromProto pcInfo = do
        case pcInfo of
            ProtoFields.NodeInfo'BakerConsensusInfo'NOT_IN_COMMITTEE -> do
                return NotInCommittee
            ProtoFields.NodeInfo'BakerConsensusInfo'ADDED_BUT_NOT_ACTIVE_IN_COMMITTEE -> do
                return AddedButNotActiveInCommittee
            ProtoFields.NodeInfo'BakerConsensusInfo'ADDED_BUT_WRONG_KEYS -> do
                return AddedButWrongKeys
            ProtoFields.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo'Unrecognized _ -> do
                fromProtoFail "Unable to convert 'NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo'."

instance FromProto Proto.NodeInfo'BakerConsensusInfo where
    type Output Proto.NodeInfo'BakerConsensusInfo = BakerConsensusInfo
    fromProto bcInfo = do
        let bakerId = fromIntegral $ bcInfo ^. ProtoFields.bakerId . ProtoFields.value
        status <- do
            st <- case bcInfo ^. ProtoFields.maybe'status of
                Nothing ->
                    fromProtoFail
                        "Unable to convert 'NodeInfo'BakerConsensusInfo' due to missing field in response payload."
                Just v -> return v
            case st of
                ProtoFields.NodeInfo'BakerConsensusInfo'ActiveBakerCommitteeInfo' _ ->
                    return ActiveBaker
                ProtoFields.NodeInfo'BakerConsensusInfo'ActiveFinalizerCommitteeInfo' _ ->
                    return ActiveFinalizer
                ProtoFields.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo pcInfo ->
                    PassiveBaker <$> fromProto pcInfo
        return BakerConsensusInfo{..}

instance FromProto Proto.NodeInfo where
    type Output Proto.NodeInfo = NodeInfo
    fromProto nInfo = do
        nDetails <- case nInfo ^. ProtoFields.maybe'details of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'NodeInfo' due to missing field in response payload."
            Just v -> return v
        details <- case nDetails of
            ProtoFields.NodeInfo'Bootstrapper _ ->
                return NodeBootstrapper
            ProtoFields.NodeInfo'Node' node -> do
                n <- case node ^. ProtoFields.maybe'consensusStatus of
                    Nothing -> fromProtoFail "Unable to convert 'NodeInfo'Node'' due to missing field in response payload."
                    Just v -> return v
                case n of
                    ProtoFields.NodeInfo'Node'NotRunning _ ->
                        return NodeNotRunning
                    ProtoFields.NodeInfo'Node'Passive _ ->
                        return NodePassive
                    ProtoFields.NodeInfo'Node'Active cInfo ->
                        NodeActive <$> fromProto cInfo
        let peerVersion = nInfo ^. ProtoFields.peerVersion
        localTime <- fromProto $ nInfo ^. ProtoFields.localTime
        peerUptime <- fromProto $ nInfo ^. ProtoFields.peerUptime
        networkInfo <- fromProto $ nInfo ^. ProtoFields.networkInfo
        return NodeInfo{..}

instance FromProto Proto.NodeInfo'NetworkInfo where
    type Output Proto.NodeInfo'NetworkInfo = NetworkInfo
    fromProto nInfo = do
        let nodeId = nInfo ^. ProtoFields.nodeId . ProtoFields.value
        let peerTotalSent = fromIntegral $ nInfo ^. ProtoFields.peerTotalSent
        let peerTotalReceived = fromIntegral $ nInfo ^. ProtoFields.peerTotalReceived
        let avgBpsIn = fromIntegral $ nInfo ^. ProtoFields.avgBpsIn
        let avgBpsOut = fromIntegral $ nInfo ^. ProtoFields.avgBpsOut
        return NetworkInfo{..}

instance FromProto Proto.FinalizationCommitteeParameters where
    type Output Proto.FinalizationCommitteeParameters = Parameters.FinalizationCommitteeParameters
    fromProto fcParams = do
        _fcpMinFinalizers <- do
            let minFinalizers = fcParams ^. ProtoFields.minimumFinalizers
            unless
                (minFinalizers >= 1)
                (fromProtoFail "Unable to convert 'FinalizationCommitteeParameters': 'minFinalizers' must be at least 1.")
            return minFinalizers
        _fcpMaxFinalizers <- do
            let maxFinalizers = fcParams ^. ProtoFields.maximumFinalizers
            unless
                (maxFinalizers >= _fcpMinFinalizers)
                (fromProtoFail "Unable to convert 'FinalizationCommitteeParameters': 'minFinalizers' exceeds 'maxFinalizers'.")
            return maxFinalizers
        -- The @FromProto@ instance for @AmountFraction@ ensures this does not exceed 100_000.
        _fcpFinalizerRelativeStakeThreshold <- fmap coerce $ fromProto $ fcParams ^. ProtoFields.finalizerRelativeStakeThreshold
        return Parameters.FinalizationCommitteeParameters{..}

instance FromProto Proto.ValidatorScoreParameters where
    type Output Proto.ValidatorScoreParameters = Parameters.ValidatorScoreParameters

    fromProto vsParams = do
        let _vspMaxMissedRounds = vsParams ^. ProtoFields.maximumMissedRounds
        return Parameters.ValidatorScoreParameters{..}

instance FromProto Proto.ChainParametersV0 where
    -- \|The internal Haskell type for representing chain parameters expects
    -- an account _index_, while the protocol buffer representation uses an
    -- account _address_ for the foundation account. The workaround here is to
    -- return the address and a closure. The address can then be converted
    -- to its corresponding index and fed to the closure to get the desired
    -- @EChainParametersAndKeys@ instance.
    type Output Proto.ChainParametersV0 = (AccountAddress, AccountIndex -> FromProtoResult EChainParametersAndKeys)
    fromProto cParams = do
        faAddress <- fromProto $ cParams ^. ProtoFields.foundationAccount
        return (faAddress, faIdxToOutput)
      where
        faIdxToOutput faIndex = do
            _cpConsensusParameters <- do
                _cpElectionDifficulty <- fromProto $ cParams ^. ProtoFields.electionDifficulty
                return $ Parameters.ConsensusParametersV0 _cpElectionDifficulty
            _cpExchangeRates <- do
                euroPerEnergy <- fromProto $ cParams ^. ProtoFields.euroPerEnergy
                microCcdPerEuro <- fromProto $ cParams ^. ProtoFields.microCcdPerEuro
                return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
            _cpCooldownParameters <- do
                epoch <- fromProto $ cParams ^. ProtoFields.bakerCooldownEpochs
                return $ Parameters.CooldownParametersV0 epoch
            let _cpTimeParameters = Parameters.NoParam
            _cpAccountCreationLimit <- fromProto $ cParams ^. ProtoFields.accountCreationLimit
            _cpRewardParameters <- do
                _rpMintDistribution <- fromProto $ cParams ^. ProtoFields.mintDistribution
                _rpTransactionFeeDistribution <- fromProto $ cParams ^. ProtoFields.transactionFeeDistribution
                _rpGASRewards <- fromProto $ cParams ^. ProtoFields.gasRewards
                return (Parameters.RewardParameters{..} :: Parameters.RewardParameters 'ChainParametersV0)
            let _cpFoundationAccount = faIndex
            _cpPoolParameters <- do
                thresh <- fromProto $ cParams ^. ProtoFields.minimumThresholdForBaking
                return $ Parameters.PoolParametersV0 thresh
            let _cpFinalizationCommitteeParameters = Parameters.NoParam
            let _cpValidatorScoreParameters = Parameters.NoParam
            let ecpParams = Parameters.ChainParameters{..}
            rootKeys <- fmap snd . fromProto $ cParams ^. ProtoFields.rootKeys
            level1Keys <- fmap fst . fromProto $ cParams ^. ProtoFields.level1Keys
            level2Keys <- fromProto $ cParams ^. ProtoFields.level2Keys
            let ecpKeys = Updates.UpdateKeysCollection{..}
            return $ EChainParametersAndKeys{..}

instance FromProto Proto.ConsensusParametersV1 where
    type Output Proto.ConsensusParametersV1 = Parameters.ConsensusParameters' 'Parameters.ConsensusParametersVersion1
    fromProto cParams = do
        _cpTimeoutParameters <- fromProto $ cParams ^. ProtoFields.timeoutParameters
        _cpMinBlockTime <- fromProto $ cParams ^. ProtoFields.minBlockTime
        _cpBlockEnergyLimit <- fromProto $ cParams ^. ProtoFields.blockEnergyLimit
        return Parameters.ConsensusParametersV1{..}

instance FromProto Proto.ChainParametersV1 where
    -- \|The internal Haskell type for representing chain parameters expects
    -- an account _index_, while the protocol buffer representation uses an
    -- account _address_ for the foundation account. The workaround here is to
    -- return the address and a closure. The address can then be converted
    -- to its corresponding index and fed to the closure to get the desired
    -- @EChainParametersAndKeys@ instance.
    type Output Proto.ChainParametersV1 = (AccountAddress, AccountIndex -> FromProtoResult EChainParametersAndKeys)
    fromProto cParams = do
        faAddress <- fromProto $ cParams ^. ProtoFields.foundationAccount
        return (faAddress, faIdxToOutput)
      where
        faIdxToOutput faIndex = do
            _cpConsensusParameters <- do
                _cpElectionDifficulty <- fromProto $ cParams ^. ProtoFields.electionDifficulty
                return $ Parameters.ConsensusParametersV0 _cpElectionDifficulty
            _cpElectionDifficulty <- fromProto $ cParams ^. ProtoFields.electionDifficulty
            _cpExchangeRates <- do
                euroPerEnergy <- fromProto $ cParams ^. ProtoFields.euroPerEnergy
                microCcdPerEuro <- fromProto $ cParams ^. ProtoFields.microCcdPerEuro
                return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
            _cpCooldownParameters <- fromProto $ cParams ^. ProtoFields.cooldownParameters
            _cpTimeParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.timeParameters
            _cpAccountCreationLimit <- fromProto $ cParams ^. ProtoFields.accountCreationLimit
            _cpRewardParameters <- do
                _rpMintDistribution <- fromProto $ cParams ^. ProtoFields.mintDistribution
                _rpTransactionFeeDistribution <- fromProto $ cParams ^. ProtoFields.transactionFeeDistribution
                _rpGASRewards <- fromProto $ cParams ^. ProtoFields.gasRewards
                return (Parameters.RewardParameters{..} :: Parameters.RewardParameters 'ChainParametersV1)
            let _cpFoundationAccount = faIndex
            _cpPoolParameters <- fromProto $ cParams ^. ProtoFields.poolParameters
            let _cpFinalizationCommitteeParameters = Parameters.NoParam
            let _cpValidatorScoreParameters = Parameters.NoParam
            let ecpParams = Parameters.ChainParameters{..}
            rootKeys <- fmap snd . fromProto $ cParams ^. ProtoFields.rootKeys
            level1Keys <- fmap fst . fromProto $ cParams ^. ProtoFields.level1Keys
            level2Keys <- fromProto $ cParams ^. ProtoFields.level2Keys
            let ecpKeys = Updates.UpdateKeysCollection{..}
            return $ EChainParametersAndKeys ecpParams ecpKeys

instance FromProto Proto.ChainParametersV2 where
    -- \|The internal Haskell type for representing chain parameters expects
    -- an account _index_, while the protocol buffer representation uses an
    -- account _address_ for the foundation account. The workaround here is to
    -- return the address and a closure. The address can then be converted
    -- to its corresponding index and fed to the closure to get the desired
    -- @EChainParametersAndKeys@ instance.
    type Output Proto.ChainParametersV2 = (AccountAddress, AccountIndex -> FromProtoResult EChainParametersAndKeys)
    fromProto cParams = do
        faAddress <- fromProto $ cParams ^. ProtoFields.foundationAccount
        return (faAddress, faIdxToOutput)
      where
        faIdxToOutput faIndex = do
            _cpConsensusParameters <- fromProto $ cParams ^. ProtoFields.consensusParameters
            _cpExchangeRates <- do
                euroPerEnergy <- fromProto $ cParams ^. ProtoFields.euroPerEnergy
                microCcdPerEuro <- fromProto $ cParams ^. ProtoFields.microCcdPerEuro
                return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
            _cpCooldownParameters <- fromProto $ cParams ^. ProtoFields.cooldownParameters
            _cpTimeParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.timeParameters
            _cpAccountCreationLimit <- fromProto $ cParams ^. ProtoFields.accountCreationLimit
            _cpRewardParameters <- do
                _rpMintDistribution <- fromProto $ cParams ^. ProtoFields.mintDistribution
                _rpTransactionFeeDistribution <- fromProto $ cParams ^. ProtoFields.transactionFeeDistribution
                _rpGASRewards <- fromProto $ cParams ^. ProtoFields.gasRewards
                return (Parameters.RewardParameters{..} :: Parameters.RewardParameters 'ChainParametersV2)
            let _cpFoundationAccount = faIndex
            _cpPoolParameters <- fromProto $ cParams ^. ProtoFields.poolParameters
            _cpFinalizationCommitteeParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.finalizationCommitteeParameters
            let _cpValidatorScoreParameters = Parameters.NoParam
            let ecpParams = Parameters.ChainParameters{..}
            rootKeys <- fmap snd . fromProto $ cParams ^. ProtoFields.rootKeys
            level1Keys <- fmap fst . fromProto $ cParams ^. ProtoFields.level1Keys
            level2Keys <- fromProto $ cParams ^. ProtoFields.level2Keys
            let ecpKeys = Updates.UpdateKeysCollection{..}
            return $ EChainParametersAndKeys ecpParams ecpKeys

instance FromProto Proto.ChainParametersV3 where
    -- \|The internal Haskell type for representing chain parameters expects
    -- an account _index_, while the protocol buffer representation uses an
    -- account _address_ for the foundation account. The workaround here is to
    -- return the address and a closure. The address can then be converted
    -- to its corresponding index and fed to the closure to get the desired
    -- @EChainParametersAndKeys@ instance.
    type Output Proto.ChainParametersV3 = (AccountAddress, AccountIndex -> FromProtoResult EChainParametersAndKeys)
    fromProto cParams = do
        faAddress <- fromProto $ cParams ^. ProtoFields.foundationAccount
        return (faAddress, faIdxToOutput)
      where
        faIdxToOutput faIndex = do
            _cpConsensusParameters <- fromProto $ cParams ^. ProtoFields.consensusParameters
            _cpExchangeRates <- do
                euroPerEnergy <- fromProto $ cParams ^. ProtoFields.euroPerEnergy
                microCcdPerEuro <- fromProto $ cParams ^. ProtoFields.microCcdPerEuro
                return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
            _cpCooldownParameters <- fromProto $ cParams ^. ProtoFields.cooldownParameters
            _cpTimeParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.timeParameters
            _cpAccountCreationLimit <- fromProto $ cParams ^. ProtoFields.accountCreationLimit
            _cpRewardParameters <- do
                _rpMintDistribution <- fromProto $ cParams ^. ProtoFields.mintDistribution
                _rpTransactionFeeDistribution <- fromProto $ cParams ^. ProtoFields.transactionFeeDistribution
                _rpGASRewards <- fromProto $ cParams ^. ProtoFields.gasRewards
                return (Parameters.RewardParameters{..} :: Parameters.RewardParameters 'ChainParametersV3)
            let _cpFoundationAccount = faIndex
            _cpPoolParameters <- fromProto $ cParams ^. ProtoFields.poolParameters
            _cpFinalizationCommitteeParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.finalizationCommitteeParameters
            _cpValidatorScoreParameters <- fmap Parameters.SomeParam $ fromProto $ cParams ^. ProtoFields.validatorScoreParameters
            let ecpParams = Parameters.ChainParameters{..}
            rootKeys <- fmap snd . fromProto $ cParams ^. ProtoFields.rootKeys
            level1Keys <- fmap fst . fromProto $ cParams ^. ProtoFields.level1Keys
            level2Keys <- fromProto $ cParams ^. ProtoFields.level2Keys
            let ecpKeys = Updates.UpdateKeysCollection{..}
            return $ EChainParametersAndKeys ecpParams ecpKeys

instance FromProto Proto.Epoch where
    type Output Proto.Epoch = Epoch
    fromProto = return . deMkWord64

instance FromProto Proto.CredentialsPerBlockLimit where
    type Output Proto.CredentialsPerBlockLimit = CredentialsPerBlockLimit
    fromProto cpbl =
        case deMkWord16 cpbl of
            Left err ->
                fromProtoFail $
                    "Unable to convert 'CredentialsPerBlockLimit': " <> err
            Right v -> return v

instance FromProto Proto.ChainParameters where
    type Output Proto.ChainParameters = (AccountAddress, AccountIndex -> FromProtoResult EChainParametersAndKeys)
    fromProto cParams = do
        cp <- case cParams ^. ProtoFields.maybe'parameters of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'ChainParameters' due to missing field in response payload."
            Just v -> return v
        case cp of
            Proto.ChainParameters'V0 v0 -> fromProto v0
            Proto.ChainParameters'V1 v1 -> fromProto v1
            Proto.ChainParameters'V2 v2 -> fromProto v2
            Proto.ChainParameters'V3 v3 -> fromProto v3

instance FromProto Proto.CryptographicParameters where
    type Output Proto.CryptographicParameters = Parameters.CryptographicParameters
    fromProto cParams =
        do
            let genString = cParams ^. ProtoFields.genesisString
            let bpGens = cParams ^. ProtoFields.bulletproofGenerators
            let occKey = cParams ^. ProtoFields.onChainCommitmentKey
            case createGlobalContext genString bpGens occKey of
                Nothing ->
                    fromProtoFail $
                        "Unable to convert 'CryptographicParameters'. Could not create foreign instance "
                            <> "due to malformed payload data."
                Just v -> return v

instance FromProto Proto.ArrivedBlockInfo where
    type Output Proto.ArrivedBlockInfo = ArrivedBlockInfo
    fromProto abInfo = do
        abiBlockHash <- fromProto $ abInfo ^. ProtoFields.hash
        abiBlockHeight <- fromProto $ abInfo ^. ProtoFields.height
        return ArrivedBlockInfo{..}

instance FromProto Proto.FinalizedBlockInfo where
    type Output Proto.FinalizedBlockInfo = ArrivedBlockInfo
    fromProto abInfo = do
        abiBlockHash <- fromProto $ abInfo ^. ProtoFields.hash
        abiBlockHeight <- fromProto $ abInfo ^. ProtoFields.height
        return ArrivedBlockInfo{..}

instance FromProto Proto.InstanceStateKVPair where
    type Output Proto.InstanceStateKVPair = (ByteString, ByteString)
    fromProto ikvPair = do
        let key = ikvPair ^. ProtoFields.key
        let value = ikvPair ^. ProtoFields.value
        return (key, value)

instance FromProto Proto.DelegatorInfo where
    type Output Proto.DelegatorInfo = DelegatorInfo
    fromProto dInfo = do
        pdiAccount <- fromProto $ dInfo ^. ProtoFields.account
        pdiStake <- fromProto $ dInfo ^. ProtoFields.stake
        pdiPendingChanges <-
            case dInfo ^. ProtoFields.maybe'pendingChange of
                Nothing -> return NoChange
                Just v -> fromProto v
        return DelegatorInfo{..}

instance FromProto Proto.DelegatorRewardPeriodInfo where
    type Output Proto.DelegatorRewardPeriodInfo = DelegatorRewardPeriodInfo
    fromProto dInfo = do
        pdrpiAccount <- fromProto $ dInfo ^. ProtoFields.account
        pdrpiStake <- fromProto $ dInfo ^. ProtoFields.stake
        return DelegatorRewardPeriodInfo{..}

instance FromProto Proto.BakerRewardPeriodInfo where
    type Output Proto.BakerRewardPeriodInfo = BakerRewardPeriodInfo
    fromProto brpInfo = do
        brpiBaker <- fromProto $ brpInfo ^. ProtoFields.baker
        brpiEffectiveStake <- fromProto $ brpInfo ^. ProtoFields.effectiveStake
        brpiCommissionRates <- fromProto $ brpInfo ^. ProtoFields.commissionRates
        brpiEquityCapital <- fromProto $ brpInfo ^. ProtoFields.equityCapital
        brpiDelegatedCapital <- fromProto $ brpInfo ^. ProtoFields.delegatedCapital
        let brpiIsFinalizer = brpInfo ^. ProtoFields.isFinalizer
        return BakerRewardPeriodInfo{..}

instance FromProto Proto.BlockSpecialEvent'AccountAmounts where
    type Output Proto.BlockSpecialEvent'AccountAmounts = Transactions.AccountAmounts
    fromProto aAmounts = do
        pairs <- mapM convertEntry $ aAmounts ^. ProtoFields.entries
        return $ Transactions.AccountAmounts $ Map.fromList pairs
      where
        convertEntry e = do
            address <- fromProto $ e ^. ProtoFields.account
            amount <- fromProto $ e ^. ProtoFields.amount
            return (address, amount)

instance FromProto Proto.QuorumSignature where
    type Output Proto.QuorumSignature = KonsensusV1.QuorumCertificateSignature
    fromProto qs = case deMkSerialize qs of
        Left err -> fromProtoFail $ "Unable to convert 'QuorumSignature': " <> err
        Right v -> return v

instance FromProto Proto.QuorumCertificate where
    type Output Proto.QuorumCertificate = KonsensusV1.QuorumCertificate
    fromProto qc = do
        qcBlock <- fromProto $ qc ^. ProtoFields.blockHash
        qcRound <- fromProto $ qc ^. ProtoFields.round
        qcEpoch <- fromProto $ qc ^. ProtoFields.epoch
        qcAggregateSignature <- fromProto $ qc ^. ProtoFields.aggregateSignature
        qcSignatories <- mapM fromProto $ qc ^. ProtoFields.signatories
        return KonsensusV1.QuorumCertificate{..}

instance FromProto Proto.TimeoutSignature where
    type Output Proto.TimeoutSignature = KonsensusV1.TimeoutCertificateSignature
    fromProto ts = case deMkSerialize ts of
        Left err -> fromProtoFail $ "Unable to convert 'TimeoutSignature': " <> err
        Right v -> return v

instance FromProto Proto.FinalizerRound where
    type Output Proto.FinalizerRound = KonsensusV1.FinalizerRound
    fromProto fr = do
        frRound <- fromProto $ fr ^. ProtoFields.round
        frFinalizers <- mapM fromProto $ fr ^. ProtoFields.finalizers
        return KonsensusV1.FinalizerRound{..}

instance FromProto Proto.TimeoutCertificate where
    type Output Proto.TimeoutCertificate = KonsensusV1.TimeoutCertificate
    fromProto tc = do
        tcRound <- fromProto $ tc ^. ProtoFields.round
        tcMinEpoch <- fromProto $ tc ^. ProtoFields.minEpoch
        tcFinalizerQCRoundsFirstEpoch <- mapM fromProto $ tc ^. ProtoFields.qcRoundsFirstEpoch
        tcFinalizerQCRoundsSecondEpoch <- mapM fromProto $ tc ^. ProtoFields.qcRoundsSecondEpoch
        tcAggregateSignature <- fromProto $ tc ^. ProtoFields.aggregateSignature
        return KonsensusV1.TimeoutCertificate{..}

instance FromProto Proto.SuccessorProof where
    type Output Proto.SuccessorProof = KonsensusV1.SuccessorProof
    fromProto sp = case deMkSerialize sp of
        Left err -> fromProtoFail $ "Unable to convert 'SuccessorProof': " <> err
        Right v -> return v

instance FromProto Proto.EpochFinalizationEntry where
    type Output Proto.EpochFinalizationEntry = KonsensusV1.EpochFinalizationEntry
    fromProto efe = do
        efeFinalizedQC <- fromProto $ efe ^. ProtoFields.finalizedQc
        efeSuccessorQC <- fromProto $ efe ^. ProtoFields.successorQc
        efeSuccessorProof <- fromProto $ efe ^. ProtoFields.successorProof
        return KonsensusV1.EpochFinalizationEntry{..}

instance FromProto Proto.BlockCertificates where
    type Output Proto.BlockCertificates = KonsensusV1.BlockCertificates
    fromProto bc = do
        bcQuorumCertificate <- fromProtoMaybe $ bc ^. ProtoFields.maybe'quorumCertificate
        bcTimeoutCertificate <- fromProtoMaybe $ bc ^. ProtoFields.maybe'timeoutCertificate
        bcEpochFinalizationEntry <- fromProtoMaybe $ bc ^. ProtoFields.maybe'epochFinalizationEntry
        return KonsensusV1.BlockCertificates{..}

instance FromProto Proto.BlockSpecialEvent where
    type Output Proto.BlockSpecialEvent = Transactions.SpecialTransactionOutcome
    fromProto bsEvent = do
        bse <- case bsEvent ^. ProtoFields.maybe'event of
            Nothing ->
                fromProtoFail
                    "Unable to convert 'BlockSpecialEvent'' due to missing field in response payload."
            Just v -> return v
        case bse of
            ProtoFields.BlockSpecialEvent'BakingRewards' bReward -> do
                stoBakerRewards <- fromProto $ bReward ^. ProtoFields.bakerRewards
                stoRemainder <- fromProto $ bReward ^. ProtoFields.remainder
                return Transactions.BakingRewards{..}
            ProtoFields.BlockSpecialEvent'Mint' mint -> do
                stoMintBakingReward <- fromProto $ mint ^. ProtoFields.mintBakingReward
                stoMintFinalizationReward <- fromProto $ mint ^. ProtoFields.mintFinalizationReward
                stoMintPlatformDevelopmentCharge <- fromProto $ mint ^. ProtoFields.mintPlatformDevelopmentCharge
                stoFoundationAccount <- fromProto $ mint ^. ProtoFields.foundationAccount
                return Transactions.Mint{..}
            ProtoFields.BlockSpecialEvent'FinalizationRewards' fRewards -> do
                stoFinalizationRewards <- fromProto $ fRewards ^. ProtoFields.finalizationRewards
                stoRemainder <- fromProto $ fRewards ^. ProtoFields.remainder
                return Transactions.FinalizationRewards{..}
            ProtoFields.BlockSpecialEvent'BlockReward' bReward -> do
                stoTransactionFees <- fromProto $ bReward ^. ProtoFields.transactionFees
                stoOldGASAccount <- fromProto $ bReward ^. ProtoFields.oldGasAccount
                stoNewGASAccount <- fromProto $ bReward ^. ProtoFields.newGasAccount
                stoBakerReward <- fromProto $ bReward ^. ProtoFields.bakerReward
                stoFoundationCharge <- fromProto $ bReward ^. ProtoFields.foundationCharge
                stoBaker <- fromProto $ bReward ^. ProtoFields.baker
                stoFoundationAccount <- fromProto $ bReward ^. ProtoFields.foundationAccount
                return Transactions.BlockReward{..}
            ProtoFields.BlockSpecialEvent'PaydayFoundationReward' pdfReward -> do
                stoFoundationAccount <- fromProto $ pdfReward ^. ProtoFields.foundationAccount
                stoDevelopmentCharge <- fromProto $ pdfReward ^. ProtoFields.developmentCharge
                return Transactions.PaydayFoundationReward{..}
            ProtoFields.BlockSpecialEvent'PaydayAccountReward' pdaReward -> do
                stoAccount <- fromProto $ pdaReward ^. ProtoFields.account
                stoTransactionFees <- fromProto $ pdaReward ^. ProtoFields.transactionFees
                stoBakerReward <- fromProto $ pdaReward ^. ProtoFields.bakerReward
                stoFinalizationReward <- fromProto $ pdaReward ^. ProtoFields.finalizationReward
                return Transactions.PaydayAccountReward{..}
            ProtoFields.BlockSpecialEvent'BlockAccrueReward' baReward -> do
                stoTransactionFees <- fromProto $ baReward ^. ProtoFields.transactionFees
                stoOldGASAccount <- fromProto $ baReward ^. ProtoFields.oldGasAccount
                stoNewGASAccount <- fromProto $ baReward ^. ProtoFields.newGasAccount
                stoBakerReward <- fromProto $ baReward ^. ProtoFields.bakerReward
                stoPassiveReward <- fromProto $ baReward ^. ProtoFields.passiveReward
                stoFoundationCharge <- fromProto $ baReward ^. ProtoFields.foundationCharge
                stoBakerId <- fromProto $ baReward ^. ProtoFields.baker
                return Transactions.BlockAccrueReward{..}
            ProtoFields.BlockSpecialEvent'PaydayPoolReward' ppReward -> do
                stoPoolOwner <- fromProtoMaybe $ ppReward ^. ProtoFields.maybe'poolOwner
                stoTransactionFees <- fromProto $ ppReward ^. ProtoFields.transactionFees
                stoBakerReward <- fromProto $ ppReward ^. ProtoFields.bakerReward
                stoFinalizationReward <- fromProto $ ppReward ^. ProtoFields.finalizationReward
                return Transactions.PaydayPoolReward{..}
            ProtoFields.BlockSpecialEvent'ValidatorSuspended' vSuspended -> do
                vsBakerId <- fromProto $ vSuspended ^. ProtoFields.bakerId
                vsAccount <- fromProto $ vSuspended ^. ProtoFields.account
                return Transactions.ValidatorSuspended{..}
            ProtoFields.BlockSpecialEvent'ValidatorPrimedForSuspension' vpfSuspension -> do
                vpfsBakerId <- fromProto $ vpfSuspension ^. ProtoFields.bakerId
                vpfsAccount <- fromProto $ vpfSuspension ^. ProtoFields.account
                return Transactions.ValidatorPrimedForSuspension{..}

instance FromProto Proto.PendingUpdate where
    type Output Proto.PendingUpdate = PendingUpdate
    fromProto pUpdate = do
        puEffectiveTime <- fromProto $ pUpdate ^. ProtoFields.effectiveTime
        puEffect <- do
            pue <- case pUpdate ^. ProtoFields.maybe'effect of
                Nothing ->
                    fromProtoFail
                        "Unable to convert 'PendingUpdate' due to missing field in response payload."
                Just v -> return v
            case pue of
                ProtoFields.PendingUpdate'RootKeys rKeys -> do
                    PUERootKeys . snd <$> fromProto rKeys
                ProtoFields.PendingUpdate'Level1Keys l1Keys -> do
                    PUELevel1Keys . fst <$> fromProto l1Keys
                ProtoFields.PendingUpdate'Level2KeysCpv0 l2Keys -> do
                    PUELevel2KeysV0 <$> fromProto l2Keys
                ProtoFields.PendingUpdate'Level2KeysCpv1 l2Keys -> do
                    PUELevel2KeysV1 <$> fromProto l2Keys
                ProtoFields.PendingUpdate'Protocol protocol -> do
                    PUEProtocol <$> fromProto protocol
                ProtoFields.PendingUpdate'ElectionDifficulty eDifficulty -> do
                    PUEElectionDifficulty <$> fromProto eDifficulty
                ProtoFields.PendingUpdate'EuroPerEnergy epEnergy -> do
                    PUEEuroPerEnergy <$> fromProto epEnergy
                ProtoFields.PendingUpdate'MicroCcdPerEuro mcpEuro -> do
                    PUEMicroCCDPerEuro <$> fromProto mcpEuro
                ProtoFields.PendingUpdate'FoundationAccount fAccount ->
                    PUEFoundationAccount <$> fromProto fAccount
                ProtoFields.PendingUpdate'MintDistributionCpv0 mDistribution ->
                    PUEMintDistributionV0 <$> fromProto mDistribution
                ProtoFields.PendingUpdate'MintDistributionCpv1 mDistribution ->
                    PUEMintDistributionV1 <$> fromProto mDistribution
                ProtoFields.PendingUpdate'TransactionFeeDistribution tfDistribution ->
                    PUETransactionFeeDistribution <$> fromProto tfDistribution
                ProtoFields.PendingUpdate'GasRewards gRewards ->
                    PUEGASRewardsV0 <$> fromProto gRewards
                ProtoFields.PendingUpdate'GasRewardsCpv2 gRewards ->
                    PUEGASRewardsV1 <$> fromProto gRewards
                ProtoFields.PendingUpdate'PoolParametersCpv0 pParameters ->
                    PUEPoolParametersV0 <$> fromProto pParameters
                ProtoFields.PendingUpdate'PoolParametersCpv1 pParameters ->
                    PUEPoolParametersV1 <$> fromProto pParameters
                ProtoFields.PendingUpdate'AddAnonymityRevoker aaRevoker ->
                    PUEAddAnonymityRevoker <$> fromProto aaRevoker
                ProtoFields.PendingUpdate'AddIdentityProvider aiProvider ->
                    PUEAddIdentityProvider <$> fromProto aiProvider
                ProtoFields.PendingUpdate'CooldownParameters cdParameters -> do
                    PUECooldownParameters <$> fromProto cdParameters
                ProtoFields.PendingUpdate'TimeParameters tParameters -> do
                    PUETimeParameters <$> fromProto tParameters
                ProtoFields.PendingUpdate'TimeoutParameters toParams -> do
                    PUETimeoutParameters <$> fromProto toParams
                ProtoFields.PendingUpdate'MinBlockTime mbTime -> do
                    PUEMinBlockTime <$> fromProto mbTime
                ProtoFields.PendingUpdate'BlockEnergyLimit beLimit -> do
                    PUEBlockEnergyLimit <$> fromProto beLimit
                ProtoFields.PendingUpdate'FinalizationCommitteeParameters fcParams -> do
                    PUEFinalizationCommitteeParameters <$> fromProto fcParams
                ProtoFields.PendingUpdate'ValidatorScoreParameters vsParams -> do
                    PUEValidatorScoreParameters <$> fromProto vsParams
        return PendingUpdate{..}

instance FromProto Proto.WinningBaker where
    type Output Proto.WinningBaker = WinningBaker
    fromProto winningBaker = do
        wbRound <- fromProto (winningBaker ^. ProtoFields.round)
        wbWinner <- fromProto (winningBaker ^. ProtoFields.winner)
        let wbPresent = winningBaker ^. ProtoFields.present
        return WinningBaker{..}

instance FromProto Proto.FinalizerIndex where
    type Output Proto.FinalizerIndex = FinalizerIndex
    fromProto fi = return . FinalizerIndex $ fi ^. ProtoFields.value

instance FromProto Proto.QuorumMessage where
    type Output Proto.QuorumMessage = QuorumMessage
    fromProto qm = do
        qmSignature <- fromProto (qm ^. ProtoFields.signature)
        qmBlock <- fromProto (qm ^. ProtoFields.block)
        qmFinalizer <- fromProto (qm ^. ProtoFields.finalizer)
        qmRound <- fromProto (qm ^. ProtoFields.round)
        qmEpoch <- fromProto (qm ^. ProtoFields.epoch)
        return QuorumMessage{..}

instance FromProto Proto.RawQuorumCertificate where
    type Output Proto.RawQuorumCertificate = QuorumCertificate
    fromProto qc = do
        qcBlockHash <- fromProto (qc ^. ProtoFields.blockHash)
        qcRound <- fromProto (qc ^. ProtoFields.round)
        qcEpoch <- fromProto (qc ^. ProtoFields.epoch)
        qcAggregateSignature <- fromProto (qc ^. ProtoFields.aggregateSignature)
        qcSignatories <- mapM fromProto (qc ^. ProtoFields.signatories)
        return QuorumCertificate{..}

instance FromProto Proto.BlockSignature where
    type Output Proto.BlockSignature = BlockSignature.Signature
    fromProto = deMkSerialize

instance FromProto Proto.TimeoutMessage where
    type Output Proto.TimeoutMessage = TimeoutMessage
    fromProto tm = do
        tmFinalizer <- fromProto (tm ^. ProtoFields.finalizer)
        tmRound <- fromProto (tm ^. ProtoFields.round)
        tmEpoch <- fromProto (tm ^. ProtoFields.epoch)
        tmQuorumCertificate <- fromProto (tm ^. ProtoFields.quorumCertificate)
        tmSignature <- fromProto (tm ^. ProtoFields.signature)
        tmMessageSignature <- fromProto (tm ^. ProtoFields.messageSignature)
        return TimeoutMessage{..}

instance FromProto Proto.RawFinalizerRound where
    type Output Proto.RawFinalizerRound = FinalizerRound
    fromProto fr = do
        frRound <- fromProto (fr ^. ProtoFields.round)
        frFinalizers <- mapM fromProto (fr ^. ProtoFields.finalizers)
        return FinalizerRound{..}

instance FromProto Proto.RawTimeoutCertificate where
    type Output Proto.RawTimeoutCertificate = TimeoutCertificate
    fromProto tc = do
        tcRound <- fromProto (tc ^. ProtoFields.round)
        tcMinEpoch <- fromProto (tc ^. ProtoFields.minEpoch)
        tcQcRoundsFirstEpoch <- mapM fromProto (tc ^. ProtoFields.qcRoundsFirstEpoch)
        tcQcRoundsSecondEpoch <- mapM fromProto (tc ^. ProtoFields.qcRoundsSecondEpoch)
        tcAggregateSignature <- fromProto (tc ^. ProtoFields.aggregateSignature)
        return TimeoutCertificate{..}

instance FromProto Proto.PersistentRoundStatus where
    type Output Proto.PersistentRoundStatus = PersistentRoundStatus
    fromProto prs = do
        prsLastSignedQuorumMessage <- mapM fromProto (prs ^. ProtoFields.maybe'lastSignedQuorumMessage)
        prsLastSignedTimeoutMessage <- mapM fromProto (prs ^. ProtoFields.maybe'lastSignedTimeoutMessage)
        prsLastBakedRound <- fromProto (prs ^. ProtoFields.lastBakedRound)
        prsLatestTimeout <- mapM fromProto (prs ^. ProtoFields.maybe'latestTimeout)
        return PersistentRoundStatus{..}

instance FromProto Proto.RoundTimeout where
    type Output Proto.RoundTimeout = RoundTimeout
    fromProto rt = do
        rtTimeoutCertificate <- fromProto (rt ^. ProtoFields.timeoutCertificate)
        rtQuorumCertificate <- fromProto (rt ^. ProtoFields.quorumCertificate)
        return RoundTimeout{..}

instance FromProto Proto.RawFinalizationEntry where
    type Output Proto.RawFinalizationEntry = FinalizationEntry
    fromProto fe = do
        feFinalizedQC <- fromProto (fe ^. ProtoFields.finalizedQc)
        feSuccessorQC <- fromProto (fe ^. ProtoFields.successorQc)
        feSuccessorProof <- fromProto (fe ^. ProtoFields.successorProof)
        return FinalizationEntry{..}

instance FromProto Proto.RoundStatus where
    type Output Proto.RoundStatus = RoundStatus
    fromProto rs = do
        rsCurrentRound <- fromProto (rs ^. ProtoFields.currentRound)
        rsHighestCertifiedBlock <- fromProto (rs ^. ProtoFields.highestCertifiedBlock)
        rsPreviousRoundTimeout <- mapM fromProto (rs ^. ProtoFields.maybe'previousRoundTimeout)
        let rsRoundEligibleToBake = rs ^. ProtoFields.roundEligibleToBake
        rsCurrentEpoch <- fromProto (rs ^. ProtoFields.currentEpoch)
        rsLastEpochFinalizationEntry <- mapM fromProto (rs ^. ProtoFields.maybe'lastEpochFinalizationEntry)
        rsCurrentTimeout <- fromProto (rs ^. ProtoFields.currentTimeout)
        return RoundStatus{..}

instance FromProto Proto.BlockTableSummary where
    type Output Proto.BlockTableSummary = BlockTableSummary
    fromProto bts = do
        let btsDeadBlockCacheSize = bts ^. ProtoFields.deadBlockCacheSize
        btsLiveBlocks <- mapM fromProto (bts ^. ProtoFields.liveBlocks)
        return BlockTableSummary{..}

instance FromProto Proto.RoundExistingBlock where
    type Output Proto.RoundExistingBlock = RoundExistingBlock
    fromProto reb = do
        rebRound <- fromProto (reb ^. ProtoFields.round)
        rebBaker <- fromProto (reb ^. ProtoFields.baker)
        rebBlock <- fromProto (reb ^. ProtoFields.block)
        return RoundExistingBlock{..}

instance FromProto Proto.RoundExistingQC where
    type Output Proto.RoundExistingQC = RoundExistingQC
    fromProto req = do
        reqRound <- fromProto (req ^. ProtoFields.round)
        reqEpoch <- fromProto (req ^. ProtoFields.epoch)
        return RoundExistingQC{..}

instance FromProto Proto.FullBakerInfo where
    type Output Proto.FullBakerInfo = FullBakerInfo
    fromProto fbi = do
        fbiBakerIdentity <- fromProto (fbi ^. ProtoFields.bakerIdentity)
        fbiElectionVerifyKey <- fromProto (fbi ^. ProtoFields.electionVerifyKey)
        fbiSignatureVerifyKey <- fromProto (fbi ^. ProtoFields.signatureVerifyKey)
        fbiAggregationVerifyKey <- fromProto (fbi ^. ProtoFields.aggregationVerifyKey)
        fbiStake <- fromProto (fbi ^. ProtoFields.stake)
        return FullBakerInfo{..}

instance FromProto Proto.FinalizationCommitteeHash where
    type Output Proto.FinalizationCommitteeHash = Hash
    fromProto = deMkSerialize

instance FromProto Proto.BakersAndFinalizers where
    type Output Proto.BakersAndFinalizers = BakersAndFinalizers
    fromProto baf = do
        bafBakers <- mapM fromProto (baf ^. ProtoFields.bakers)
        bafFinalizers <- mapM fromProto (baf ^. ProtoFields.finalizers)
        bafBakerTotalStake <- fromProto (baf ^. ProtoFields.bakerTotalStake)
        bafFinalizerTotalStake <- fromProto (baf ^. ProtoFields.finalizerTotalStake)
        bafFinalizationCommitteeHash <- fromProto (baf ^. ProtoFields.finalizationCommitteeHash)
        return BakersAndFinalizers{..}

instance FromProto Proto.EpochBakers where
    type Output Proto.EpochBakers = EpochBakers
    fromProto epochBakers = do
        ebPreviousEpochBakers <- fromProto (epochBakers ^. ProtoFields.previousEpochBakers)
        ebCurrentEpochBakers <- mapM fromProto (epochBakers ^. ProtoFields.maybe'currentEpochBakers)
        ebNextEpochBakers <- mapM fromProto (epochBakers ^. ProtoFields.maybe'nextEpochBakers)
        ebNextPayday <- fromProto (epochBakers ^. ProtoFields.nextPayday)
        return EpochBakers{..}

instance FromProto Proto.TimeoutMessages where
    type Output Proto.TimeoutMessages = TimeoutMessages
    fromProto timeoutMessages = do
        tmFirstEpoch <- fromProto (timeoutMessages ^. ProtoFields.firstEpoch)
        tmFirstEpochTimeouts <- mapM fromProto (timeoutMessages ^. ProtoFields.firstEpochTimeouts)
        tmSecondEpochTimeouts <- mapM fromProto (timeoutMessages ^. ProtoFields.secondEpochTimeouts)
        return TimeoutMessages{..}

instance FromProto Proto.BranchBlocks where
    type Output Proto.BranchBlocks = [BlockHash]
    fromProto branchBlocks = mapM fromProto (branchBlocks ^. ProtoFields.blocksAtBranchHeight)

instance FromProto Proto.ConsensusDetailedStatus where
    type Output Proto.ConsensusDetailedStatus = ConsensusDetailedStatus
    fromProto consensusDetailedStatus = do
        cdsGenesisBlock <- fromProto (consensusDetailedStatus ^. ProtoFields.genesisBlock)
        cdsPersistentRoundStatus <- fromProto (consensusDetailedStatus ^. ProtoFields.persistentRoundStatus)
        cdsRoundStatus <- fromProto (consensusDetailedStatus ^. ProtoFields.roundStatus)
        let cdsNonFinalizedTransactionCount = consensusDetailedStatus ^. ProtoFields.nonFinalizedTransactionCount
        let cdsTransactionTablePurgeCounter = consensusDetailedStatus ^. ProtoFields.transactionTablePurgeCounter
        cdsBlockTable <- fromProto (consensusDetailedStatus ^. ProtoFields.blockTable)
        cdsBranches <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.branches)
        cdsRoundExistingBlocks <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.roundExistingBlocks)
        cdsRoundExistingQCs <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.roundExistingQcs)
        cdsGenesisBlockHeight <- fromProto (consensusDetailedStatus ^. ProtoFields.genesisBlockHeight)
        cdsLastFinalizedBlock <- fromProto (consensusDetailedStatus ^. ProtoFields.lastFinalizedBlock)
        cdsLastFinalizedBlockHeight <- fromProto (consensusDetailedStatus ^. ProtoFields.lastFinalizedBlockHeight)
        cdsLatestFinalizationEntry <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.maybe'latestFinalizationEntry)
        cdsEpochBakers <- fromProto (consensusDetailedStatus ^. ProtoFields.epochBakers)
        cdsTimeoutMessages <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.maybe'timeoutMessages)
        cdsTerminalBlock <- mapM fromProto (consensusDetailedStatus ^. ProtoFields.maybe'terminalBlock)
        return ConsensusDetailedStatus{..}

instance FromProto Proto.AccountPending where
    type Output Proto.AccountPending = AccountPending
    fromProto acctPending = do
        apAccountIndex <- fromProto (acctPending ^. ProtoFields.accountIndex)
        apFirstTimestamp <- fromProto (acctPending ^. ProtoFields.firstTimestamp)
        return AccountPending{..}

type LoggerMethod = Text -> IO ()

data GrpcConfig = GrpcConfig
    { host :: !HostName,
      port :: !PortNumber,
      -- Target node, i.e. "node-0" for use with grpc-proxy.eu.test.concordium.com against testnet
      target :: !(Maybe String),
      -- | Number of times to __retry__ to establish a connection. Thus a value of
      --  0 means try only once.
      retryNum :: !Int,
      -- | Timeout of each RPC call (defaults to 5min if not given).
      timeout :: !(Maybe Int),
      -- | Whether to use TLS or not.
      useTls :: !Bool
    }

data EnvData = EnvData
    { -- How many times to retry to establish the connection.
      -- 0 means only try once.
      retryTimes :: !Int,
      config :: !GrpcClientConfig,
      rwlock :: !RWLock,
      -- | A shared reference to a connection together with a generation counter.
      --  All queries will reuse this single connection as much as possible. This
      --  is @Nothing@ if no connection is yet established. When we reconnect we
      --  increase the generation counter. The reason for the generation counter
      --  is so that if multiple queries are in-flight at the time the connection
      --  is reset, we only reconnect once, and then retry the queries.
      grpc :: !(IORef (Maybe (Word64, GrpcClient))),
      logger :: LoggerMethod,
      -- | A flag indicating that all the in-flight queries should be killed.
      --  |This is a workaround for the inadequate behaviour of the grpc library
      --  which does not handle disconnects from the server very well, and in
      --  particular it does not handle the server sending GoAway frames. Ideally
      --  in that scenario the library would either try to reconnect itself, or,
      --  alternatively, trigger a normal error that we could recover from and
      --  re-establish the connection. None of the two happen. So instead we
      --  install our own custom GoAway handler that kills all in-flight queries,
      --  and then re-establishes the connection.
      --
      --  This MVar will be empty when queries are progressing. When the queries
      --  need to be killed then we write to it. When we successfully
      --  re-establish the connection then the the MVar is again emptied.
      killConnection :: !(MVar ())
    }

-- | Monad in which the program would run
newtype ClientMonad m a = ClientMonad
    { _runClientMonad :: ReaderT EnvData (ExceptT ClientError (StateT CookieHeaders m)) a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadReader EnvData,
          MonadFail,
          MonadIO
        )

-- | Cookie headers that may be returned by the node in a query.
type CookieHeaders = Map.Map BS8.ByteString BS8.ByteString

-- | Execute the computation with the given environment (using the established connection).
runClient :: (Monad m) => EnvData -> ClientMonad m a -> m (Either ClientError a)
runClient config comp = evalStateT (runExceptT $ runReaderT (_runClientMonad comp) config) (Map.empty :: CookieHeaders)

-- | @runClient@ but with additional cookies set in the @GRPCRequest@.
--  The updated set of cookies (set via set-cookie headers) are returned.
runClientWithCookies :: CookieHeaders -> EnvData -> ClientMonad m a -> m (Either ClientError a, CookieHeaders)
runClientWithCookies hds cfg comp = runStateT (runExceptT $ runReaderT (_runClientMonad comp) cfg) hds

mkGrpcClient :: GrpcConfig -> Maybe LoggerMethod -> ClientIO EnvData
mkGrpcClient config mLogger =
    let header =
            case target config of
                Just t -> [("target", BS8.pack t)]
                Nothing -> []
        cfg =
            (grpcClientConfigSimple (host config) (port config) (useTls config))
                { _grpcClientConfigCompression = uncompressed,
                  _grpcClientConfigHeaders = header,
                  _grpcClientConfigTimeout = Timeout (fromMaybe 300 (timeout config))
                }
    in  liftIO $ do
            lock <- initializeLock
            ioref <- newIORef Nothing -- don't start the connection just now
            killConnection <- newEmptyMVar
            let logger = fromMaybe (const (return ())) mLogger
            return $! EnvData (retryNum config) cfg lock ioref logger killConnection

-- | Get and print the status of a transaction.
instance (MonadIO m) => TransactionStatusQuery (ClientMonad m) where
    queryTransactionStatus hash = do
        r <- getResponseValueOrDie =<< getBlockItemStatus hash
        case transactionStatusToTransactionStatusResult r of
            Left err -> logFatal ["Unable to query transaction status: " <> err]
            Right v -> return v
    wait t = liftIO $ do
        putChar '.'
        threadDelay $ t * 1_000_000

-- | Get all pending updates to chain parameters at the end of a given block.
getBlockPendingUpdates :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq PendingUpdate)))
getBlockPendingUpdates bhInput =
    withServerStreamCollect (call @"getBlockPendingUpdates") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all special events in a given block.
--  A special event is protocol generated event that is not directly caused by a transaction, such as minting, paying out rewards, etc.
getBlockSpecialEvents :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq Transactions.SpecialTransactionOutcome)))
getBlockSpecialEvents bhInput =
    withServerStreamCollect (call @"getBlockSpecialEvents") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all transaction events in a given block.
getBlockTransactionEvents :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq SupplementedTransactionSummary)))
getBlockTransactionEvents bhInput =
    withServerStreamCollect (call @"getBlockTransactionEvents") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all hashes of non-finalized transactions for a given account.
getAccountNonFinalizedTransactions :: (MonadIO m) => AccountAddress -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq TransactionHash)))
getAccountNonFinalizedTransactions accountAddress =
    withServerStreamCollect (call @"getAccountNonFinalizedTransactions") msg ((fmap . mapM) fromProto)
  where
    msg = toProto accountAddress

-- | Get all anonymity revokers registered at the end of a given block.
getAnonymityRevokers :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq ArInfo.ArInfo)))
getAnonymityRevokers bhInput =
    withServerStreamCollect (call @"getAnonymityRevokers") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all identity providers registered at the end of a given block.
getIdentityProviders :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq IpInfo.IpInfo)))
getIdentityProviders bhInput =
    withServerStreamCollect (call @"getIdentityProviders") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all fixed passive delegators for the reward period of a given block.
--  In contrast to @getPassiveDelegators@ which returns all delegators registered
--  at the end of a given block, this returns all fixed delegators contributing
--  stake in the reward period containing the given block.
getPassiveDelegatorsRewardPeriod :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq DelegatorRewardPeriodInfo)))
getPassiveDelegatorsRewardPeriod bhInput =
    withServerStreamCollect (call @"getPassiveDelegatorsRewardPeriod") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all registered passive delegators at the end of a given block.
getPassiveDelegators :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq DelegatorInfo)))
getPassiveDelegators bhInput =
    withServerStreamCollect (call @"getPassiveDelegators") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all fixed delegators of a given pool for the reward period of a given block.
--  In contrast to @getPoolDelegators@ which returns all active delegators registered
--  for the given block, this returns all the active fixed delegators contributing stake
--  in the reward period containing the given block.
getPoolDelegatorsRewardPeriod :: (MonadIO m) => BlockHashInput -> BakerId -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq DelegatorRewardPeriodInfo)))
getPoolDelegatorsRewardPeriod bhInput baker =
    withServerStreamCollect (call @"getPoolDelegatorsRewardPeriod") msg ((fmap . mapM) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.baker .~ toProto baker

-- | Get all registered delegators of a given pool at the end of a given block.
getPoolDelegators :: (MonadIO m) => BlockHashInput -> BakerId -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq DelegatorInfo)))
getPoolDelegators bhInput baker =
    withServerStreamCollect (call @"getPoolDelegators") msg ((fmap . mapM) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.baker .~ toProto baker

-- | Get IDs of all bakers at the end of a given block.
getBakerList :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq BakerId)))
getBakerList bhInput = withServerStreamCollect (call @"getBakerList") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get key-value pairs representing the entire state of a specific contract instance in a given block.
--  The resulting sequence consists of key-value pairs ordered lexicographically according to the keys.
getInstanceState :: (MonadIO m) => BlockHashInput -> ContractAddress -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq (ByteString, ByteString))))
getInstanceState bhInput cAddress =
    withServerStreamCollect (call @"getInstanceState") msg ((fmap . mapM) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.address .~ toProto cAddress

-- | Get the addresses of all smart contract instances in a given block.
getInstanceList :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq ContractAddress)))
getInstanceList bhInput = withServerStreamCollect (call @"getInstanceList") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get ancestors of a given block.
--  The first element of the sequence is the requested block itself, and the block
--  immediately following a block in the sequence is the parent of that block.
--  The sequence contains at most @limit@ blocks, and if the sequence is
--  strictly shorter, the last block in the list is the genesis block.
getAncestors :: (MonadIO m) => BlockHashInput -> Word64 -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq BlockHash)))
getAncestors bhInput limit = withServerStreamCollect (call @"getAncestors") msg ((fmap . mapM) fromProto)
  where
    msg =
        defMessage
            & ProtoFields.blockHash .~ toProto bhInput
            & ProtoFields.amount .~ limit

-- | Get all smart contract modules that exist at the end of a given block.
getModuleList :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq ModuleRef)))
getModuleList bhInput = withServerStreamCollect (call @"getModuleList") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Get all accounts that exist at the end of a given block.
getAccountList :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq AccountAddress)))
getAccountList bhInput = withServerStreamCollect (call @"getAccountList") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

-- | Process a stream of blocks that are finalized from the time the query is made onward.
--  This can be used to listen for newly finalized blocks. Note that there is no guarantee
--  that blocks will not be skipped if the client is too slow in processing the stream,
--  however blocks will always be sent by increasing block height. Note that this function
--  is non-terminating, so some care should be taken when invoking this. See @withGRPCCore@
--  for more info.
getFinalizedBlocks :: (MonadIO m) => (FromProtoResult ArrivedBlockInfo -> ClientIO ()) -> ClientMonad m (GRPCResult ())
getFinalizedBlocks f = withServerStreamCallback (call @"getFinalizedBlocks") defMessage mempty (\_ o -> f (fromProto o)) id

-- | Process a stream of blocks that arrive from the time the query is made onward.
--  This can be used to listen for incoming blocks. Note that this is non-terminating,
--  so some care should be taken when using this. See @withGRPCCore@ for more info.
getBlocks :: (MonadIO m) => (FromProtoResult ArrivedBlockInfo -> ClientIO ()) -> ClientMonad m (GRPCResult ())
getBlocks f = withServerStreamCallback (call @"getBlocks") defMessage mempty (\_ o -> f (fromProto o)) id

-- | Get cryptographic parameters in a given block.
getCryptographicParameters :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult Parameters.CryptographicParameters))
getCryptographicParameters bhInput = withUnary (call @"getCryptographicParameters") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get values of chain parameters in a given block.
getBlockChainParameters ::
    (MonadIO m) =>
    BlockHashInput ->
    ClientMonad m (GRPCResult (FromProtoResult EChainParametersAndKeys))
getBlockChainParameters bHash = do
    -- Get the foundation account address and the callback that allows for constructing the chain parameters.
    paramsOutput <- withUnary (call @"getBlockChainParameters") msg (fmap fromProto)
    let paramsM = case paramsOutput of
            RequestFailed err -> Left $ RequestFailed err
            StatusNotOk err -> Left $ StatusNotOk err
            StatusInvalid -> Left StatusInvalid
            StatusOk res -> Right $ grpcResponseVal res
    -- Get the account index from the account address to get the chain parameters.
    case paramsM of
        Left failed -> return failed
        Right res ->
            case res of
                Left err -> return $ RequestFailed $ "Could not convert response from GetChainParameters: " <> err
                Right (faAddr, toOutput) -> do
                    accInfoOutput <- getAccountInfo (AccAddress faAddr) bHash
                    case accInfoOutput of
                        RequestFailed err -> return $ RequestFailed err
                        StatusNotOk err -> return $ StatusNotOk err
                        StatusInvalid -> return StatusInvalid
                        StatusOk res' -> do
                            case grpcResponseVal res' of
                                Left err -> return $ RequestFailed $ "Could not convert response from GetAccountInfo: " <> err
                                Right ai -> do
                                    let chainParams = toOutput (aiAccountIndex ai)
                                    return $ StatusOk $ GRPCResponse (grpcHeaders res') chainParams
  where
    msg = toProto bHash

-- | Get information about the node. See @NodeInfo@ for details.
getNodeInfo :: (MonadIO m) => ClientMonad m (GRPCResult (FromProtoResult NodeInfo))
getNodeInfo = withUnary (call @"getNodeInfo") msg (fmap fromProto)
  where
    msg = defMessage

-- Get a list of the peers that the node is connected to and network-related information for each peer.
getPeersInfo :: (MonadIO m) => ClientMonad m (GRPCResult (FromProtoResult [PeerInfo]))
getPeersInfo = withUnary (call @"getPeersInfo") msg (fmap fromProto)
  where
    msg = defMessage

-- | Get a summary of the finalization data in a given block.
getBlockFinalizationSummary :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Maybe FinalizationSummary)))
getBlockFinalizationSummary bhInput = withUnary (call @"getBlockFinalizationSummary") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get the status of and information about a specific block item (transaction).
getBlockItemStatus :: (MonadIO m) => TransactionHash -> ClientMonad m (GRPCResult (FromProtoResult SupplementedTransactionStatus))
getBlockItemStatus tHash = withUnary (call @"getBlockItemStatus") msg (fmap fromProto)
  where
    msg = toProto tHash

-- | Send a block item. A block item is either an @AccountTransaction@, which is
--  a transaction signed and paid for by an account, a @CredentialDeployment@,
--  which creates a new account, or an @UpdateInstruction@, which is an
--  instruction to change some parameters of the chain. Update instructions can
--  only be sent by the governance committee.
--
--  Returns a hash of the block item, which can be used with
--  @GetBlockItemStatus@.
sendBlockItem :: (MonadIO m) => Transactions.BareBlockItem -> ClientMonad m (GRPCResult (FromProtoResult TransactionHash))
sendBlockItem bbiInput = withUnary (call @"sendBlockItem") msg (fmap fromProto)
  where
    msg = toProto bbiInput

-- | Get the value at a specific key of a contract state. In contrast to
--  @GetInstanceState@ this is more efficient, but requires the user to know
--  the specific key to look for.
instanceStateLookup ::
    (MonadIO m) =>
    BlockHashInput ->
    ContractAddress ->
    ByteString ->
    ClientMonad m (GRPCResult (FromProtoResult ByteString))
instanceStateLookup bhInput cAddr key =
    withUnary (call @"instanceStateLookup") msg (fmap fromProto)
  where
    msg =
        defMessage
            & ProtoFields.blockHash .~ toProto bhInput
            & ProtoFields.address .~ toProto cAddr
            & ProtoFields.key .~ key

-- | Stop dumping packets.
--  This feature is enabled if the node was built with the @network_dump@ feature.
--  Returns a GRPC error if the network dump could not be stopped.
dumpStop :: (MonadIO m) => ClientMonad m (GRPCResult ())
dumpStop = withUnary (call @"dumpStop") defMessage ((fmap . const) ())

-- | Start dumping network packets into the specified file.
--  This feature is enabled if the node was built with the @network_dump@ feature.
--  Returns a GRPC error if the network dump failed to start.
dumpStart :: (MonadIO m) => Text -> Bool -> ClientMonad m (GRPCResult ())
dumpStart file raw = withUnary (call @"dumpStart") msg ((fmap . const) ())
  where
    msg = defMessage & ProtoFields.file .~ file & ProtoFields.raw .~ raw

-- | Unban a peer. Returns a GRPC error if the action failed.
unbanPeer :: (MonadIO m) => Peer -> ClientMonad m (GRPCResult ())
unbanPeer peer = withUnary (call @"unbanPeer") msg ((fmap . const) ())
  where
    msg = defMessage & ProtoFields.ipAddress .~ toProto peer

-- | Ban a peer. Returns a GRPC error if the action failed.
banPeer :: (MonadIO m) => Peer -> ClientMonad m (GRPCResult ())
banPeer peer = withUnary (call @"banPeer") msg ((fmap . const) ())
  where
    msg = defMessage & ProtoFields.ipAddress .~ toProto peer

-- | Get a list of peers banned by the node.
getBannedPeers :: (MonadIO m) => ClientMonad m (GRPCResult (FromProtoResult [Peer]))
getBannedPeers = withUnary (call @"getBannedPeers") defMessage (fmap fromProto)

-- | Ask the node to disconnect from the peer with the submitted details.
--  On success, the peer is removed from the peer-list of the node and a
--  @GRPCResponse@ is returned. Otherwise a GRPC error is returned.
peerDisconnect :: (MonadIO m) => IpAddress -> IpPort -> ClientMonad m (GRPCResult ())
peerDisconnect ip port = withUnary (call @"peerDisconnect") msg ((fmap . const) ())
  where
    msg = defMessage & ProtoFields.ip .~ toProto ip & ProtoFields.port .~ toProto port

-- | Ask a peer to connect to the peer with the submitted details.
--  On success, the peer is added in the peer-list of the node and a
--  @GRPCResponse@ is returned. Otherwise a GRPC error is returned.
--  Note that the peer may not be connected instantly, in which case
--  the call succeeds.
peerConnect :: (MonadIO m) => IpAddress -> IpPort -> ClientMonad m (GRPCResult ())
peerConnect ip port = withUnary (call @"peerConnect") msg ((fmap . const) ())
  where
    msg = defMessage & ProtoFields.ip .~ toProto ip & ProtoFields.port .~ toProto port

-- | Shut down the node. Returns a GRPC error if the shutdown failed.
shutdown :: (MonadIO m) => ClientMonad m (GRPCResult ())
shutdown = withUnary (call @"shutdown") defMessage ((fmap . const) ())

-- | Get next available sequence numbers for updating chain parameters after a given block.
getNextUpdateSequenceNumbers :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult NextUpdateSequenceNumbers))
getNextUpdateSequenceNumbers bhInput = withUnary (call @"getNextUpdateSequenceNumbers") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get all accounts that have scheduled releases.
getScheduledReleaseAccounts :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq AccountPending)))
getScheduledReleaseAccounts bhInput =
    withServerStreamCollect (call @"getScheduledReleaseAccounts") msg (fmap (mapM fromProto))
  where
    msg = toProto bhInput

-- | Get all accounts that have stake in cooldown.
getCooldownAccounts :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq AccountPending)))
getCooldownAccounts bhInput =
    withServerStreamCollect (call @"getCooldownAccounts") msg (fmap (mapM fromProto))
  where
    msg = toProto bhInput

-- | Get all accounts that have stake in pre-cooldown.
getPreCooldownAccounts :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq AccountIndex)))
getPreCooldownAccounts bhInput =
    withServerStreamCollect (call @"getPreCooldownAccounts") msg (fmap (mapM fromProto))
  where
    msg = toProto bhInput

-- | Get all accounts that have stake in pre-pre-cooldown.
getPrePreCooldownAccounts :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq AccountIndex)))
getPrePreCooldownAccounts bhInput =
    withServerStreamCollect (call @"getPrePreCooldownAccounts") msg (fmap (mapM fromProto))
  where
    msg = toProto bhInput

-- | Get information related to the baker election for a particular block.
getElectionInfo :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult BlockBirkParameters))
getElectionInfo bhInput = withUnary (call @"getElectionInfo") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get the current branches of blocks starting from and including the last finalized block.
getBranches :: (MonadIO m) => ClientMonad m (GRPCResult (FromProtoResult Branch))
getBranches = withUnary (call @"getBranches") defMessage (fmap fromProto)

-- | Run the smart contract entrypoint in a given context and in the state at the end of a given block.
invokeInstance ::
    (MonadIO m) =>
    BlockHashInput ->
    InvokeContract.ContractContext ->
    ClientMonad m (GRPCResult (FromProtoResult InvokeContract.InvokeContractResult))
invokeInstance bhInput cContext = withUnary (call @"invokeInstance") msg (fmap fromProto)
  where
    msg = toProto (bhInput, cContext)

-- | Get information about tokenomics at the end of a given block.
getTokenomicsInfo :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult RewardStatus))
getTokenomicsInfo bhInput = withUnary (call @"getTokenomicsInfo") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get a list of live blocks at a given height.
getBlocksAtHeight :: (MonadIO m) => BlockHeightInput -> ClientMonad m (GRPCResult (FromProtoResult [BlockHash]))
getBlocksAtHeight blockHeight = withUnary (call @"getBlocksAtHeight") msg (fmap fromProto)
  where
    msg = toProto blockHeight

-- | Get information about the passive delegators at the end of a given block.
getPassiveDelegationInfo :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult PassiveDelegationStatus))
getPassiveDelegationInfo bhInput = withUnary (call @"getPassiveDelegationInfo") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get information about a given pool at the end of a given block.
getPoolInfo :: (MonadIO m) => BlockHashInput -> BakerId -> ClientMonad m (GRPCResult (FromProtoResult BakerPoolStatus))
getPoolInfo bhInput baker = withUnary (call @"getPoolInfo") msg (fmap fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.baker .~ toProto baker

-- | Get information, such as height, timings, and transaction counts for a given block.
getBlockInfo :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult BlockInfo))
getBlockInfo bhInput = withUnary (call @"getBlockInfo") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get information about the current state of consensus.
getConsensusInfo :: (MonadIO m) => ClientMonad m (GRPCResult (FromProtoResult ConsensusStatus))
getConsensusInfo = withUnary (call @"getConsensusInfo") defMessage (fmap fromProto)

-- | Get detailed consensus state information (for consensus version 1).
getConsensusDetailedStatus :: (MonadIO m) => Maybe GenesisIndex -> ClientMonad m (GRPCResult (FromProtoResult ConsensusDetailedStatus))
getConsensusDetailedStatus mGenesisIndex = withUnary (call @"getConsensusDetailedStatus") msg (fmap fromProto)
  where
    msg = defMessage & ProtoFields.maybe'genesisIndex .~ fmap toProto mGenesisIndex

-- | Get the source of a smart contract module.
getModuleSource :: (MonadIO m) => ModuleRef -> BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult Wasm.WasmModule))
getModuleSource modRef bhInput = withUnary (call @"getModuleSource") msg (fmap fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.moduleRef .~ toProto modRef

-- | Retrieve the account information from the chain.
getAccountInfo :: (MonadIO m) => AccountIdentifier -> BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult Concordium.Types.AccountInfo))
getAccountInfo account bhInput = withUnary (call @"getAccountInfo") msg (fmap fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.accountIdentifier .~ toProto account

getInstanceInfo :: (MonadIO m) => ContractAddress -> BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult Wasm.InstanceInfo))
getInstanceInfo cAddress bhInput = withUnary (call @"getInstanceInfo") msg (fmap fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto bhInput & ProtoFields.address .~ toProto cAddress

getNextSequenceNumber :: (MonadIO m) => AccountAddress -> ClientMonad m (GRPCResult (FromProtoResult NextAccountNonce))
getNextSequenceNumber accAddress = withUnary (call @"getNextAccountSequenceNumber") msg (fmap fromProto)
  where
    msg = toProto accAddress

-- | Retrieve a stream of 'BakerRewardPeriodInfo' given the input.
getBakersRewardPeriod :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq BakerRewardPeriodInfo)))
getBakersRewardPeriod bhInput = withServerStreamCollect (call @"getBakersRewardPeriod") msg ((fmap . mapM) fromProto)
  where
    msg = toProto bhInput

getBlockCertificates :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (FromProtoResult KonsensusV1.BlockCertificates))
getBlockCertificates bhInput = withUnary (call @"getBlockCertificates") msg (fmap fromProto)
  where
    msg = toProto bhInput

-- | Get the earliest time at which a baker may be expected to bake a block.
getBakerEarliestWinTime :: (MonadIO m) => BakerId -> ClientMonad m (GRPCResult (FromProtoResult Timestamp))
getBakerEarliestWinTime bakerId = withUnary (call @"getBakerEarliestWinTime") msg (fmap fromProto)
  where
    msg = toProto bakerId

getWinningBakersEpoch :: (MonadIO m) => EpochRequest -> ClientMonad m (GRPCResult (FromProtoResult (Seq.Seq WinningBaker)))
getWinningBakersEpoch epochReq = withServerStreamCollect (call @"getWinningBakersEpoch") msg ((fmap . mapM) fromProto)
  where
    msg = toProto epochReq

getFirstBlockEpoch :: (MonadIO m) => EpochRequest -> ClientMonad m (GRPCResult (FromProtoResult BlockHash))
getFirstBlockEpoch epochReq = withUnary (call @"getFirstBlockEpoch") msg (fmap fromProto)
  where
    msg = toProto epochReq

-- | Call a unary V2 GRPC API endpoint and return the result.
withUnary ::
    ( HasMethod CS.Queries m,
      MonadIO n,
      i ~ MethodInput CS.Queries m,
      o ~ MethodOutput CS.Queries m
    ) =>
    -- | The procedure to call.
    RPC CS.Queries m ->
    -- | The procedure input.
    i ->
    -- | A mapping of the result.
    (GRPCResult o -> b) ->
    ClientMonad n b
withUnary method input k = withGRPCCore callHelper k
  where
    -- This is here so we may leverage @withGRPCCore@.
    callHelper client = do
        res <- rawUnary method client input
        return $ fmap RawUnaryOutput res

-- | Call a streaming V2 GRPC API endpoint and return the (collected) results in a sequence.
--  Note that some care should be taken when using this with long-running calls. See
--  @withGRPCCore@ for more info.
withServerStreamCollect ::
    ( HasMethod CS.Queries m,
      MonadIO n,
      i ~ MethodInput CS.Queries m,
      o ~ Seq.Seq (MethodOutput CS.Queries m)
    ) =>
    -- | The procedure to call.
    RPC CS.Queries m ->
    -- | The procedure input.
    i ->
    -- | A mapping of the collected result.
    (GRPCResult o -> b) ->
    ClientMonad n b
withServerStreamCollect method input =
    withServerStreamCallback method input mempty handler
  where
    handler acc o = return $ acc <> pure o

-- | Call a streaming GRPC API endpoint and return the (collected) results.
--  Takes a @fold@-like callback and an accumulator, and returns the result
--  of folding through each stream element, once the stream terminates. Note
--  that some care should be taken when using this with long-running calls.
--  See @withGRPCCore@ for more info.
withServerStreamCallback ::
    ( HasMethod CS.Queries m,
      MonadIO n,
      i ~ MethodInput CS.Queries m,
      o ~ MethodOutput CS.Queries m
    ) =>
    -- | The procedure to call.
    RPC CS.Queries m ->
    -- | The procedure input.
    i ->
    -- | An initial @fold@-like accumulator that
    --  is updated with the handler each time a
    --  stream element arrives.
    a ->
    -- | A @fold@-like handler which is used
    --  to process a stream object and update
    --  the accumulator.
    (a -> o -> ClientIO a) ->
    -- | A mapping of the accumulated result.
    (GRPCResult a -> b) ->
    ClientMonad n b
withServerStreamCallback method input acc handler =
    withGRPCCore callHelper
  where
    -- This is simply a handler which conforms to the one required by
    -- @rawStreamServer@. This is here, so we may ignore the response
    -- headers in the handler, just for the sake of ergonomics.
    handler' acc' _hds streamObj = handler acc' streamObj
    -- Helper that invokes the streaming call with the above callback.
    -- This is here so we may leverage @withGRPCCore@.
    callHelper client = do
        res <- rawStreamServer method client acc input handler'
        return $ fmap ServerStreamOutput res

-- | Run a request helper function with a client instance to call a GRPC procedure.
--  The output is interpreted using the function given in the second parameter.
--
--  Note that long-running streaming call may block other calls from retrying to
--  establish their connection if they fail. Therefore some care should be taken
--  when using long-running or unproductive calls, and in particular those targeting
--  never-ending streaming endpoints such as @getBlocks@ and @getFinalizedBlocks@.
withGRPCCore ::
    (MonadIO n) =>
    -- | A helper which takes a client, issues a GRPC request in the client and returns the result.
    (GrpcClient -> ClientIO (Either TooMuchConcurrency (GRPCOutput b))) ->
    -- | A mapping of the result.
    (GRPCResult b -> t) ->
    ClientMonad n t
withGRPCCore helper k = do
    clientRef <- asks grpc
    cfg <- asks config
    lock <- asks rwlock
    logm <- asks logger
    cookies <- ClientMonad (lift get)
    mv <- asks killConnection
    -- FIXME: Timeout should probably be handled differently when
    --        invoking a tailing procedure such as @getBlocks@.
    let Timeout timeoutSeconds = _grpcClientConfigTimeout cfg
    -- try to establish a connection
    let tryEstablish :: Int -> IO (Maybe GrpcClient)
        tryEstablish n = do
            logm $ "Trying to establish connection, n = " <> Text.pack (show n)
            let cfg' =
                    cfg
                        { _grpcClientConfigGoAwayHandler = \x -> do
                            liftIO (logm ("Receive GOAWAY message: " <> fromString (show x)))
                            -- use tryPutMVar instead of putMVar since multiple queries might do this at the same time.
                            -- That should not matter since once an MVar is set we will kill the existing client.
                            _ <- liftIO (tryPutMVar mv ())
                            return ()
                        }
            if n <= 0
                then return Nothing
                else
                    try @IOException (runExceptT (setupGrpcClient cfg')) >>= \case
                        Right (Right client) -> return (Just client)
                        _ -> do
                            -- retry in case of error or exception, after waiting 1s
                            threadDelay 1_000_000
                            tryEstablish (n - 1)

    let tryRun =
            withReadLock lock $ do
                logm "Running gRPC query."
                mclient <- readIORef clientRef
                case mclient of
                    Nothing -> do
                        logm "No network client."
                        -- using 0 here means that the generation check below will always
                        -- yield False, in case the connection is established by another
                        -- query from this point until the retry. And thus that client
                        -- will be used next time.
                        return (0, Left Retry)
                    Just (gen, client) -> do
                        -- if the MVar is not set then we are free to attempt a new query.
                        -- If it is set then it means a GOAWAY frame is being handled. We
                        -- fail here (the Just () branch) and will try the next time after
                        -- a new client has been established.
                        tryTakeMVar mv >>= \case
                            Nothing -> do
                                logm "Network client exists, running query."
                                -- Overwrite the headers in the client with existing ones in the config.
                                -- This makes it possible to supply per-request headers.
                                let client' = client{_grpcClientHeaders = _grpcClientConfigHeaders cfg ++ fmap (\(x, y) -> ("Cookie", x <> "=" <> y)) (Map.toList cookies)}
                                let runRPC =
                                        runExceptT (helper client')
                                            >>= \case
                                                Left err -> Left Retry <$ logm ("Network error: " <> fromString (show err)) -- client error
                                                Right (Left err) -> Left (DoNotRetry (StatusNotOk (RESOURCE_EXHAUSTED, "Too many concurrent requests."))) <$ logm ("Too much concurrency: " <> fromString (show err))
                                                Right (Right x) -> return (Right x)
                                race (race (readMVar mv) (threadDelay (timeoutSeconds * 1000000))) runRPC
                                    >>= \case
                                        Left (Left ()) -> (gen, Left Retry) <$ logm "Terminating query because GOAWAY received."
                                        Left (Right ()) -> (gen, Left (DoNotRetry (StatusNotOk (DEADLINE_EXCEEDED, "Query timed out.")))) <$ logm "Terminating query because it timed out."
                                        Right x -> return (gen, x)
                            Just () -> return (gen, Left Retry) -- fail this round, go again after the client is established.
    ret <- liftIO tryRun

    case ret of
        (usedGen, Left Retry) -> do
            -- failed, need to establish connection
            liftIO (logm "gRPC call failed. Will try to reestablish connection.")
            retryNum <- asks retryTimes
            tryAgain <- liftIO $! withWriteLock lock $ do
                reEstablish <-
                    readIORef clientRef >>= \case
                        Nothing -> return (Just 1)
                        Just (curGen, oldClient) | usedGen >= curGen -> do
                            err <- try @SomeException (runExceptT (close oldClient))
                            case err of
                                Left exc -> logm (fromString ("An exception occurred while closing the connection: " ++ show exc))
                                Right (Left cerr) -> logm (fromString ("An error occurred while closing the connection: " ++ show cerr))
                                Right (Right ()) -> return ()
                            -- We close the connection here regardless of whether errors or
                            -- exceptions occurred.
                            --
                            -- Clear the GOAWAY signal. Since this is happening under a write
                            -- lock it will clear out the MVar since the MVar is only set
                            -- under a read lock. The reason for using tryTakeMVar instead of
                            -- takeMVar is that we could be in this state because a query
                            -- timed out, not necessarily because we got a GOAWAY messages.
                            _ <- tryTakeMVar mv
                            return (Just (curGen + 1))
                        Just _ -> return Nothing
                case reEstablish of
                    Nothing -> return True
                    Just newGen ->
                        tryEstablish (retryNum + 1) >>= \case
                            Nothing -> do
                                atomicWriteIORef clientRef Nothing
                                return False
                            Just newClient -> do
                                logm "Established a connection."
                                atomicWriteIORef clientRef (Just (newGen, newClient))
                                return True
            if tryAgain
                then do
                    liftIO (logm "Reestablished connection, trying again.")
                    (_, ret') <- liftIO tryRun
                    let response = toGRPCResult ret'
                    addHeaders response
                    return $ k response
                else return $ k (RequestFailed "Cannot establish connection to GRPC endpoint.")
        (_, Left (DoNotRetry r)) -> do
            return (k r)
        (_, Right v) ->
            let response = toGRPCResult' v
            in  do
                    addHeaders response
                    return (k response)
  where
    addHeaders response = case response of
        StatusOk GRPCResponse{..} -> do
            ClientMonad $ do
                forM_ grpcHeaders $ \(hn, hv) ->
                    when (hn == "set-cookie") $
                        let c = Cookie.parseSetCookie hv
                        in  modify' (Map.insert (Cookie.setCookieName c) (Cookie.setCookieValue c))
        _ -> return ()

call :: forall m. RPC CS.Queries m
call = RPC @CS.Queries @m
