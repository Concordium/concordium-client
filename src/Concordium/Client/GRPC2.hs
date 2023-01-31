{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- FIXME: My fourmulo is acting up, so I need to add this for now. I will remove it later.
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Part of the implementation of the GRPC2 interface.
module Concordium.Client.GRPC2 where

import Control.Concurrent
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.Int
import Data.Map.Strict qualified as Map
import Data.ProtoLens.Combinators qualified as Proto
import Data.ProtoLens.Field qualified
import Data.Ratio qualified as Ratio
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Data.Word
import Lens.Micro.Platform

import Proto.V2.Concordium.Types qualified as Proto
import Proto.V2.Concordium.Types_Fields qualified as ProtoFields

import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Queries qualified as QueryTypes
import Concordium.Types.Transactions qualified as Transactions

import Concordium.Common.Time
import Concordium.Common.Version
import Concordium.Crypto.SignatureScheme (Signature (..), VerifyKey (..))
import Concordium.ID.AnonymityRevoker qualified as ArInfo
import Concordium.ID.IdentityProvider qualified as IpInfo
import Concordium.Types.Accounts.Releases
import Concordium.Types.Block (AbsoluteBlockHeight (..))
import Concordium.Types.Execution
import Concordium.Types.InvokeContract qualified as InvokeContract
import Concordium.Types.Parameters qualified as Parameters
import Concordium.Types.Updates qualified as Updates
import Concordium.Wasm qualified as Wasm
import Data.Text (Text, pack)
import Data.Time (UTCTime)

import Concordium.Client.GRPC
import Concordium.Client.Runner.Helper
import Control.Exception
import Network.GRPC.Client
import Network.GRPC.Client.Helpers hiding (Address)
import Network.GRPC.HTTP2.ProtoLens
import Network.HTTP2.Client
import Web.Cookie qualified as Cookie

import Concordium.GRPC2 hiding (Output')
import Concordium.ID.AnonymityRevoker (createArInfo)
import Concordium.ID.IdentityProvider (createIpInfo)
import Concordium.ID.Parameters (createGlobalContext)
import Concordium.Types.Accounts qualified as Concordium.Types
import Concordium.Types.InvokeContract (ContractContext (ContractContext))
import Concordium.Types.Parameters (CryptographicParameters)
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (fromJust, maybeToList)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field qualified as Field
import Data.ProtoLens.Service.Types
import Data.Sequence (Seq (Empty))
import Data.String
import Data.Text qualified as Text
import Proto.V2.Concordium.Service qualified as CS
import Proto.V2.Concordium.Types qualified as ProtoFields
import Proto.V2.Concordium.Types_Fields qualified as Proto

-- |A helper function that serves as an inverse to `mkSerialize`,
--
-- Converts a protocol buffer wrapper type to a native Haskell type
-- which is a member of `Serialize`.
--
-- More concretely, the wrapper type should be of the form
--
-- > message Wrapper {
-- >    bytes value = 1
-- > }
--
-- where the name @Wrapper@ can be arbitrary, but the @value@ field must exist,
-- and it must have type @bytes@.
deMkSerialize ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        BS.ByteString,
      S.Serialize a
    ) =>
    b ->
    Maybe a
deMkSerialize val = case S.decode $ val ^. ProtoFields.value of
    Left _ -> Nothing
    Right v -> v

-- |Like 'deMkSerialize' above, but for Word64 fields.
deMkWord64 ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        Word64
    ) =>
    b ->
    Word64
deMkWord64 val = coerce $ val ^. ProtoFields.value

-- |Like 'deMkWord64' above, but for Word32 fields instead.
deMkWord32 ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Word32
deMkWord32 val = coerce $ val ^. ProtoFields.value

-- |Like 'deMkWord64' above, but for Word16 fields instead.
deMkWord16 ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Maybe Word16
deMkWord16 val =
    if v <= fromIntegral (maxBound :: Word16)
        then return $ fromIntegral v
        else Nothing
  where
    v = val ^. ProtoFields.value

-- |Like 'mkWord32', but for Word8 fields instead.
deMkWord8 ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Maybe Word8
deMkWord8 val =
    if v <= fromIntegral (maxBound :: Word8)
        then return $ fromIntegral v
        else Nothing
  where
    v = val ^. ProtoFields.value

-- |Validate a protocol buffer message with an `url` Text field.
-- Returns @Nothing@ if the string is too long and the text
-- wrapped in @UrlText@ otherwise.
deMkUrlText ::
    ( Data.ProtoLens.Field.HasField
        b
        "url"
        Text
    ) =>
    b ->
    Maybe UrlText
deMkUrlText val =
    if Text.length v <= fromIntegral maxUrlTextLength
        then return $ UrlText v
        else Nothing
  where
    v = val ^. ProtoFields.url

-- |A helper class analogous to something like Aeson's FromJSON.
-- It exists to make it more manageable to convert the Protobuf
-- types to their internal Haskell type equivalents.
class FromProto a where
    -- |The corresponding Haskell type.
    type Output' a

    -- |A conversion function from the protobuf type to its Haskell
    -- equivalent. Returns Nothing if the conversion failed.
    fromProto :: a -> Maybe (Output' a)

-- |A block identifier.
-- A block is either identified via a hash, or as one of the special
-- blocks at a given time (last final or best block). Queries which
-- just need the recent state can use `LastFinal` or `Best` to get the
-- result without first establishing what the last final or best block
-- is.
data BlockHashInput = Best | LastFinal | Given !BlockHash

-- |An account identifier input
type AccountIdentifierInput = AccountIdentifier

instance FromProto Proto.AccountIndex where
    type Output' Proto.AccountIndex = AccountIndex
    fromProto = return . AccountIndex . deMkWord64

instance FromProto Proto.AccountAddress where
    type Output' Proto.AccountAddress = AccountAddress
    fromProto = deMkSerialize

instance FromProto Proto.CredentialRegistrationId where
    type Output' Proto.CredentialRegistrationId = (CredentialRegistrationID, RawCredentialRegistrationID)
    fromProto crId = do
        raw <- deMkSerialize crId
        nonRaw <- credIdFromRaw raw
        return (nonRaw, raw)

instance FromProto Proto.AccountIdentifierInput where
    type Output' Proto.AccountIdentifierInput = AccountIdentifierInput
    fromProto a = do
        aii <- a ^. ProtoFields.maybe'accountIdentifierInput
        case aii of
            Proto.AccountIdentifierInput'AccountIndex accIdx -> AccIndex <$> fromProto accIdx
            Proto.AccountIdentifierInput'Address addr -> AccAddress <$> fromProto addr
            Proto.AccountIdentifierInput'CredId cId -> CredRegID . snd <$> fromProto cId

instance ToProto AccountIdentifierInput where
    type Output AccountIdentifierInput = Proto.AccountIdentifierInput
    toProto = \case
        CredRegID cred -> Proto.make $ ProtoFields.credId .= toProto cred
        AccAddress addr -> Proto.make $ ProtoFields.address .= toProto addr
        AccIndex accIdx -> Proto.make $ ProtoFields.accountIndex .= toProto accIdx

instance FromProto Proto.SequenceNumber where
    type Output' Proto.SequenceNumber = Nonce

    -- The nonce can not be 0.
    fromProto sn =
        if word == 0
            then Nothing
            else return $ Nonce word
      where
        word = deMkWord64 sn

instance FromProto Proto.ReleaseSchedule where
    type Output' Proto.ReleaseSchedule = AccountReleaseSummary
    fromProto ars = do
        releaseTotal <- fromProto =<< ars ^? ProtoFields.total
        releaseSchedule <- do
            schedules <- ars ^? ProtoFields.schedules
            mapM fromProto schedules
        return AccountReleaseSummary{..}

instance FromProto Proto.Timestamp where
    type Output' Proto.Timestamp = Timestamp
    fromProto t = do
        time <- fromIntegral <$> t ^? ProtoFields.value
        return $ Timestamp time

instance FromProto Proto.TransactionHash where
    type Output' Proto.TransactionHash = TransactionHashV0
    fromProto = deMkSerialize

instance FromProto Proto.Release where
    type Output' Proto.Release = ScheduledRelease
    fromProto r = do
        releaseAmount <- fromProto =<< r ^? ProtoFields.amount
        releaseTimestamp <- do
            time <- r ^? ProtoFields.timestamp
            fromProto time
        releaseTransactions <- do
            ts <- r ^? ProtoFields.transactions
            mapM fromProto ts
        return ScheduledRelease{..}

instance FromProto Proto.AccountThreshold where
    type Output' Proto.AccountThreshold = AccountThreshold
    fromProto = fmap AccountThreshold . deMkWord8

instance FromProto Proto.EncryptedAmount where
    type Output' Proto.EncryptedAmount = EncryptedAmount
    fromProto = deMkSerialize

instance FromProto Proto.EncryptedBalance where
    type Output' Proto.EncryptedBalance = AccountEncryptedAmount
    fromProto eb = do
        _selfAmount <- fromProto =<< eb ^? ProtoFields.selfAmount
        _startIndex <- do
            si <- eb ^? ProtoFields.startIndex
            return $ EncryptedAmountAggIndex . fromIntegral $ si
        let _aggregatedAmount = do
                aa <- fromProto =<< eb ^. ProtoFields.maybe'aggregatedAmount
                na <- eb ^. ProtoFields.maybe'numAggregated
                return (aa, na)
        -- FIXME: This was not present previously due to a node bug.
        -- Bump submodule and fix before merging.
        let _incomingEncryptedAmounts = Empty
        return AccountEncryptedAmount{..}

instance FromProto Proto.EncryptionKey where
    type Output' Proto.EncryptionKey = AccountEncryptionKey
    fromProto = deMkSerialize

instance FromProto Proto.BakerId where
    type Output' Proto.BakerId = BakerId
    fromProto = return . BakerId . AccountIndex . deMkWord64

instance FromProto Proto.DelegatorId where
    type Output' Proto.DelegatorId = DelegatorId
    fromProto dId = fmap DelegatorId . fromProto =<< dId ^? ProtoFields.id

instance FromProto Proto.DelegationTarget where
    type Output' Proto.DelegationTarget = DelegationTarget
    fromProto dTarget = do
        dt <- dTarget ^. Proto.maybe'target
        case dt of
            Proto.DelegationTarget'Passive _ -> return DelegatePassive
            Proto.DelegationTarget'Baker baker -> DelegateToBaker <$> fromProto baker

instance FromProto Proto.StakePendingChange where
    type Output' Proto.StakePendingChange = StakePendingChange' Timestamp
    fromProto pc = do
        spc <- pc ^. Proto.maybe'change
        case spc of
            Proto.StakePendingChange'Reduce' reduce -> do
                ns <- fromProto =<< reduce ^? ProtoFields.newStake
                ts <- fromProto =<< reduce ^? ProtoFields.effectiveTime
                return $ ReduceStake ns ts
            Proto.StakePendingChange'Remove remove -> do
                rm <- fromProto remove
                return $ RemoveStake rm

instance FromProto Proto.BakerInfo where
    type Output' Proto.BakerInfo = BakerInfo
    fromProto bInfo = do
        _bakerIdentity <- fromProto =<< bInfo ^? ProtoFields.bakerId
        _bakerElectionVerifyKey <- fromProto =<< bInfo ^? ProtoFields.electionKey
        _bakerSignatureVerifyKey <- fromProto =<< bInfo ^? ProtoFields.signatureKey
        _bakerAggregationVerifyKey <- fromProto =<< bInfo ^? ProtoFields.aggregationKey
        return BakerInfo{..}

instance FromProto Proto.BakerSignatureVerifyKey where
    type Output' Proto.BakerSignatureVerifyKey = BakerSignVerifyKey
    fromProto = deMkSerialize

instance FromProto Proto.BakerElectionVerifyKey where
    type Output' Proto.BakerElectionVerifyKey = BakerElectionVerifyKey
    fromProto = deMkSerialize

instance FromProto Proto.BakerAggregationVerifyKey where
    type Output' Proto.BakerAggregationVerifyKey = BakerAggregationVerifyKey
    fromProto = deMkSerialize

instance FromProto Proto.OpenStatus where
    type Output' Proto.OpenStatus = OpenStatus
    fromProto = \case
        Proto.OPEN_STATUS_OPEN_FOR_ALL -> return OpenForAll
        Proto.OPEN_STATUS_CLOSED_FOR_NEW -> return ClosedForNew
        Proto.OPEN_STATUS_CLOSED_FOR_ALL -> return ClosedForAll
        ProtoFields.OpenStatus'Unrecognized _ -> Nothing

instance FromProto Proto.BakerPoolInfo where
    type Output' Proto.BakerPoolInfo = BakerPoolInfo
    fromProto poolInfo = do
        _poolOpenStatus <- fromProto =<< poolInfo ^? ProtoFields.openStatus
        _poolMetadataUrl <- deMkUrlText poolInfo
        _poolCommissionRates <- fromProto =<< poolInfo ^? ProtoFields.commissionRates
        return BakerPoolInfo{..}

instance FromProto Proto.AmountFraction where
    type Output' Proto.AmountFraction = AmountFraction

    -- This must not exceed 100000. We check it here since
    -- `makePartsPerHundredThousand` referenced in
    -- `makeAmountFraction` assumes this.
    fromProto amountFraction = do
        af <- amountFraction ^? ProtoFields.partsPerHundredThousand
        if af > 100_000
            then Nothing
            else return $ makeAmountFraction af

instance FromProto Proto.CommissionRates where
    type Output' Proto.CommissionRates = CommissionRates
    fromProto commissionRates = do
        _finalizationCommission <- fromProto =<< commissionRates ^? ProtoFields.finalization
        _bakingCommission <- fromProto =<< commissionRates ^? ProtoFields.baking
        _transactionCommission <- fromProto =<< commissionRates ^? ProtoFields.transaction
        return CommissionRates{..}

instance FromProto Proto.AccountStakingInfo where
    type Output' Proto.AccountStakingInfo = AccountStakingInfo
    fromProto si = do
        asi <- si ^. Proto.maybe'stakingInfo
        case asi of
            Proto.AccountStakingInfo'Delegator' delegator -> do
                asiStakedAmount <- fromProto =<< delegator ^? ProtoFields.stakedAmount
                asiStakeEarnings <- delegator ^? ProtoFields.restakeEarnings
                asiDelegationTarget <- fromProto =<< delegator ^? ProtoFields.target
                asiDelegationPendingChange <-
                    case delegator ^. ProtoFields.maybe'pendingChange of
                        Nothing -> return NoChange
                        Just ch -> fmap timestampToUTCTime <$> fromProto ch
                return AccountStakingDelegated{..}
            Proto.AccountStakingInfo'Baker' baker -> do
                asiStakedAmount <- fromProto =<< baker ^? ProtoFields.stakedAmount
                asiStakeEarnings <- baker ^? ProtoFields.restakeEarnings
                asiBakerInfo <- fromProto =<< baker ^? ProtoFields.bakerInfo
                asiPendingChange <-
                    case baker ^. ProtoFields.maybe'pendingChange of
                        Nothing -> return NoChange
                        Just ch -> fmap timestampToUTCTime <$> fromProto ch
                let asiPoolInfo = fromProto =<< baker ^. ProtoFields.maybe'poolInfo
                return AccountStakingBaker{..}

instance FromProto Proto.ArThreshold where
    type Output' Proto.ArThreshold = Threshold
    fromProto t =
        -- This can not be 0.
        if t == 0
            then Nothing
            else fmap Threshold . deMkWord8

instance FromProto (Map.Map Word32 Proto.ChainArData) where
    type Output' (Map.Map Word32 Proto.ChainArData) = Map.Map ArIdentity ChainArData
    fromProto m = do
        converted <- mapM convert $ Map.toAscList m
        return $ Map.fromAscList converted
      where
        convert :: (Word32, Proto.ChainArData) -> Maybe (ArIdentity, ChainArData)
        convert (arId, chainArData) = do
            arBytes <- chainArData ^? ProtoFields.encIdCredPubShare
            encId <- case S.decode arBytes of
                Left _ -> Nothing
                Right val -> val
            return (ArIdentity arId, encId)

instance FromProto Proto.Commitment where
    type Output' Proto.Commitment = Commitment
    fromProto = deMkSerialize

instance FromProto Proto.CredentialCommitments where
    type Output' Proto.CredentialCommitments = CredentialDeploymentCommitments
    fromProto cdc = do
        cmmPrf <- fromProto =<< cdc ^? ProtoFields.prf
        cmmCredCounter <- fromProto =<< cdc ^? ProtoFields.credCounter
        cmmMaxAccounts <- fromProto =<< cdc ^? ProtoFields.maxAccounts
        cmmAttributes <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                =<< cdc ^? ProtoFields.attributes
        cmmIdCredSecSharingCoeff <- mapM fromProto =<< cdc ^? ProtoFields.idCredSecSharingCoeff
        return CredentialDeploymentCommitments{..}
      where
        convert (aTag, comm) = do
            c <- fromProto comm
            return (AttributeTag $ fromIntegral aTag, c)

instance FromProto Proto.AccountCredential where
    type Output' Proto.AccountCredential = RawAccountCredential
    fromProto ac = do
        a <- ac ^. Proto.maybe'credentialValues
        case a of
            Proto.AccountCredential'Initial initial -> do
                icdvAccount <- fromProto =<< initial ^? ProtoFields.keys
                icdvRegId <- fmap snd . fromProto =<< initial ^? ProtoFields.credId
                icdvIpId <- fromProto =<< initial ^? ProtoFields.ipId
                icdvPolicy <- fromProto =<< initial ^? ProtoFields.policy
                return $ InitialAC InitialCredentialDeploymentValues{..}
            Proto.AccountCredential'Normal normal -> do
                cdvPublicKeys <- fromProto =<< normal ^? ProtoFields.keys
                cdvCredId <- fmap snd . fromProto =<< normal ^? ProtoFields.credId
                cdvIpId <- fromProto =<< normal ^? ProtoFields.ipId
                cdvPolicy <- fromProto =<< normal ^? ProtoFields.policy
                cdvThreshold <- fromProto =<< normal ^? ProtoFields.arThreshold
                cdvArData <- fromProto =<< normal ^? ProtoFields.arData
                commitments <- fromProto =<< normal ^? ProtoFields.commitments
                return $ NormalAC CredentialDeploymentValues{..} commitments

instance FromProto Proto.YearMonth where
    type Output' Proto.YearMonth = YearMonth
    fromProto yearMonth = do
        ymYear <- fromIntegral <$> yearMonth ^? ProtoFields.year
        ymMonth <- fromIntegral <$> yearMonth ^? ProtoFields.month
        return YearMonth{..}

instance FromProto Proto.Policy where
    type Output' Proto.Policy = Policy
    fromProto p = do
        pCreatedAt <- fromProto =<< p ^? ProtoFields.createdAt
        pValidTo <- fromProto =<< p ^? ProtoFields.validTo
        pItems <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                =<< p ^? ProtoFields.attributes
        return Policy{..}
      where
        -- \|Convert a tag and a bytestring pair to an attribute tag and attribute value pair.
        -- The length of the bytestring should be at most 31 and the tag should fit in an `Word8`.
        convert (aTag, aVal) =
            -- FIXME: Should we check that aTag matches
            --        the length of the bytestring? It is
            --        not clear to me whether this equals
            --        the length.
            if fromIntegral aTag <= (maxBound :: Word8)
                then do
                    -- FIXME: Should we get aTag bytes here?
                    val <- case S.runGet (S.getShortByteString $ fromIntegral aTag) aVal of
                        Left _ -> Nothing
                        Right v -> Just v
                    return (AttributeTag $ fromIntegral aTag, AttributeValue val)
                else Nothing

instance FromProto Proto.IdentityProviderIdentity where
    type Output' Proto.IdentityProviderIdentity = IdentityProviderIdentity
    fromProto = return . IP_ID . deMkWord32

instance FromProto Proto.CredentialPublicKeys where
    type Output' Proto.CredentialPublicKeys = CredentialPublicKeys
    fromProto cpk = do
        credKeys <-
            fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                =<< cpk ^? ProtoFields.keys
        credThreshold <-
            fmap SignatureThreshold
                . deMkWord8
                =<< cpk ^? ProtoFields.threshold
        -- This can not be 0.
        if credThreshold == 0
            then Nothing
            else return CredentialPublicKeys{..}
      where
        convert (ki, pKey) = do
            key <- pKey ^? ProtoFields.ed25519Key
            k <- case S.decode key of
                Left _ -> Nothing
                Right k -> return k
            -- Ensure that the index fits in a Word8.
            if fromIntegral ki > (maxBound :: Word8)
                then Nothing
                else return (fromIntegral ki, VerifyKeyEd25519 k)

instance FromProto Proto.AccountInfo where
    type Output' Proto.AccountInfo = AccountInfo
    fromProto ai = do
        aiAccountNonce <- fromProto =<< ai ^? ProtoFields.sequenceNumber
        aiAccountAmount <- fromProto =<< ai ^? ProtoFields.amount
        aiAccountReleaseSchedule <- fromProto =<< ai ^? ProtoFields.schedule
        aiAccountCredentials <-
            do
                fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                =<< ai ^? ProtoFields.creds
        aiAccountThreshold <- fromProto =<< ai ^? ProtoFields.threshold
        aiAccountEncryptedAmount <- fromProto =<< ai ^? ProtoFields.encryptedBalance
        aiAccountEncryptionKey <- fromProto =<< ai ^? ProtoFields.encryptionKey
        aiAccountIndex <- fromProto =<< ai ^? ProtoFields.index
        aiStakingInfo <-
            case ai ^. ProtoFields.maybe'stake of
                Nothing -> return AccountStakingNone
                Just asi -> fromProto asi
        aiAccountAddress <- fromProto =<< ai ^? ProtoFields.address
        return AccountInfo{..}
      where
        versionTag = 0
        convert (key, val) = do
            v <- fromProto val
            -- Ensure that the index fits in a Word8.
            if fromIntegral key > (maxBound :: Word8)
                then Nothing
                else return (CredentialIndex $ fromIntegral key, Versioned versionTag v)

instance ToProto BlockHashInput where
    type Output BlockHashInput = Proto.BlockHashInput
    toProto = \case
        Best -> Proto.make $ ProtoFields.best .= defMessage
        LastFinal -> Proto.make $ ProtoFields.lastFinal .= defMessage
        Given bh -> Proto.make $ ProtoFields.given .= toProto bh

instance FromProto Proto.BlockHash where
    type Output' Proto.BlockHash = BlockHash
    fromProto bh = BlockHash <$> deMkSerialize bh

instance FromProto Proto.ModuleRef where
    type Output' Proto.ModuleRef = ModuleRef
    fromProto mr = do
        moduleRef <- deMkSerialize mr
        return ModuleRef{..}

instance FromProto Proto.VersionedModuleSource where
    type Output' Proto.VersionedModuleSource = Wasm.WasmModule
    fromProto vmSource = do
        vms <- vmSource ^. Proto.maybe'module'
        case vms of
            Proto.VersionedModuleSource'V0 v0 ->
                Wasm.WasmModuleV0 . Wasm.WasmModuleV . Wasm.ModuleSource <$> (v0 ^? Proto.value)
            Proto.VersionedModuleSource'V1 v1 ->
                Wasm.WasmModuleV1 . Wasm.WasmModuleV . Wasm.ModuleSource <$> (v1 ^? Proto.value)

instance FromProto Proto.ContractStateV0 where
    type Output' Proto.ContractStateV0 = Wasm.ContractState
    fromProto cs = Wasm.ContractState <$> cs ^? ProtoFields.value

instance FromProto Proto.ReceiveName where
    type Output' Proto.ReceiveName = Wasm.ReceiveName

    -- FIXME: Validation needed here, and I might be wrong;
    -- but I believe the description in `types.proto` is
    -- ambiguous, and may not reflect the reality of things.
    -- Where do I find a good definition of this?
    fromProto name = Wasm.ReceiveName <$> name ^? ProtoFields.value

instance FromProto Proto.InitName where
    type Output' Proto.InitName = Wasm.InitName

    -- FIXME: Validation needed here, and I might be wrong;
    -- but I believe the description in `types.proto` is
    -- ambiguous, and may not reflect the reality of things.
    -- Where do I find a good definition of this?
    fromProto name = Wasm.InitName <$> name ^? ProtoFields.value

instance FromProto Proto.InstanceInfo where
    type Output' Proto.InstanceInfo = Wasm.InstanceInfo
    fromProto iInfo = do
        ii <- iInfo ^. Proto.maybe'version
        case ii of
            Proto.InstanceInfo'V0' v0 -> do
                iiModel <- fromProto =<< v0 ^? ProtoFields.model
                iiOwner <- fromProto =<< v0 ^? ProtoFields.owner
                iiAmount <- fromProto =<< v0 ^? ProtoFields.amount
                iiMethods <- do
                    methods <- v0 ^? ProtoFields.methods
                    Set.fromList <$> mapM fromProto methods
                iiName <- fromProto =<< v0 ^? ProtoFields.name
                iiSourceModule <- fromProto =<< v0 ^? ProtoFields.sourceModule
                return Wasm.InstanceInfoV0{..}
            Proto.InstanceInfo'V1' v1 -> do
                iiOwner <- fromProto =<< v1 ^? ProtoFields.owner
                iiAmount <- fromProto =<< v1 ^? ProtoFields.amount
                iiMethods <- do
                    methods <- v1 ^? ProtoFields.methods
                    Set.fromList <$> mapM fromProto methods
                iiName <- fromProto =<< v1 ^? ProtoFields.name
                iiSourceModule <- fromProto =<< v1 ^? ProtoFields.sourceModule
                return Wasm.InstanceInfoV1{..}

instance FromProto Proto.NextAccountSequenceNumber where
    type Output' Proto.NextAccountSequenceNumber = QueryTypes.NextAccountNonce
    fromProto asn = do
        nanNonce <- fromProto =<< asn ^? ProtoFields.sequenceNumber
        nanAllFinal <- asn ^? ProtoFields.allFinal
        return QueryTypes.NextAccountNonce{..}

instance FromProto Proto.ProtocolVersion where
    type Output' Proto.ProtocolVersion = ProtocolVersion
    fromProto = \case
        Proto.PROTOCOL_VERSION_1 -> return P1
        Proto.PROTOCOL_VERSION_2 -> return P2
        Proto.PROTOCOL_VERSION_3 -> return P3
        Proto.PROTOCOL_VERSION_4 -> return P4
        Proto.PROTOCOL_VERSION_5 -> return P5
        Proto.PROTOCOL_VERSION_6 -> return P6
        Proto.ProtocolVersion'Unrecognized _ -> Nothing

instance FromProto Proto.Duration where
    type Output' Proto.Duration = Duration
    fromProto = return . Duration . deMkWord64

instance FromProto Proto.BlockHeight where
    type Output' Proto.BlockHeight = BlockHeight
    fromProto = return . BlockHeight . deMkWord64

instance FromProto Proto.AbsoluteBlockHeight where
    type Output' Proto.AbsoluteBlockHeight = AbsoluteBlockHeight
    fromProto = return . AbsoluteBlockHeight . deMkWord64

instance FromProto Proto.GenesisIndex where
    type Output' Proto.GenesisIndex = GenesisIndex
    fromProto = return . GenesisIndex . deMkWord32

instance FromProto Proto.ConsensusInfo where
    type Output' Proto.ConsensusInfo = QueryTypes.ConsensusStatus
    fromProto ci = do
        csBestBlock <- fromProto =<< ci ^? ProtoFields.bestBlock
        csGenesisBlock <- fromProto =<< ci ^? ProtoFields.genesisBlock
        csGenesisTime <- fmap timestampToUTCTime . fromProto =<< ci ^? ProtoFields.genesisTime
        csSlotDuration <- fromProto =<< ci ^? ProtoFields.slotDuration
        csEpochDuration <- fromProto =<< ci ^? ProtoFields.epochDuration
        csLastFinalizedBlock <- fromProto =<< ci ^? ProtoFields.lastFinalizedBlock
        csBestBlockHeight <- fromProto =<< ci ^? ProtoFields.bestBlockHeight
        csLastFinalizedBlockHeight <- fromProto =<< ci ^? ProtoFields.lastFinalizedBlockHeight
        csBlocksReceivedCount <- fromIntegral <$> ci ^? ProtoFields.blocksReceivedCount
        csBlocksVerifiedCount <- fromIntegral <$> ci ^? ProtoFields.blocksVerifiedCount
        let csBlockLastReceivedTime = fmap timestampToUTCTime . fromProto =<< ci ^. ProtoFields.maybe'blockLastReceivedTime
        csBlockReceiveLatencyEMA <- ci ^? ProtoFields.blockReceiveLatencyEma
        csBlockReceiveLatencyEMSD <- ci ^? ProtoFields.blockReceiveLatencyEmsd
        let csBlockReceivePeriodEMA = ci ^. ProtoFields.maybe'blockReceivePeriodEma
        let csBlockReceivePeriodEMSD = ci ^. ProtoFields.maybe'blockReceivePeriodEmsd
        let csBlockLastArrivedTime = fmap timestampToUTCTime . fromProto =<< ci ^. ProtoFields.maybe'blockLastArrivedTime
        csBlockArriveLatencyEMA <- ci ^? ProtoFields.blockArriveLatencyEma
        csBlockArriveLatencyEMSD <- ci ^? ProtoFields.blockArriveLatencyEmsd
        let csBlockArrivePeriodEMA = ci ^. ProtoFields.maybe'blockArrivePeriodEma
        let csBlockArrivePeriodEMSD = ci ^. ProtoFields.maybe'blockArrivePeriodEmsd
        csTransactionsPerBlockEMA <- ci ^? ProtoFields.transactionsPerBlockEma
        csTransactionsPerBlockEMSD <- ci ^? ProtoFields.transactionsPerBlockEmsd
        csFinalizationCount <- fromIntegral <$> ci ^? ProtoFields.finalizationCount
        let csLastFinalizedTime = fmap timestampToUTCTime . fromProto =<< ci ^. ProtoFields.maybe'lastFinalizedTime
        let csFinalizationPeriodEMA = ci ^. ProtoFields.maybe'finalizationPeriodEma
        let csFinalizationPeriodEMSD = ci ^. ProtoFields.maybe'finalizationPeriodEmsd
        csProtocolVersion <- fromProto =<< ci ^? ProtoFields.protocolVersion
        csGenesisIndex <- fromProto =<< ci ^? ProtoFields.genesisIndex
        csCurrentEraGenesisBlock <- fromProto =<< ci ^? ProtoFields.currentEraGenesisBlock
        csCurrentEraGenesisTime <- fmap timestampToUTCTime . fromProto =<< ci ^? ProtoFields.currentEraGenesisTime
        return QueryTypes.ConsensusStatus{..}

instance FromProto Proto.Slot where
    type Output' Proto.Slot = Slot
    fromProto = return . Slot . deMkWord64

instance FromProto Proto.StateHash where
    type Output' Proto.StateHash = StateHash
    fromProto sh = StateHashV0 <$> deMkSerialize sh

instance FromProto Proto.Energy where
    type Output' Proto.Energy = Energy
    fromProto = return . Energy . deMkWord64

instance FromProto Proto.BlockInfo where
    type Output' Proto.BlockInfo = QueryTypes.BlockInfo
    fromProto bi = do
        biBlockHash <- fromProto =<< bi ^? ProtoFields.hash
        biBlockHeight <- fromProto =<< bi ^? ProtoFields.height
        biBlockParent <- fromProto =<< bi ^? ProtoFields.parentBlock
        biBlockLastFinalized <- fromProto =<< bi ^? ProtoFields.lastFinalizedBlock
        biGenesisIndex <- fromProto =<< bi ^? ProtoFields.genesisIndex
        biEraBlockHeight <- fromProto =<< bi ^? ProtoFields.eraBlockHeight
        biBlockReceiveTime <- fmap timestampToUTCTime . fromProto =<< bi ^? ProtoFields.receiveTime
        biBlockArriveTime <- fmap timestampToUTCTime . fromProto =<< bi ^? ProtoFields.arriveTime
        biBlockSlot <- fromProto =<< bi ^? ProtoFields.slotNumber
        biBlockSlotTime <- fmap timestampToUTCTime . fromProto =<< bi ^? ProtoFields.slotTime
        let biBlockBaker = fromProto =<< bi ^. ProtoFields.maybe'baker
        biFinalized <- bi ^? ProtoFields.finalized
        biTransactionCount <- fromIntegral <$> bi ^? ProtoFields.transactionCount
        biTransactionEnergyCost <- fromProto =<< bi ^? ProtoFields.transactionsEnergyCost
        biTransactionsSize <- fromIntegral <$> bi ^? ProtoFields.transactionsSize
        biBlockStateHash <- fromProto =<< bi ^? ProtoFields.stateHash
        return QueryTypes.BlockInfo{..}

instance FromProto Proto.Amount where
    type Output' Proto.Amount = Amount
    fromProto = return . Amount . deMkWord64

instance FromProto Proto.PoolCurrentPaydayInfo where
    type Output' Proto.PoolCurrentPaydayInfo = QueryTypes.CurrentPaydayBakerPoolStatus
    fromProto cpi = do
        bpsBlocksBaked <- fromIntegral <$> cpi ^? ProtoFields.blocksBaked
        bpsFinalizationLive <- cpi ^? ProtoFields.finalizationLive
        bpsTransactionFeesEarned <- fromProto =<< cpi ^? ProtoFields.transactionFeesEarned
        bpsEffectiveStake <- fromProto =<< cpi ^? ProtoFields.effectiveStake
        bpsLotteryPower <- cpi ^? ProtoFields.lotteryPower
        bpsBakerEquityCapital <- fromProto =<< cpi ^? ProtoFields.bakerEquityCapital
        bpsDelegatedCapital <- fromProto =<< cpi ^? ProtoFields.delegatedCapital
        return QueryTypes.CurrentPaydayBakerPoolStatus{..}

instance FromProto Proto.PoolInfoResponse where
    type Output' Proto.PoolInfoResponse = QueryTypes.PoolStatus
    fromProto pir = do
        psBakerId <- fromProto =<< pir ^? ProtoFields.baker
        psBakerAddress <- fromProto =<< pir ^? ProtoFields.address
        psBakerEquityCapital <- fromProto =<< pir ^? ProtoFields.equityCapital
        psDelegatedCapital <- fromProto =<< pir ^? ProtoFields.delegatedCapital
        psDelegatedCapitalCap <- fromProto =<< pir ^? ProtoFields.delegatedCapitalCap
        psPoolInfo <- fromProto =<< pir ^? ProtoFields.poolInfo
        psBakerStakePendingChange <-
            case pir ^. ProtoFields.maybe'equityPendingChange of
                Nothing -> return QueryTypes.PPCNoChange
                Just ppc -> fromProto ppc
        let psCurrentPaydayStatus = fromProto =<< pir ^. ProtoFields.maybe'currentPaydayInfo
        psAllPoolTotalCapital <- fromProto =<< pir ^? ProtoFields.allPoolTotalCapital
        return QueryTypes.BakerPoolStatus{..}

instance FromProto Proto.PassiveDelegationInfo where
    type Output' Proto.PassiveDelegationInfo = QueryTypes.PoolStatus
    fromProto pdi = do
        psDelegatedCapital <- fromProto =<< pdi ^? ProtoFields.delegatedCapital
        psCommissionRates <- fromProto =<< pdi ^? ProtoFields.commissionRates
        psCurrentPaydayTransactionFeesEarned <- fromProto =<< pdi ^? ProtoFields.currentPaydayTransactionFeesEarned
        psCurrentPaydayDelegatedCapital <- fromProto =<< pdi ^? ProtoFields.currentPaydayDelegatedCapital
        psAllPoolTotalCapital <- fromProto =<< pdi ^? ProtoFields.allPoolTotalCapital
        return QueryTypes.PassiveDelegationStatus{..}

instance FromProto Proto.PoolPendingChange where
    type Output' Proto.PoolPendingChange = QueryTypes.PoolPendingChange
    fromProto ppChange = do
        ppc <- ppChange ^. Proto.maybe'change
        case ppc of
            Proto.PoolPendingChange'Reduce' reduce -> do
                ppcBakerEquityCapital <- fromProto =<< reduce ^? ProtoFields.reducedEquityCapital
                ppcEffectiveTime <- fmap timestampToUTCTime . fromProto =<< reduce ^? ProtoFields.effectiveTime
                return QueryTypes.PPCReduceBakerCapital{..}
            Proto.PoolPendingChange'Remove' remove -> do
                ppcEffectiveTime <- fmap timestampToUTCTime . fromProto =<< remove ^? ProtoFields.effectiveTime
                return QueryTypes.PPCRemovePool{..}

instance FromProto Proto.BlocksAtHeightResponse where
    type Output' Proto.BlocksAtHeightResponse = [BlockHash]
    fromProto bahr = mapM fromProto =<< bahr ^? ProtoFields.blocks

-- |Input for `getBlocksAtHeightV2`.
data BlockHeightInput
    = -- |The height of a block relative to a genesis index. This differs from the
      -- absolute block height in that it counts height from the protocol update
      -- corresponding to the provided genesis index.
      Relative
        { -- |Genesis index.
          rGenesisIndex :: !GenesisIndex,
          -- |Block height starting from the genesis block at the genesis index.
          rBlockHeight :: !BlockHeight,
          -- |Whether to return results only from the specified genesis index (`True`),
          -- or allow results from more recent genesis indices as well (`False`).
          rRestrict :: !Bool
        }
    | -- |The absolute height of a block. This is the number of ancestors of a block
      -- since the genesis block. In particular, the chain genesis block has absolute
      -- height 0.
      Absolute
        { aBlockHeight :: !AbsoluteBlockHeight
        }

instance ToProto BlockHeightInput where
    type Output BlockHeightInput = Proto.BlocksAtHeightRequest
    toProto Relative{..} =
        Proto.make $
            ProtoFields.relative
                .= Proto.make
                    ( do
                        ProtoFields.genesisIndex .= toProto rGenesisIndex
                        ProtoFields.height .= toProto rBlockHeight
                        ProtoFields.restrict .= rRestrict
                    )
    toProto Absolute{..} =
        Proto.make $
            ProtoFields.absolute .= Proto.make (ProtoFields.height .= toProto aBlockHeight)

instance FromProto Proto.MintRate where
    type Output' Proto.MintRate = MintRate
    fromProto mr = do
        mrMantissa <- mr ^? ProtoFields.mantissa
        mrExponent <- fromIntegral <$> mr ^? ProtoFields.exponent
        -- Ensure that the exponent fits in a Word8.
        if mrExponent > (maxBound :: Word8)
            then Nothing
            else return MintRate{..}

instance FromProto Proto.TokenomicsInfo where
    type Output' Proto.TokenomicsInfo = QueryTypes.RewardStatus
    fromProto tInfo = do
        ti <- tInfo ^. Proto.maybe'tokenomics
        case ti of
            Proto.TokenomicsInfo'V0' v0 -> do
                rsTotalAmount <- fromProto =<< v0 ^? ProtoFields.totalAmount
                rsTotalEncryptedAmount <- fromProto =<< v0 ^? ProtoFields.totalEncryptedAmount
                rsBakingRewardAccount <- fromProto =<< v0 ^? ProtoFields.bakingRewardAccount
                rsFinalizationRewardAccount <- fromProto =<< v0 ^? ProtoFields.finalizationRewardAccount
                rsGasAccount <- fromProto =<< v0 ^? ProtoFields.gasAccount
                rsProtocolVersion <- fromProto =<< v0 ^? ProtoFields.protocolVersion
                return QueryTypes.RewardStatusV0{..}
            Proto.TokenomicsInfo'V1' v1 -> do
                rsTotalAmount <- fromProto =<< v1 ^? ProtoFields.totalAmount
                rsTotalEncryptedAmount <- fromProto =<< v1 ^? ProtoFields.totalEncryptedAmount
                rsBakingRewardAccount <- fromProto =<< v1 ^? ProtoFields.bakingRewardAccount
                rsFinalizationRewardAccount <- fromProto =<< v1 ^? ProtoFields.finalizationRewardAccount
                rsGasAccount <- fromProto =<< v1 ^? ProtoFields.gasAccount
                rsFoundationTransactionRewards <- fromProto =<< v1 ^? ProtoFields.foundationTransactionRewards
                rsNextPaydayTime <- fmap timestampToUTCTime . fromProto =<< v1 ^? ProtoFields.nextPaydayTime
                rsNextPaydayMintRate <- fromProto =<< v1 ^? ProtoFields.nextPaydayMintRate
                rsTotalStakedCapital <- fromProto =<< v1 ^? ProtoFields.totalStakedCapital
                rsProtocolVersion <- fromProto =<< v1 ^? ProtoFields.protocolVersion
                return QueryTypes.RewardStatusV1{..}

-- |Input for `invokeInstanceV2`.
type InvokeInstanceInput = (BlockHashInput, ContractContext)

instance ToProto InvokeInstanceInput where
    type Output InvokeInstanceInput = Proto.InvokeInstanceRequest
    toProto (bhi, ContractContext{..}) =
        Proto.make $ do
            ProtoFields.blockHash .= toProto bhi
            ProtoFields.maybe'invoker .= fmap toProto ccInvoker
            ProtoFields.instance' .= toProto ccContract
            ProtoFields.amount .= toProto ccAmount
            ProtoFields.entrypoint .= toProto ccMethod
            ProtoFields.parameter .= toProto ccParameter
            ProtoFields.energy .= toProto ccEnergy

instance FromProto Proto.ContractEvent where
    type Output' Proto.ContractEvent = Wasm.ContractEvent
    -- FIXME: Can we just use deMkSerialize here? Types check out, but fromShort
    -- is used in the other direction...
    fromProto ce = return . Wasm.ContractEvent . BSS.toShort $ ce ^. ProtoFields.value

instance FromProto Proto.Parameter where
    type Output' Proto.Parameter = Wasm.Parameter
    -- FIXME: There are protocol-dependent limits on the length. Can something
    -- be done to enforce this here?
    -- FIXME: Can we just use deMkSerialize here? Types check out, but fromShort
    -- is used in the other direction...
    fromProto p = return . Wasm.Parameter . BSS.toShort $ p ^. ProtoFields.value

instance FromProto Proto.Address where
    type Output' Proto.Address = Address
    fromProto a = do
        addr <- a ^. Proto.maybe'type'
        case addr of
            Proto.Address'Account aAddr -> AddressAccount <$> fromProto aAddr
            Proto.Address'Contract cAddr -> AddressContract <$> fromProto cAddr

instance FromProto Proto.ContractAddress where
    type Output' Proto.ContractAddress = ContractAddress
    fromProto ca = do
        contractIndex <- ContractIndex <$> ca ^? ProtoFields.index
        contractSubindex <- ContractSubindex <$> ca ^? ProtoFields.subindex
        return ContractAddress{..}

instance FromProto Proto.ContractVersion where
    type Output' Proto.ContractVersion = Wasm.WasmVersion
    fromProto cv = case cv of
        Proto.V0 -> return Wasm.V0
        Proto.V1 -> return Wasm.V1
        Proto.ContractVersion'Unrecognized _ -> Nothing

instance FromProto Proto.ContractTraceElement where
    type Output' Proto.ContractTraceElement = Event
    fromProto ctElement = do
        cte <- ctElement ^. Proto.maybe'element
        case cte of
            Proto.ContractTraceElement'Updated updated -> do
                euContractVersion <- fromProto =<< updated ^? ProtoFields.contractVersion
                euAddress <- fromProto =<< updated ^? ProtoFields.address
                euInstigator <- fromProto =<< updated ^? ProtoFields.instigator
                euAmount <- fromProto =<< updated ^? ProtoFields.amount
                euMessage <- fromProto =<< updated ^? ProtoFields.parameter
                euReceiveName <- fromProto =<< updated ^? ProtoFields.receiveName
                euEvents <- mapM fromProto =<< updated ^? ProtoFields.events
                return Updated{..}
            Proto.ContractTraceElement'Transferred' transferred -> do
                etFrom <- fmap AddressContract $ fromProto =<< transferred ^? ProtoFields.sender
                etAmount <- fromProto =<< transferred ^? ProtoFields.amount
                etTo <- fmap AddressAccount $ fromProto =<< transferred ^? ProtoFields.receiver
                return Transferred{..}
            Proto.ContractTraceElement'Interrupted' interrupted -> do
                iAddress <- fromProto =<< interrupted ^? ProtoFields.address
                iEvents <- mapM fromProto =<< interrupted ^? ProtoFields.events
                return Interrupted{..}
            Proto.ContractTraceElement'Resumed' resumed -> do
                rAddress <- fromProto =<< resumed ^? ProtoFields.address
                rSuccess <- resumed ^? ProtoFields.success
                return Resumed{..}
            Proto.ContractTraceElement'Upgraded' upgraded -> do
                euAddress <- fromProto =<< upgraded ^? ProtoFields.address
                euFrom <- fromProto =<< upgraded ^? ProtoFields.from
                euTo <- fromProto =<< upgraded ^? ProtoFields.to
                return Upgraded{..}

instance FromProto Proto.RejectReason where
    type Output' Proto.RejectReason = RejectReason
    fromProto rReason = do
        r <- rReason ^. Proto.maybe'reason
        case r of
            Proto.RejectReason'ModuleNotWf _ ->
                return ModuleNotWF
            Proto.RejectReason'ModuleHashAlreadyExists mr ->
                ModuleHashAlreadyExists <$> fromProto mr
            Proto.RejectReason'InvalidAccountReference addr ->
                InvalidAccountReference <$> fromProto addr
            Proto.RejectReason'InvalidInitMethod' rr -> do
                moduleRef <- fromProto =<< rr ^? ProtoFields.moduleRef
                initName <- fromProto =<< rr ^? ProtoFields.initName
                return $ InvalidInitMethod moduleRef initName
            Proto.RejectReason'InvalidReceiveMethod' rr -> do
                moduleRef <- fromProto =<< rr ^? ProtoFields.moduleRef
                receiveName <- fromProto =<< rr ^? ProtoFields.receiveName
                return $ InvalidReceiveMethod moduleRef receiveName
            Proto.RejectReason'InvalidModuleReference mr ->
                InvalidModuleReference <$> fromProto mr
            Proto.RejectReason'InvalidContractAddress cAddr ->
                InvalidContractAddress <$> fromProto cAddr
            Proto.RejectReason'RuntimeFailure _ ->
                return RuntimeFailure
            Proto.RejectReason'AmountTooLarge' rr -> do
                addr <- fromProto =<< rr ^? ProtoFields.address
                amt <- fromProto =<< rr ^? ProtoFields.amount
                return $ AmountTooLarge addr amt
            Proto.RejectReason'SerializationFailure _ ->
                return SerializationFailure
            Proto.RejectReason'OutOfEnergy _ ->
                return OutOfEnergy
            Proto.RejectReason'RejectedInit' rr ->
                RejectedInit <$> rr ^? ProtoFields.rejectReason
            Proto.RejectReason'RejectedReceive' rr -> do
                rejectReason <- rr ^? ProtoFields.rejectReason
                contractAddress <- fromProto =<< rr ^? ProtoFields.contractAddress
                receiveName <- fromProto =<< rr ^? ProtoFields.receiveName
                parameter <- fromProto =<< rr ^? ProtoFields.parameter
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
            Proto.RejectReason'DuplicateAggregationKey key ->
                DuplicateAggregationKey <$> deMkSerialize key
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
                raw <- mapM (fmap snd . fromProto) =<< rr ^? ProtoFields.ids
                credIds <- mapM credIdFromRaw raw
                return $ DuplicateCredIDs credIds
            Proto.RejectReason'NonExistentCredIds' necIds -> do
                ids <- mapM (fmap fst . fromProto) =<< necIds ^? ProtoFields.ids
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
                return BakingRewardCommissionNotInRange
            Proto.RejectReason'BakingRewardCommissionNotInRange _ -> do
                return TransactionFeeCommissionNotInRange
            Proto.RejectReason'TransactionFeeCommissionNotInRange _ -> do
                return FinalizationRewardCommissionNotInRange
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

instance FromProto Proto.InvokeInstanceResponse where
    type Output' Proto.InvokeInstanceResponse = InvokeContract.InvokeContractResult
    fromProto icResult = do
        icr <- icResult ^. Proto.maybe'result
        case icr of
            Proto.InvokeInstanceResponse'Success' success -> do
                let rcrReturnValue = success ^. ProtoFields.maybe'returnValue
                rcrUsedEnergy <- fromProto =<< success ^? ProtoFields.usedEnergy
                rcrEvents <- mapM fromProto =<< success ^? ProtoFields.effects
                return InvokeContract.Success{..}
            Proto.InvokeInstanceResponse'Failure' failure -> do
                let rcrReturnValue = failure ^. ProtoFields.maybe'returnValue
                rcrUsedEnergy <- fromProto =<< failure ^? ProtoFields.usedEnergy
                rcrReason <- fromProto =<< failure ^? ProtoFields.reason
                return InvokeContract.Failure{..}

instance FromProto Proto.Branch where
    type Output' Proto.Branch = QueryTypes.Branch
    fromProto branch = do
        branchBlockHash <- fromProto =<< branch ^? ProtoFields.blockHash
        branchChildren <- mapM fromProto =<< branch ^? ProtoFields.children
        return QueryTypes.Branch{..}

instance FromProto Proto.ElectionInfo'Baker where
    type Output' Proto.ElectionInfo'Baker = QueryTypes.BakerSummary
    fromProto baker = do
        bsBakerId <- fromProto =<< baker ^? ProtoFields.baker
        bsBakerAccount <- fromProto <$> baker ^? ProtoFields.account
        bsBakerLotteryPower <- baker ^? ProtoFields.lotteryPower
        return QueryTypes.BakerSummary{..}

instance FromProto Proto.ElectionDifficulty where
    type Output' Proto.ElectionDifficulty = ElectionDifficulty
    fromProto eDiff = do
        af <- eDiff ^? (ProtoFields.value . ProtoFields.partsPerHundredThousand)
        -- This must be strictly less than 100_000.
        if af >= 100_000
            then Nothing
            else return $ makeElectionDifficultyUnchecked af

instance FromProto Proto.LeadershipElectionNonce where
    type Output' Proto.LeadershipElectionNonce = LeadershipElectionNonce
    fromProto = deMkSerialize

instance FromProto Proto.ElectionInfo where
    type Output' Proto.ElectionInfo = QueryTypes.BlockBirkParameters
    fromProto eInfo = do
        bbpBakers <-
            mapM fromProto
                . Vec.fromList
                =<< eInfo ^? ProtoFields.bakerElectionInfo
        bbpElectionDifficulty <- fromProto =<< eInfo ^? ProtoFields.electionDifficulty
        bbpElectionNonce <- fromProto =<< eInfo ^? ProtoFields.electionNonce
        return QueryTypes.BlockBirkParameters{..}

instance FromProto Proto.NextUpdateSequenceNumbers where
    type Output' Proto.NextUpdateSequenceNumbers = QueryTypes.NextUpdateSequenceNumbers
    fromProto nums = do
        _nusnRootKeys <- fromProto =<< nums ^? ProtoFields.rootKeys
        _nusnLevel1Keys <- fromProto =<< nums ^? ProtoFields.level1Keys
        _nusnLevel2Keys <- fromProto =<< nums ^? ProtoFields.level2Keys
        _nusnProtocol <- fromProto =<< nums ^? ProtoFields.protocol
        _nusnElectionDifficulty <- fromProto =<< nums ^? ProtoFields.electionDifficulty
        _nusnEuroPerEnergy <- fromProto =<< nums ^? ProtoFields.euroPerEnergy
        _nusnMicroCCDPerEuro <- fromProto =<< nums ^? ProtoFields.microCcdPerEuro
        _nusnFoundationAccount <- fromProto =<< nums ^? ProtoFields.foundationAccount
        _nusnMintDistribution <- fromProto =<< nums ^? ProtoFields.mintDistribution
        _nusnTransactionFeeDistribution <- fromProto =<< nums ^? ProtoFields.transactionFeeDistribution
        _nusnGASRewards <- fromProto =<< nums ^? ProtoFields.gasRewards
        _nusnPoolParameters <- fromProto =<< nums ^? ProtoFields.poolParameters
        _nusnAddAnonymityRevoker <- fromProto =<< nums ^? ProtoFields.addAnonymityRevoker
        _nusnAddIdentityProvider <- fromProto =<< nums ^? ProtoFields.addIdentityProvider
        _nusnCooldownParameters <- fromProto =<< nums ^? ProtoFields.cooldownParameters
        _nusnTimeParameters <- fromProto =<< nums ^? ProtoFields.timeParameters
        return QueryTypes.NextUpdateSequenceNumbers{..}

-- An IP address.
type IpAddress = Text

-- An IP port.
type IpPort = Int16

-- A peer are represented by its IP address.
type Peer = IpAddress

-- Compound type representing a pair of an IP address and a port.
type IpSocketAddress = (IpAddress, IpPort)

instance ToProto IpAddress where
    type Output IpAddress = Proto.IpAddress
    toProto ip = Proto.make $ ProtoFields.value .= ip

instance ToProto IpPort where
    type Output IpPort = Proto.Port
    toProto ip = Proto.make $ ProtoFields.value .= fromIntegral ip

instance FromProto Proto.IpAddress where
    type Output' Proto.IpAddress = IpAddress

    -- FIXME: Should this be validated?
    fromProto peer = peer ^? ProtoFields.value

instance FromProto Proto.Port where
    type Output' Proto.Port = IpPort

    -- FIXME: Should this be validated?
    fromProto port = fromIntegral <$> port ^? ProtoFields.value

instance FromProto Proto.BannedPeer where
    type Output' Proto.BannedPeer = Peer
    fromProto peer = fromProto =<< peer ^? ProtoFields.ipAddress

instance FromProto Proto.BannedPeers where
    type Output' Proto.BannedPeers = [Peer]
    fromProto peers = mapM fromProto =<< peers ^? ProtoFields.peers

instance FromProto Proto.InstanceStateValueAtKey where
    type Output' Proto.InstanceStateValueAtKey = ByteString
    fromProto = deMkSerialize

instance FromProto Proto.Signature where
    type Output' Proto.Signature = Signature
    fromProto = deMkSerialize

instance FromProto Proto.SignatureMap where
    type Output' Proto.SignatureMap = Updates.UpdateInstructionSignatures
    fromProto uis = do
        signatures <-
            fmap Map.fromAscList
                . mapM deMk
                . Map.toAscList
                =<< uis ^? ProtoFields.signatures
        return Updates.UpdateInstructionSignatures{..}
      where
        deMk (k, s) = do
            -- Ensure that the value fits into a Word16.
            if fromIntegral k <= (maxBound :: Word16)
                then do
                    sig <- fromProto s
                    return (fromIntegral k, sig)
                else Nothing

instance FromProto Proto.TransactionTime where
    type Output' Proto.TransactionTime = TransactionTime
    fromProto = return . TransactionTime . deMkWord64

instance FromProto Proto.UpdateSequenceNumber where
    type Output' Proto.UpdateSequenceNumber = Nonce
    fromProto = return . Nonce . deMkWord64

instance FromProto Proto.CredentialType where
    type Output' Proto.CredentialType = CredentialType
    fromProto Proto.CREDENTIAL_TYPE_INITIAL = return Initial
    fromProto Proto.CREDENTIAL_TYPE_NORMAL = return Normal
    fromProto (Proto.CredentialType'Unrecognized _) = Nothing

instance FromProto Proto.UpdateType where
    type Output' Proto.UpdateType = Updates.UpdateType
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
    fromProto (Proto.UpdateType'Unrecognized _) = Nothing

instance FromProto Proto.TransactionType where
    type Output' Proto.TransactionType = TransactionType
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
    fromProto (ProtoFields.TransactionType'Unrecognized _) = Nothing

instance FromProto Proto.AccountCreationDetails where
    type Output' Proto.AccountCreationDetails = ValidResult
    fromProto acDetails = do
        ecdAccount <- fromProto =<< acDetails ^? ProtoFields.address
        ecdRegId <- fmap fst . fromProto =<< acDetails ^? ProtoFields.regId
        let vrEvents = [AccountCreated ecdAccount, CredentialDeployed{..}]
        return TxSuccess{..}

instance FromProto Proto.BakerEvent'BakerAdded where
    type Output' Proto.BakerEvent'BakerAdded = BakerAddedEvent
    fromProto baEvent = do
        keysEvent <- fromProto =<< baEvent ^? ProtoFields.keysEvent
        stake <- fromProto =<< baEvent ^? ProtoFields.stake
        restakeEarnings <- baEvent ^? ProtoFields.restakeEarnings
        return (keysEvent, stake, restakeEarnings)

instance FromProto Proto.BakerKeysEvent where
    type Output' Proto.BakerKeysEvent = BakerKeysEvent
    fromProto bkEvent = do
        bakerId <- fromProto =<< bkEvent ^? ProtoFields.bakerId
        addr <- fromProto =<< bkEvent ^? ProtoFields.account
        signKey <- fromProto =<< bkEvent ^? ProtoFields.signKey
        electionKey <- fromProto =<< bkEvent ^? ProtoFields.electionKey
        aggregationKey <- fromProto =<< bkEvent ^? ProtoFields.aggregationKey
        return (bakerId, addr, signKey, electionKey, aggregationKey)

instance FromProto Proto.RegisteredData where
    type Output' Proto.RegisteredData = RegisteredData
    -- FIXME: Can we just use deMkSerialize here? Types check out, but fromShort
    -- is used in the other direction...
    fromProto rData = RegisteredData . BSS.toShort <$> rData ^? ProtoFields.value

instance FromProto Proto.NewRelease where
    type Output' Proto.NewRelease = (Timestamp, Amount)
    fromProto nRelease = do
        tStamp <- fromProto =<< nRelease ^? ProtoFields.timestamp
        amount <- fromProto =<< nRelease ^? ProtoFields.amount
        return (tStamp, amount)

instance FromProto Proto.Memo where
    type Output' Proto.Memo = Memo
    -- FIXME: Can we just use deMkSerialize here? Types check out, but fromShort
    -- is used in the other direction...
    fromProto memo = Memo . BSS.toShort <$> memo ^? ProtoFields.value

instance FromProto Proto.ArInfo'ArIdentity where
    type Output' Proto.ArInfo'ArIdentity = ArIdentity
    fromProto arIdentity = do
        let arId = deMkWord32 arIdentity
        -- This can not be 0.
        -- FIXME: This is not checked in the FFI `ar_info_create` function of base.
        if arId == 0
            then Nothing
            else return $ ArIdentity arId

instance FromProto Proto.IpIdentity where
    type Output' Proto.IpIdentity = IdentityProviderIdentity
    fromProto arIdentity = do
        let ipIp = deMkWord32 arIdentity
        -- FIXME: Can this not be 0 either?
        -- FIXME: This is not checked in the FFI `ip_info_create` function of base.
        if ipIp == 0
            then Nothing
            else return $ IP_ID ipIp

instance FromProto Proto.ArInfo where
    type Output' Proto.ArInfo = ArInfo.ArInfo
    fromProto arInfo = do
        arIdentity <- fromProto =<< arInfo ^? ProtoFields.identity
        arPubKey <- arInfo ^? ProtoFields.publicKey . ProtoFields.value
        arD <- arInfo ^? ProtoFields.description
        arName <- arD ^? ProtoFields.name
        arUrl <- arD ^? ProtoFields.url
        arDescription <- arD ^? ProtoFields.description
        createArInfo arIdentity arPubKey arName arUrl arDescription

instance FromProto Proto.IpInfo where
    type Output' Proto.IpInfo = IpInfo.IpInfo
    fromProto ipInfo = do
        ipVerifyKey <- ipInfo ^? ProtoFields.verifyKey . ProtoFields.value
        ipCdiVerifyKey <- ipInfo ^? ProtoFields.cdiVerifyKey . ProtoFields.value
        ipD <- ipInfo ^? ProtoFields.description
        ipIdentity <- fromProto =<< ipInfo ^? ProtoFields.identity
        ipName <- ipD ^? ProtoFields.name
        ipUrl <- ipD ^? ProtoFields.url
        ipDescription <- ipD ^? ProtoFields.description
        createIpInfo ipIdentity ipVerifyKey ipCdiVerifyKey ipName ipUrl ipDescription

instance FromProto Proto.BakerStakeThreshold where
    type Output' Proto.BakerStakeThreshold = Parameters.PoolParameters 'ChainParametersV0
    fromProto pParams = fmap Parameters.PoolParametersV0 . fromProto =<< pParams ^? ProtoFields.bakerStakeThreshold

instance FromProto Proto.DurationSeconds where
    type Output' Proto.DurationSeconds = DurationSeconds
    fromProto = return . DurationSeconds . deMkWord64

instance FromProto Proto.CooldownParametersCpv1 where
    type Output' Proto.CooldownParametersCpv1 = Parameters.CooldownParameters 'ChainParametersV1
    fromProto cdParams = do
        _cpPoolOwnerCooldown <- fromProto =<< cdParams ^? ProtoFields.poolOwnerCooldown
        _cpDelegatorCooldown <- fromProto =<< cdParams ^? ProtoFields.delegatorCooldown
        return Parameters.CooldownParametersV1{..}

instance FromProto Proto.TimeParametersCpv1 where
    type Output' Proto.TimeParametersCpv1 = (Parameters.TimeParameters 'ChainParametersV1)
    fromProto tParams = do
        _tpRewardPeriodLength <- fromProto =<< tParams ^? ProtoFields.rewardPeriodLength
        _tpMintPerPayday <- fromProto =<< tParams ^? ProtoFields.mintPerPayday
        return Parameters.TimeParametersV1{..}

instance FromProto Proto.RewardPeriodLength where
    type Output' Proto.RewardPeriodLength = RewardPeriodLength
    fromProto rpl = do
        t <- rpl ^? ProtoFields.value
        return . RewardPeriodLength $ deMkWord64 t

instance FromProto Proto.Ratio where
    type Output' Proto.Ratio = (Ratio.Ratio Word64)
    fromProto ratio = do
        numerator <- ratio ^? ProtoFields.numerator
        denominator <- ratio ^? ProtoFields.denominator
        -- Ensure we do not divide by zero.
        if denominator == 0
            then Nothing
            else return $ numerator Ratio.% denominator

instance FromProto Proto.ExchangeRate where
    type Output' Proto.ExchangeRate = ExchangeRate
    fromProto er = fmap ExchangeRate . fromProto =<< er ^? ProtoFields.value

instance FromProto Proto.GasRewards where
    type Output' Proto.GasRewards = Parameters.GASRewards
    fromProto gRewards = do
        _gasBaker <- fromProto =<< gRewards ^? ProtoFields.baker
        _gasFinalizationProof <- fromProto =<< gRewards ^? ProtoFields.finalizationProof
        _gasAccountCreation <- fromProto =<< gRewards ^? ProtoFields.accountCreation
        _gasChainUpdate <- fromProto =<< gRewards ^? ProtoFields.chainUpdate
        return Parameters.GASRewards{..}

instance FromProto Proto.AccessStructure where
    type Output' Proto.AccessStructure = Updates.AccessStructure
    fromProto aStructure = do
        accessPublicKeys <-
            Set.fromList
                <$> (mapM fromProtoUpdateKeysIndex =<< aStructure ^? ProtoFields.accessPublicKeys)
        accessThreshold <- fromProto =<< aStructure ^? ProtoFields.accessThreshold
        return Updates.AccessStructure{..}
      where
        fromProtoUpdateKeysIndex i = do
            i' <- i ^? ProtoFields.value
            if i' > (maxBound :: Word16)
                then Nothing
                else fromIntegral i

instance FromProto Proto.AuthorizationsV0 where
    type Output' Proto.AuthorizationsV0 = Updates.Authorizations 'ChainParametersV0
    fromProto auth = do
        asKeys <- Vec.fromList <$> (mapM fromProto =<< auth ^? ProtoFields.keys)
        asEmergency <- fromProto =<< auth ^? ProtoFields.emergency
        asProtocol <- fromProto =<< auth ^? ProtoFields.protocol
        asParamElectionDifficulty <- fromProto =<< auth ^? ProtoFields.parameterElectionDifficulty
        asParamEuroPerEnergy <- fromProto =<< auth ^? ProtoFields.parameterEuroPerEnergy
        asParamMicroGTUPerEuro <- fromProto =<< auth ^? ProtoFields.parameterMicroCCDPerEuro
        asParamFoundationAccount <- fromProto =<< auth ^? ProtoFields.parameterFoundationAccount
        asParamMintDistribution <- fromProto =<< auth ^? ProtoFields.parameterMintDistribution
        asParamTransactionFeeDistribution <- fromProto =<< auth ^? ProtoFields.parameterTransactionFeeDistribution
        asParamGASRewards <- fromProto =<< auth ^? ProtoFields.parameterGasRewards
        asPoolParameters <- fromProto =<< auth ^? ProtoFields.poolParameters
        asAddAnonymityRevoker <- fromProto =<< auth ^? ProtoFields.addAnonymityRevoker
        asAddIdentityProvider <- fromProto =<< auth ^? ProtoFields.addIdentityProvider
        let asCooldownParameters = NothingForCPV1
        let asTimeParameters = NothingForCPV1
        return Updates.Authorizations{..}

instance FromProto Proto.AuthorizationsV1 where
    type Output' Proto.AuthorizationsV1 = Updates.Authorizations 'ChainParametersV1
    fromProto auth = do
        v0 <- auth ^? ProtoFields.v0
        asKeys <- Vec.fromList <$> (mapM fromProto =<< v0 ^? ProtoFields.keys)
        asEmergency <- fromProto =<< v0 ^? ProtoFields.emergency
        asProtocol <- fromProto =<< v0 ^? ProtoFields.protocol
        asParamElectionDifficulty <- fromProto =<< v0 ^? ProtoFields.parameterElectionDifficulty
        asParamEuroPerEnergy <- fromProto =<< v0 ^? ProtoFields.parameterEuroPerEnergy
        asParamMicroGTUPerEuro <- fromProto =<< v0 ^? ProtoFields.parameterMicroCCDPerEuro
        asParamFoundationAccount <- fromProto =<< v0 ^? ProtoFields.parameterFoundationAccount
        asParamMintDistribution <- fromProto =<< v0 ^? ProtoFields.parameterMintDistribution
        asParamTransactionFeeDistribution <- fromProto =<< v0 ^? ProtoFields.parameterTransactionFeeDistribution
        asParamGASRewards <- fromProto =<< v0 ^? ProtoFields.parameterGasRewards
        asPoolParameters <- fromProto =<< v0 ^? ProtoFields.poolParameters
        asAddAnonymityRevoker <- fromProto =<< v0 ^? ProtoFields.addAnonymityRevoker
        asAddIdentityProvider <- fromProto =<< v0 ^? ProtoFields.addIdentityProvider
        asCooldownParameters <- justForCPV1A . fromProto =<< auth ^? ProtoFields.parameterCooldown
        asTimeParameters <- justForCPV1A . fromProto =<< auth ^? ProtoFields.parameterTime
        return Updates.Authorizations{..}

instance FromProto Proto.Level1Update where
    type Output' Proto.Level1Update = Updates.Level1Update
    fromProto l1Update = do
        u <- l1Update ^. ProtoFields.maybe'updateType
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
    type Output' Proto.UpdatePublicKey = Updates.UpdatePublicKey
    fromProto key = do
        keyBytes <- key ^? ProtoFields.value
        let decoded = case S.decode keyBytes of
                Left _ -> Nothing
                Right val -> val
        VerifyKeyEd25519 <$> decoded

instance FromProto Proto.UpdateKeysThreshold where
    type Output' Proto.UpdateKeysThreshold = Updates.UpdateKeysThreshold
    fromProto ukTreshold = do
        -- Ensure that the value fits into a Word16.
        treshold <- deMkWord16 ukTreshold
        return $ Updates.UpdateKeysThreshold (fromIntegral treshold)

instance FromProto Proto.RootUpdate where
    type Output' Proto.RootUpdate = (Updates.UpdateType, Updates.RootUpdate)
    fromProto rUpdate = do
        ru <- rUpdate ^. ProtoFields.maybe'updateType
        case ru of
            ProtoFields.RootUpdate'RootKeysUpdate _ -> do
                update <-
                    fmap (Updates.RootKeysRootUpdate . snd)
                        . fromProto
                        =<< rUpdate ^? ProtoFields.rootKeysUpdate
                return (Updates.UpdateRootKeys, update)
            ProtoFields.RootUpdate'Level1KeysUpdate _ -> do
                update <-
                    fmap (Updates.Level1KeysRootUpdate . fst)
                        . fromProto
                        =<< rUpdate ^? ProtoFields.level1KeysUpdate
                return (Updates.UpdateLevel1Keys, update)
            ProtoFields.RootUpdate'Level2KeysUpdateV0 _ -> do
                update <-
                    fmap Updates.Level2KeysRootUpdate
                        . fromProto
                        =<< rUpdate ^? ProtoFields.level2KeysUpdateV0
                return (Updates.UpdateLevel2Keys, update)
            ProtoFields.RootUpdate'Level2KeysUpdateV1 _ -> do
                update <-
                    fmap Updates.Level2KeysRootUpdateV1
                        . fromProto
                        =<< rUpdate ^? ProtoFields.level2KeysUpdateV1
                return (Updates.UpdateLevel2Keys, update)

instance FromProto Proto.HigherLevelKeys where
    type
        Output' Proto.HigherLevelKeys =
            ( Updates.HigherLevelKeys Updates.Level1KeysKind,
              Updates.HigherLevelKeys Updates.RootKeysKind
            )
    fromProto keys = do
        hlkKeys <- Vec.fromList <$> (mapM fromProto =<< keys ^? ProtoFields.keys)
        hlkThreshold <- fromProto =<< keys ^? ProtoFields.threshold
        return (Updates.HigherLevelKeys{..}, Updates.HigherLevelKeys{..})

instance FromProto Proto.MintDistributionCpv0 where
    type Output' Proto.MintDistributionCpv0 = Parameters.MintDistribution 'ChainParametersV0
    fromProto mDistribution = do
        _mdBakingReward <- fromProto =<< mDistribution ^? ProtoFields.bakingReward
        _mdFinalizationReward <- fromProto =<< mDistribution ^? ProtoFields.finalizationReward
        _mdMintPerSlot <- fmap Parameters.MintPerSlotForCPV0Some . fromProto =<< mDistribution ^? ProtoFields.mintPerSlot
        return Parameters.MintDistribution{..}

instance FromProto Proto.MintDistributionCpv1 where
    type Output' Proto.MintDistributionCpv1 = Parameters.MintDistribution 'ChainParametersV1
    fromProto mDistribution = do
        _mdBakingReward <- fromProto =<< mDistribution ^? ProtoFields.bakingReward
        _mdFinalizationReward <- fromProto =<< mDistribution ^? ProtoFields.finalizationReward
        let _mdMintPerSlot = Parameters.MintPerSlotForCPV0None
        return Parameters.MintDistribution{..}

instance FromProto Proto.InclusiveRangeAmountFraction where
    type Output' Proto.InclusiveRangeAmountFraction = Parameters.InclusiveRange AmountFraction
    fromProto iRange = do
        irMin <- fromProto =<< iRange ^? ProtoFields.min
        irMax <- fromProto =<< iRange ^? ProtoFields.max
        return Parameters.InclusiveRange{..}

instance FromProto Proto.CapitalBound where
    type Output' Proto.CapitalBound = Parameters.CapitalBound
    fromProto cBound = fmap Parameters.CapitalBound . fromProto =<< cBound ^? ProtoFields.value

instance FromProto Proto.LeverageFactor where
    type Output' Proto.LeverageFactor = Parameters.LeverageFactor
    fromProto lFactor = fmap Parameters.LeverageFactor . fromProto =<< lFactor ^? ProtoFields.value

instance FromProto Proto.PoolParametersCpv1 where
    type Output' Proto.PoolParametersCpv1 = (Parameters.PoolParameters 'ChainParametersV1)
    fromProto pParams = do
        _ppPassiveCommissions <- do
            _finalizationCommission <- fromProto =<< pParams ^? ProtoFields.passiveFinalizationCommission
            _bakingCommission <- fromProto =<< pParams ^? ProtoFields.passiveBakingCommission
            _transactionCommission <- fromProto =<< pParams ^? ProtoFields.passiveTransactionCommission
            return CommissionRates{..}
        _ppCommissionBounds <- do
            cBounds <- pParams ^? ProtoFields.commissionBounds
            _finalizationCommissionRange <- fromProto =<< cBounds ^? ProtoFields.finalization
            _bakingCommissionRange <- fromProto =<< cBounds ^? ProtoFields.baking
            _transactionCommissionRange <- fromProto =<< cBounds ^? ProtoFields.transaction
            return Parameters.CommissionRanges{..}
        _ppMinimumEquityCapital <- fromProto =<< pParams ^? ProtoFields.minimumEquityCapital
        _ppCapitalBound <- fromProto =<< pParams ^? ProtoFields.capitalBound
        _ppLeverageBound <- fromProto =<< pParams ^? ProtoFields.leverageBound
        return Parameters.PoolParametersV1{..}

instance FromProto Proto.Sha256Hash where
    type Output' Proto.Sha256Hash = Hash
    fromProto = deMkSerialize

instance FromProto Proto.ProtocolUpdate where
    type Output' Proto.ProtocolUpdate = Updates.ProtocolUpdate
    fromProto pUpdate = do
        puMessage <- pUpdate ^? ProtoFields.message
        puSpecificationURL <- pUpdate ^? ProtoFields.specificationUrl
        puSpecificationHash <- fromProto =<< pUpdate ^? ProtoFields.specificationHash
        puSpecificationAuxiliaryData <- pUpdate ^? ProtoFields.specificationAuxiliaryData
        return Updates.ProtocolUpdate{..}

instance FromProto Proto.TransactionFeeDistribution where
    type Output' Proto.TransactionFeeDistribution = Parameters.TransactionFeeDistribution
    fromProto tfDistribution = do
        _tfdBaker <- fromProto =<< tfDistribution ^? ProtoFields.baker
        _tfdGASAccount <- fromProto =<< tfDistribution ^? ProtoFields.gasAccount
        return Parameters.TransactionFeeDistribution{..}

instance FromProto Proto.UpdatePayload where
    type Output' Proto.UpdatePayload = (Updates.UpdateType, Updates.UpdatePayload)
    fromProto uPayload = do
        pl <- uPayload ^. ProtoFields.maybe'payload
        case pl of
            ProtoFields.UpdatePayload'AddAnonymityRevokerUpdate aarUpdate -> do
                ai <- fromProto aarUpdate
                return (Updates.UpdateAddAnonymityRevoker, Updates.AddAnonymityRevokerUpdatePayload ai)
            ProtoFields.UpdatePayload'AddIdentityProviderUpdate aipUpdate -> do
                ip <- fromProto aipUpdate
                return (Updates.UpdateAddIdentityProvider, Updates.AddIdentityProviderUpdatePayload ip)
            ProtoFields.UpdatePayload'BakerStakeThresholdUpdate bstUpdate -> do
                pp <- fromProto bstUpdate
                return (Updates.UpdatePoolParameters, Updates.BakerStakeThresholdUpdatePayload pp)
            ProtoFields.UpdatePayload'CooldownParametersCpv1Update cdpc1Update -> do
                cp <- fromProto cdpc1Update
                return (Updates.UpdateCooldownParameters, Updates.CooldownParametersCPV1UpdatePayload cp)
            ProtoFields.UpdatePayload'ElectionDifficultyUpdate edUpdate -> do
                ed <- fromProto edUpdate
                return (Updates.UpdateElectionDifficulty, Updates.ElectionDifficultyUpdatePayload ed)
            ProtoFields.UpdatePayload'EuroPerEnergyUpdate epeUpdate -> do
                er <- fromProto epeUpdate
                return (Updates.UpdateEuroPerEnergy, Updates.EuroPerEnergyUpdatePayload er)
            ProtoFields.UpdatePayload'FoundationAccountUpdate faUpdate -> do
                addr <- fromProto faUpdate
                return (Updates.UpdateFoundationAccount, Updates.FoundationAccountUpdatePayload addr)
            ProtoFields.UpdatePayload'GasRewardsUpdate grUpdate -> do
                gr <- fromProto grUpdate
                return (Updates.UpdateGASRewards, Updates.GASRewardsUpdatePayload gr)
            ProtoFields.UpdatePayload'Level1Update l1Update -> do
                u <- l1Update ^. ProtoFields.maybe'updateType
                case u of
                    ProtoFields.Level1Update'Level1KeysUpdate l1kUpdate -> do
                        l1kl1uKeys <- fst <$> fromProto l1kUpdate
                        return (Updates.UpdateLevel1Keys, Updates.Level1UpdatePayload Updates.Level1KeysLevel1Update{..})
                    ProtoFields.Level1Update'Level2KeysUpdateV0 l2kUpdateV0 -> do
                        l2kl1uAuthorizations <- fromProto l2kUpdateV0
                        return (Updates.UpdateLevel2Keys, Updates.Level1UpdatePayload Updates.Level2KeysLevel1Update{..})
                    ProtoFields.Level1Update'Level2KeysUpdateV1 l2kUpdateV1 -> do
                        l2kl1uAuthorizationsV1 <- fromProto l2kUpdateV1
                        return
                            ( Updates.UpdateLevel2Keys,
                              Updates.Level1UpdatePayload
                                Updates.Level2KeysLevel1UpdateV1{..}
                            )
            ProtoFields.UpdatePayload'MicroCcdPerEuroUpdate mcpeUpdate -> do
                er <- fromProto mcpeUpdate
                return (Updates.UpdateMicroGTUPerEuro, Updates.MicroGTUPerEuroUpdatePayload er)
            ProtoFields.UpdatePayload'MintDistributionUpdate mdUpdate -> do
                md <- fromProto mdUpdate
                return (Updates.UpdateMintDistribution, Updates.MintDistributionUpdatePayload md)
            ProtoFields.UpdatePayload'MintDistributionCpv1Update mdcv1Update -> do
                md <- fromProto mdcv1Update
                return (Updates.UpdateMintDistribution, Updates.MintDistributionCPV1UpdatePayload md)
            ProtoFields.UpdatePayload'PoolParametersCpv1Update ppcv1Update -> do
                pp <- fromProto ppcv1Update
                return (Updates.UpdatePoolParameters, Updates.PoolParametersCPV1UpdatePayload pp)
            ProtoFields.UpdatePayload'ProtocolUpdate pUpdate -> do
                pu <- fromProto pUpdate
                return (Updates.UpdateProtocol, Updates.ProtocolUpdatePayload pu)
            ProtoFields.UpdatePayload'RootUpdate rUpdate -> do
                (uType, update) <- fromProto rUpdate
                return (uType, Updates.RootUpdatePayload update)
            ProtoFields.UpdatePayload'TimeParametersCpv1Update tpcv1Update -> do
                tp <- fromProto tpcv1Update
                return (Updates.UpdateTimeParameters, Updates.TimeParametersCPV1UpdatePayload tp)
            ProtoFields.UpdatePayload'TransactionFeeDistributionUpdate tfdUpdate -> do
                tfd <- fromProto tfdUpdate
                return (Updates.UpdateTransactionFeeDistribution, Updates.TransactionFeeDistributionUpdatePayload tfd)

instance FromProto Proto.BlockItemSummary where
    type Output' Proto.BlockItemSummary = TransactionSummary
    fromProto biSummary = do
        -- Common block item summary fields
        tsIndex <- TransactionIndex . deMkWord64 <$> biSummary ^? ProtoFields.index
        tsEnergyCost <- fromProto =<< biSummary ^? ProtoFields.energyCost
        tsHash <- fromProto =<< biSummary ^? ProtoFields.hash
        -- Discern between transactions
        bis <- biSummary ^. Proto.maybe'details
        case bis of
            -- Account creation
            ProtoFields.BlockItemSummary'AccountCreation aCreation -> do
                let tsSender = Nothing
                tsCost <- Just $ Amount 0
                credType <- fromProto =<< aCreation ^? ProtoFields.credentialType
                let tsType = TSTCredentialDeploymentTransaction credType
                tsResult <- fromProto aCreation
                return TransactionSummary{..}
            -- Account transaction
            ProtoFields.BlockItemSummary'AccountTransaction aTransaction -> do
                sender <- aTransaction ^? ProtoFields.sender
                let tsSender = fromProto sender
                tsCost <- fromProto =<< aTransaction ^? ProtoFields.cost
                (tType, tsResult) <- fromProto (sender, aTransaction)
                let tsType = TSTAccountTransaction tType
                return TransactionSummary{..}
            ProtoFields.BlockItemSummary'Update update -> do
                let tsSender = Nothing
                tsCost <- Just $ Amount 0
                (tType, tsResult) <- do
                    ueEffectiveTime <- fromProto =<< update ^? ProtoFields.effectiveTime
                    (uType, uePayload) <- fromProto =<< update ^? ProtoFields.payload
                    return (uType, TxSuccess [UpdateEnqueued{..}])
                let tsType = TSTUpdateTransaction tType
                return TransactionSummary{..}

instance FromProto (Proto.AccountAddress, Proto.AccountTransactionDetails) where
    type Output' (Proto.AccountAddress, Proto.AccountTransactionDetails) = (Maybe TransactionType, ValidResult)
    fromProto (senderAcc, atDetails) = do
        sender <- fromProto senderAcc
        tSender <- deMkSerialize =<< atDetails ^? ProtoFields.sender
        ate <- atDetails ^. ProtoFields.effects . ProtoFields.maybe'effect
        case ate of
            ProtoFields.AccountTransactionEffects'AccountTransfer' aTransfer -> do
                etAmount <- fromProto =<< aTransfer ^? ProtoFields.amount
                etTo <- deMkSerialize =<< aTransfer ^? ProtoFields.receiver
                let etFrom = tSender
                let memo = fromProto =<< aTransfer ^. ProtoFields.maybe'memo
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
                vrEvents <- mapM (\e -> fromProto (senderAcc, e)) =<< bConfigured ^? ProtoFields.events
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
                ebsreBakerId <- fromProto =<< breUpdated ^? ProtoFields.bakerId
                ebsreRestakeEarnings <- breUpdated ^? ProtoFields.restakeEarnings
                return (Just TTUpdateBakerRestakeEarnings, TxSuccess [BakerSetRestakeEarnings{..}])
            ProtoFields.AccountTransactionEffects'BakerStakeUpdated' bsUpdated -> do
                let vrEvents = maybeToList $ case bsUpdated ^. ProtoFields.maybe'update of
                        Nothing -> Nothing
                        Just update -> do
                            increased <- update ^? ProtoFields.increased
                            let ebsiAccount = sender
                            ebsiBakerId <- fromProto =<< update ^? ProtoFields.bakerId
                            ebsiNewStake <- fromProto =<< update ^? ProtoFields.newStake
                            if increased then return BakerStakeIncreased{..} else return BakerStakeDecreased{..}
                return (Just TTUpdateBakerStake, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'ContractInitialized cInitialized -> do
                ecContractVersion <- fromProto =<< cInitialized ^? ProtoFields.contractVersion
                ecRef <- fromProto =<< cInitialized ^? ProtoFields.originRef
                ecAddress <- fromProto =<< cInitialized ^? ProtoFields.address
                ecAmount <- fromProto =<< cInitialized ^? ProtoFields.amount
                ecInitName <- fromProto =<< cInitialized ^? ProtoFields.initName
                ecEvents <- mapM fromProto =<< cInitialized ^? ProtoFields.events
                return (Just TTInitContract, TxSuccess [ContractInitialized{..}])
            ProtoFields.AccountTransactionEffects'ContractUpdateIssued' cuIssued -> do
                vrEvents <- mapM fromProto =<< cuIssued ^? ProtoFields.effects
                return (Just TTUpdate, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'CredentialKeysUpdated ckUpdated -> do
                ckuCredId <- fst <$> fromProto ckUpdated
                return (Just TTUpdateCredentialKeys, TxSuccess [CredentialKeysUpdated ckuCredId])
            ProtoFields.AccountTransactionEffects'CredentialsUpdated' cUpdated -> do
                let cuAccount = sender
                cuNewCredIds <- mapM (fmap fst . fromProto) =<< cUpdated ^? ProtoFields.newCredIds
                cuRemovedCredIds <- mapM (fmap fst . fromProto) =<< cUpdated ^? ProtoFields.removedCredIds
                cuNewThreshold <- fromProto =<< cUpdated ^? ProtoFields.newThreshold
                return (Just TTUpdateCredentials, TxSuccess [CredentialsUpdated{..}])
            ProtoFields.AccountTransactionEffects'DataRegistered dRegistered -> do
                drData <- fromProto dRegistered
                return (Just TTRegisterData, TxSuccess [DataRegistered{..}])
            ProtoFields.AccountTransactionEffects'DelegationConfigured' dConfigured -> do
                vrEvents <- mapM (\e -> fromProto (senderAcc, e)) =<< dConfigured ^? ProtoFields.events
                return (Just TTConfigureDelegation, TxSuccess{..})
            ProtoFields.AccountTransactionEffects'EncryptedAmountTransferred' eaTransferred -> do
                -- Removed encrypted amounts.
                removed <- eaTransferred ^? ProtoFields.removed
                earAccount <- fromProto =<< removed ^? ProtoFields.account
                earNewAmount <- fromProto =<< removed ^? ProtoFields.newAmount
                earInputAmount <- fromProto =<< removed ^? ProtoFields.inputAmount
                earUpToIndex <- EncryptedAmountAggIndex <$> removed ^? ProtoFields.upToIndex
                -- Added encrypted amounts.
                added <- eaTransferred ^? ProtoFields.added
                neaAccount <- fromProto =<< added ^? ProtoFields.receiver
                neaNewIndex <- EncryptedAmountIndex <$> added ^? ProtoFields.newIndex
                neaEncryptedAmount <- fromProto =<< added ^? ProtoFields.encryptedAmount
                -- The memo.
                let memo = fromProto =<< eaTransferred ^. ProtoFields.maybe'memo
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
                vrRejectReason <- fromProto =<< none ^? ProtoFields.rejectReason
                let transactionType = fromProto =<< none ^. ProtoFields.maybe'transactionType
                return (transactionType, TxReject{..})
            ProtoFields.AccountTransactionEffects'TransferredToEncrypted ttEncrypted -> do
                eaaAccount <- fromProto =<< ttEncrypted ^? ProtoFields.account
                eaaNewAmount <- fromProto =<< ttEncrypted ^? ProtoFields.newAmount
                eaaAmount <- fromProto =<< ttEncrypted ^? ProtoFields.amount
                return (Just TTTransferToEncrypted, TxSuccess [EncryptedSelfAmountAdded{..}])
            ProtoFields.AccountTransactionEffects'TransferredToPublic' ttPublic -> do
                -- Amount added by decryption.
                aabdAmount <- fromProto =<< ttPublic ^? ProtoFields.amount
                -- Removed encrypted amounts.
                removed <- ttPublic ^? ProtoFields.removed
                aabdAccount <- fromProto =<< removed ^? ProtoFields.account
                earAccount <- fromProto =<< removed ^? ProtoFields.account
                earNewAmount <- fromProto =<< removed ^? ProtoFields.newAmount
                earInputAmount <- fromProto =<< removed ^? ProtoFields.inputAmount
                earUpToIndex <- EncryptedAmountAggIndex <$> removed ^? ProtoFields.upToIndex
                return (Just TTTransferToPublic, TxSuccess [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}])
            ProtoFields.AccountTransactionEffects'TransferredWithSchedule' twSchedule -> do
                let etwsFrom = sender
                etwsTo <- fromProto =<< twSchedule ^? ProtoFields.receiver
                etwsAmount <- mapM fromProto =<< twSchedule ^? ProtoFields.amount
                let memo = fromProto =<< twSchedule ^. ProtoFields.maybe'memo
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

instance FromProto (Proto.AccountAddress, Proto.DelegationEvent) where
    type Output' (Proto.AccountAddress, Proto.DelegationEvent) = Event
    fromProto (senderAcc, dEvent) = do
        sender <- fromProto senderAcc
        de <- dEvent ^. ProtoFields.maybe'event
        case de of
            Proto.DelegationEvent'DelegationAdded dAdded -> do
                let edaAccount = sender
                edaDelegatorId <- fromProto dAdded
                return DelegationAdded{..}
            ProtoFields.DelegationEvent'DelegationStakeIncreased' dsIncreased -> do
                let edsiAccount = sender
                edsiDelegatorId <- fromProto =<< dsIncreased ^? ProtoFields.delegatorId
                edsiNewStake <- fromProto =<< dsIncreased ^? ProtoFields.newStake
                return DelegationStakeIncreased{..}
            ProtoFields.DelegationEvent'DelegationStakeDecreased' dsDecreased -> do
                let edsdAccount = sender
                edsdDelegatorId <- fromProto =<< dsDecreased ^? ProtoFields.delegatorId
                edsdNewStake <- fromProto =<< dsDecreased ^? ProtoFields.newStake
                return DelegationStakeDecreased{..}
            ProtoFields.DelegationEvent'DelegationSetRestakeEarnings' dsrEarnings -> do
                let edsreAccount = sender
                edsreDelegatorId <- fromProto =<< dsrEarnings ^? ProtoFields.delegatorId
                edsreRestakeEarnings <- dsrEarnings ^? ProtoFields.restakeEarnings
                return DelegationSetRestakeEarnings{..}
            ProtoFields.DelegationEvent'DelegationSetDelegationTarget' dsdTarget -> do
                let edsdtAccount = sender
                edsdtDelegatorId <- fromProto =<< dsdTarget ^? ProtoFields.delegatorId
                edsdtDelegationTarget <- fromProto =<< dsdTarget ^? ProtoFields.delegationTarget
                return DelegationSetDelegationTarget{..}
            ProtoFields.DelegationEvent'DelegationRemoved dRemoved -> do
                let edrAccount = sender
                edrDelegatorId <- fromProto dRemoved
                return DelegationRemoved{..}

instance FromProto (Proto.AccountAddress, Proto.BakerEvent) where
    type Output' (Proto.AccountAddress, Proto.BakerEvent) = Event
    fromProto (senderAcc, bEvent) = do
        sender <- fromProto senderAcc
        be <- bEvent ^. ProtoFields.maybe'event
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
                ebsiBakerId <- fromProto =<< bsIncreased ^? ProtoFields.bakerId
                ebsiNewStake <- fromProto =<< bsIncreased ^? ProtoFields.newStake
                return BakerStakeIncreased{..}
            ProtoFields.BakerEvent'BakerStakeDecreased' bsDecreased -> do
                let ebsiAccount = sender
                ebsiBakerId <- fromProto =<< bsDecreased ^? ProtoFields.bakerId
                ebsiNewStake <- fromProto =<< bsDecreased ^? ProtoFields.newStake
                return BakerStakeDecreased{..}
            ProtoFields.BakerEvent'BakerRestakeEarningsUpdated' breUpdated -> do
                let ebsreAccount = sender
                ebsreBakerId <- fromProto =<< breUpdated ^? ProtoFields.bakerId
                ebsreRestakeEarnings <- breUpdated ^? ProtoFields.restakeEarnings
                return BakerSetRestakeEarnings{..}
            ProtoFields.BakerEvent'BakerKeysUpdated bkUpdated -> do
                (ebkuBakerId, ebkuAccount, ebkuSignKey, ebkuElectionKey, ebkuAggregationKey) <- fromProto bkUpdated
                return BakerKeysUpdated{..}
            ProtoFields.BakerEvent'BakerSetOpenStatus' bsoStatus -> do
                let ebsosAccount = sender
                ebsosBakerId <- fromProto =<< bsoStatus ^? ProtoFields.bakerId
                ebsosOpenStatus <- fromProto =<< bsoStatus ^? ProtoFields.openStatus
                return BakerSetOpenStatus{..}
            ProtoFields.BakerEvent'BakerSetMetadataUrl' bsmUrl -> do
                let ebsmuAccount = sender
                ebsmuBakerId <- fromProto =<< bsmUrl ^? ProtoFields.bakerId
                ebsmuMetadataURL <- deMkUrlText bsmUrl
                return BakerSetMetadataURL{..}
            ProtoFields.BakerEvent'BakerSetTransactionFeeCommission' bstfCommission -> do
                let ebstfcAccount = sender
                ebstfcBakerId <- fromProto =<< bstfCommission ^? ProtoFields.bakerId
                ebstfcTransactionFeeCommission <- fromProto =<< bstfCommission ^? ProtoFields.transactionFeeCommission
                return BakerSetTransactionFeeCommission{..}
            ProtoFields.BakerEvent'BakerSetBakingRewardCommission' bsbCommission -> do
                let ebsbrcAccount = sender
                ebsbrcBakerId <- fromProto =<< bsbCommission ^? ProtoFields.bakerId
                ebsbrcBakingRewardCommission <- fromProto =<< bsbCommission ^? ProtoFields.bakingRewardCommission
                return BakerSetBakingRewardCommission{..}
            ProtoFields.BakerEvent'BakerSetFinalizationRewardCommission' bsfrCommission -> do
                let ebsfrcAccount = sender
                ebsfrcBakerId <- fromProto =<< bsfrCommission ^? ProtoFields.bakerId
                ebsfrcFinalizationRewardCommission <- fromProto =<< bsfrCommission ^? ProtoFields.finalizationRewardCommission
                return BakerSetFinalizationRewardCommission{..}

instance FromProto Proto.BlockItemStatus where
    type Output' Proto.BlockItemStatus = QueryTypes.TransactionStatus
    fromProto biStatus = do
        bis <- biStatus ^. ProtoFields.maybe'status
        case bis of
            Proto.BlockItemStatus'Received _ -> return QueryTypes.Received
            Proto.BlockItemStatus'Finalized' finalized -> do
                outcome <- finalized ^? ProtoFields.outcome
                (tHash, tSumm) <- fromOutcome outcome
                return $ QueryTypes.Finalized tHash tSumm
            Proto.BlockItemStatus'Committed' committed -> do
                outcomes <- mapM fromOutcome =<< committed ^? ProtoFields.outcomes
                return $ QueryTypes.Committed (Map.fromList outcomes)
      where
        fromOutcome outcome = do
            bHash <- fromProto =<< outcome ^? ProtoFields.blockHash
            let tSumm = fromProto =<< outcome ^? ProtoFields.outcome
            return (bHash, tSumm)

instance FromProto Proto.FinalizationIndex where
    type Output' Proto.FinalizationIndex = FinalizationIndex
    fromProto = return . FinalizationIndex . deMkWord64

instance FromProto Proto.FinalizationSummaryParty where
    type Output' Proto.FinalizationSummaryParty = QueryTypes.FinalizationSummaryParty
    fromProto fsParty = do
        fspBakerId <- fromProto =<< fsParty ^? ProtoFields.baker
        fspWeight <- fromIntegral <$> fsParty ^? ProtoFields.weight
        fspSigned <- fsParty ^? ProtoFields.signed
        return QueryTypes.FinalizationSummaryParty{..}

instance FromProto Proto.BlockFinalizationSummary where
    type Output' Proto.BlockFinalizationSummary = FinalizationSummary
    fromProto bfSummary = do
        bfs <- bfSummary ^. ProtoFields.maybe'summary
        case bfs of
            ProtoFields.BlockFinalizationSummary'None _ -> Nothing
            ProtoFields.BlockFinalizationSummary'Record record -> do
                blockHash <- fromProto =<< record ^? ProtoFields.block
                finIndex <- fromProto =<< record ^? ProtoFields.index
                delay <- fromProto =<< record ^? ProtoFields.delay
                finalizers <- mapM fromProto =<< record ^? ProtoFields.finalizers
                Just FinalizationProof{..}

-- |Finalization summary that may or may not be part of the block.
data FinalizationSummary
    = -- |There is no finalization data in the block.
      None
    | -- |There is a single finalization record in the block.
      FinalizationProof
        { blockHash :: !BlockHash,
          finIndex :: !FinalizationIndex,
          delay :: !BlockHeight,
          finalizers :: ![QueryTypes.FinalizationSummaryParty]
        }

-- |Catchup status of the peer.
data PeerCatchupStatus
    = -- |The peer does not have any data unknown to us. If we receive a message
      -- from the peer that refers to unknown data (e.g., an unknown block) the
      -- peer is marked as pending.
      UpToDate
    | -- |The peer might have some data unknown to us. A peer can be in this state
      -- either because it sent a message that refers to data unknown to us, or
      -- before we have established a baseline with it. The latter happens during
      -- node startup, as well as upon protocol updates until the initial catchup
      -- handshake completes.
      Pending
    | -- |The node is currently catching up by requesting blocks from this peer.
      -- There will be at most one peer with this status at a time. Once the peer
      -- has responded to the request, its status will be changed to either `UpToDate`
      -- or `Pending`.
      CatchingUp

-- |Network statistics for the peer.
data NetworkStats = NetworkStats
    { -- |The number of messages sent to the peer.
      -- Packets are blocks, transactions, catchup messages, finalization records
      -- and network messages such as pings and peer requests.
      packetsSent :: !Integer,
      -- |The number of messages received from the peer.
      -- Packets are blocks, transactions, catchup messages, finalization records
      -- and network messages such as pings and peer requests.
      packetsReceived :: !Integer,
      -- |The connection latency (i.e., ping time) in milliseconds.
      latency :: !Integer
    }

-- |Network related peer statistics.
data PeerInfo = PeerInfo
    { peerId :: !Text,
      socketAddress :: !IpSocketAddress,
      networkStats :: !NetworkStats,
      consensusInfo :: !(Maybe PeerCatchupStatus)
    }

-- |Network related peer statistics.
type PeersInfo = [PeerInfo]

instance FromProto Proto.IpSocketAddress where
    type Output' Proto.IpSocketAddress = IpSocketAddress
    fromProto ipsAddress = do
        -- FIXME: Should this be validated?
        ip <- fromProto =<< ipsAddress ^? ProtoFields.ip
        port <- fromProto =<< ipsAddress ^? ProtoFields.port
        return (ip, port)

instance FromProto Proto.PeersInfo'Peer'NetworkStats where
    type Output' Proto.PeersInfo'Peer'NetworkStats = NetworkStats
    fromProto nStats = do
        packetsSent <- fromIntegral <$> nStats ^? ProtoFields.packetsSent
        packetsReceived <- fromIntegral <$> nStats ^? ProtoFields.packetsReceived
        latency <- fromIntegral <$> nStats ^? ProtoFields.latency
        return NetworkStats{..}

instance FromProto Proto.PeersInfo'Peer'CatchupStatus where
    type Output' Proto.PeersInfo'Peer'CatchupStatus = PeerCatchupStatus
    fromProto pcStatus = do
        case pcStatus of
            ProtoFields.PeersInfo'Peer'UPTODATE -> return UpToDate
            ProtoFields.PeersInfo'Peer'CATCHINGUP -> return CatchingUp
            ProtoFields.PeersInfo'Peer'PENDING -> return Pending
            ProtoFields.PeersInfo'Peer'CatchupStatus'Unrecognized _ -> Nothing

instance FromProto Proto.PeersInfo'Peer where
    type Output' Proto.PeersInfo'Peer = PeerInfo
    fromProto pInfo = do
        peerId <- pInfo ^? ProtoFields.peerId . ProtoFields.value
        socketAddress <- fromProto =<< pInfo ^? ProtoFields.socketAddress
        networkStats <- fromProto =<< pInfo ^? ProtoFields.networkStats
        consensusInfo <- do
            c <- pInfo ^. ProtoFields.maybe'consensusInfo
            return $ case c of
                ProtoFields.PeersInfo'Peer'Bootstrapper _ -> Nothing
                ProtoFields.PeersInfo'Peer'NodeCatchupStatus ncStatus -> fromProto ncStatus
        return PeerInfo{..}

instance FromProto Proto.PeersInfo where
    type Output' Proto.PeersInfo = PeersInfo
    fromProto pInfo = mapM fromProto =<< pInfo ^? Proto.peers

instance FromProto Proto.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo where
    type Output' Proto.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo = PassiveCommitteeInfo
    fromProto pcInfo = do
        case pcInfo of
            ProtoFields.NodeInfo'BakerConsensusInfo'NOT_IN_COMMITTEE -> do
                return NotInCommittee
            ProtoFields.NodeInfo'BakerConsensusInfo'ADDED_BUT_NOT_ACTIVE_IN_COMMITTEE -> do
                return AddedButNotActiveInCommittee
            ProtoFields.NodeInfo'BakerConsensusInfo'ADDED_BUT_WRONG_KEYS -> do
                return AddedButWrongKeys
            ProtoFields.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo'Unrecognized _ -> do
                Nothing

-- |The committee information of a node which is configured with
-- baker keys but is somehow is _not_ part of the current baking
-- committee.
data PassiveCommitteeInfo
    = -- |The node is started with baker keys however it is currently not in
      -- the baking committee. The node is _not_ baking.
      NotInCommittee
    | -- |The account is registered as a baker but not in the current `Epoch`.
      -- The node is _not_ baking.
      AddedButNotActiveInCommittee
    | -- |The node has configured invalid baker keys i.e., the configured
      -- baker keys do not match the current keys on the baker account.
      -- The node is _not_ baking.
      AddedButWrongKeys

-- |Status of the baker configured node.
data BakerConsensusInfoStatus
    = -- |The node is currently not baking.
      PassiveBaker !PassiveCommitteeInfo
    | -- |Node is configured with baker keys and active in the current baking committee
      ActiveBakerCommitteeInfo
    | -- | Node is configured with baker keys and active in the current finalizer
      -- committee (and also baking committee).
      ActiveFinalizerCommitteeInfo

instance FromProto Proto.NodeInfo'BakerConsensusInfo where
    type Output' Proto.NodeInfo'BakerConsensusInfo = BakerConsensusInfo
    fromProto bcInfo = do
        bakerId <- fromIntegral <$> bcInfo ^? ProtoFields.bakerId . ProtoFields.value
        status <- do
            st <- bcInfo ^. ProtoFields.maybe'status
            case st of
                ProtoFields.NodeInfo'BakerConsensusInfo'ActiveBakerCommitteeInfo' _ ->
                    return ActiveBakerCommitteeInfo
                ProtoFields.NodeInfo'BakerConsensusInfo'ActiveFinalizerCommitteeInfo' _ ->
                    return ActiveFinalizerCommitteeInfo
                ProtoFields.NodeInfo'BakerConsensusInfo'PassiveCommitteeInfo pcInfo ->
                    PassiveBaker <$> fromProto pcInfo
        return BakerConsensusInfo{..}

-- |Consensus info for a node configured with baker keys.
data BakerConsensusInfo = BakerConsensusInfo
    { bakerId :: !Integer,
      status :: !BakerConsensusInfoStatus
    }

instance FromProto Proto.NodeInfo where
    type Output' Proto.NodeInfo = NodeInfo
    fromProto nInfo = do
        nDetails <- nInfo ^. ProtoFields.maybe'details
        details <- case nDetails of
            ProtoFields.NodeInfo'Bootstrapper _ ->
                return NodeBootstrapper
            ProtoFields.NodeInfo'Node' node -> do
                n <- node ^. ProtoFields.maybe'consensusStatus
                case n of
                    ProtoFields.NodeInfo'Node'NotRunning _ ->
                        return NodeNotRunning
                    ProtoFields.NodeInfo'Node'Passive _ ->
                        return NodePassive
                    ProtoFields.NodeInfo'Node'Active cInfo ->
                        NodeActive <$> fromProto cInfo
        peerVersion <- nInfo ^? ProtoFields.peerVersion
        localTime <- fromProto =<< nInfo ^? ProtoFields.localTime
        peerUptime <- fromProto =<< nInfo ^? ProtoFields.peerUptime
        networkInfo <- fromProto =<< nInfo ^? ProtoFields.networkInfo
        return NodeInfo{..}

-- |Consensus related details of the peer.
data NodeDetails
    = -- |The peer is of type `Bootstrapper` is not participating in consensus
      -- and thus has no catchup status.
      NodeBootstrapper
    | -- |The node is not running consensus. This is the case only when the node
      -- is not supporting the protocol on the chain. The node does not process
      -- blocks.
      NodeNotRunning
    | -- | Consensus info for a node that is not configured with baker keys.
      -- The node is only processing blocks and relaying blocks and transactions
      -- and responding to catchup messages.
      NodePassive
    | -- | The node is configured with baker credentials and consensus is running.
      NodeActive !BakerConsensusInfo

instance FromProto Proto.NodeInfo'NetworkInfo where
    type Output' Proto.NodeInfo'NetworkInfo = NetworkInfo
    fromProto nInfo = do
        nodeId <- nInfo ^? ProtoFields.nodeId . ProtoFields.value
        peerTotalSent <- fromIntegral <$> nInfo ^? ProtoFields.peerTotalSent
        peerTotalReceived <- fromIntegral <$> nInfo ^? ProtoFields.peerTotalReceived
        avgBpsIn <- fromIntegral <$> nInfo ^? ProtoFields.avgBpsIn
        avgBpsOut <- fromIntegral <$> nInfo ^? ProtoFields.avgBpsOut
        return NetworkInfo{..}

-- |Network related information of the node.
data NetworkInfo = NetworkInfo
    { -- |The node id.
      nodeId :: !Text,
      -- |Total number of packets sent by the node.
      peerTotalSent :: !Word64,
      -- |Total number of packets received by the node.
      peerTotalReceived :: !Word64,
      -- |Average outbound throughput in bytes per second.
      avgBpsIn :: !Word64,
      -- |Average inbound throughput in bytes per second.
      avgBpsOut :: !Word64
    }

-- |Various information about the node.
data NodeInfo = NodeInfo
    { -- |The version of the node.
      peerVersion :: !Text,
      -- |The local time of the node.
      localTime :: !Timestamp,
      -- | Number of milliseconds that the node has been alive.
      peerUptime :: !Duration,
      -- | Information related to the p2p protocol.
      networkInfo :: !NetworkInfo,
      -- | Various details about the node.
      details :: !NodeDetails
    }

instance FromProto Proto.ChainParametersV0 where
    type Output' Proto.ChainParametersV0 = ChainParameterOutput
    fromProto cParams = do
        _cpElectionDifficulty <- fromProto =<< cParams ^? ProtoFields.electionDifficulty
        _cpExchangeRates <- do
            euroPerEnergy <- fromProto =<< cParams ^? ProtoFields.euroPerEnergy
            microCcdPerEuro <- fromProto =<< cParams ^? ProtoFields.microCcdPerEuro
            return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
        _cpCooldownParameters <- do
            epoch <- fromProto =<< cParams ^? ProtoFields.bakerCooldownEpochs
            return $ Parameters.CooldownParametersV0 epoch
        let _cpTimeParameters = Parameters.TimeParametersV0
        _cpAccountCreationLimit <- fromProto =<< cParams ^? ProtoFields.accountCreationLimit
        _cpRewardParameters <- do
            _rpMintDistribution <- fromProto =<< cParams ^? ProtoFields.mintDistribution
            _rpTransactionFeeDistribution <- fromProto =<< cParams ^? ProtoFields.transactionFeeDistribution
            _rpGASRewards <- fromProto =<< cParams ^? ProtoFields.gasRewards
            return Parameters.RewardParameters{..}
        -- FIXME: ProtoFields.foundationAccount has type AccountAddress, but the type expects an AccountIndex.
        --        Is it possible to convert this?
        _cpFoundationAccount <- Nothing -- <-|
        _cpPoolParameters <- do
            thresh <- fromProto =<< cParams ^? ProtoFields.minimumThresholdForBaking
            return $ Parameters.PoolParametersV0 thresh
        let ecpParams = Parameters.ChainParameters{..}
        rootKeys <- fmap snd . fromProto =<< cParams ^? ProtoFields.rootKeys
        level1Keys <- fmap fst . fromProto =<< cParams ^? ProtoFields.level1Keys
        level2Keys <- fromProto =<< cParams ^? ProtoFields.level2Keys
        let ecpKeys = Updates.UpdateKeysCollection{..}
        return $ ChainParameterOutputV0 ecpParams ecpKeys

instance FromProto Proto.ChainParametersV1 where
    type Output' Proto.ChainParametersV1 = ChainParameterOutput
    fromProto cParams = do
        _cpElectionDifficulty <- fromProto =<< cParams ^? ProtoFields.electionDifficulty
        _cpExchangeRates <- do
            euroPerEnergy <- fromProto =<< cParams ^? ProtoFields.euroPerEnergy
            microCcdPerEuro <- fromProto =<< cParams ^? ProtoFields.microCcdPerEuro
            return $ Parameters.makeExchangeRates euroPerEnergy microCcdPerEuro
        _cpCooldownParameters <- fromProto =<< cParams ^? ProtoFields.cooldownParameters
        _cpTimeParameters <- fromProto =<< cParams ^? ProtoFields.timeParameters
        _cpAccountCreationLimit <- fromProto =<< cParams ^? ProtoFields.accountCreationLimit
        _cpRewardParameters <- do
            _rpMintDistribution <- fromProto =<< cParams ^? ProtoFields.mintDistribution
            _rpTransactionFeeDistribution <- fromProto =<< cParams ^? ProtoFields.transactionFeeDistribution
            _rpGASRewards <- fromProto =<< cParams ^? ProtoFields.gasRewards
            return Parameters.RewardParameters{..}
        -- FIXME: ProtoFields.foundationAccount has type AccountAddress, but the type expects an AccountIndex.
        --        Is it possible to convert this?
        _cpFoundationAccount <- Nothing -- <-|
        _cpPoolParameters <- fromProto =<< cParams ^? ProtoFields.poolParameters
        let ecpParams = Parameters.ChainParameters{..}
        rootKeys <- fmap snd . fromProto =<< cParams ^? ProtoFields.rootKeys
        level1Keys <- fmap fst . fromProto =<< cParams ^? ProtoFields.level1Keys
        level2Keys <- fromProto =<< cParams ^? ProtoFields.level2Keys
        let ecpKeys = Updates.UpdateKeysCollection{..}
        return $ ChainParameterOutputV1 ecpParams ecpKeys

instance FromProto Proto.Epoch where
    type Output' Proto.Epoch = Epoch
    fromProto = return . deMkWord64

instance FromProto Proto.CredentialsPerBlockLimit where
    type Output' Proto.CredentialsPerBlockLimit = CredentialsPerBlockLimit
    fromProto = deMkWord16

data ChainParameterOutput
    = ChainParameterOutputV0
        !(Parameters.ChainParameters' 'ChainParametersV0)
        !(Updates.UpdateKeysCollection 'ChainParametersV0)
    | ChainParameterOutputV1
        !(Parameters.ChainParameters' 'ChainParametersV1)
        !(Updates.UpdateKeysCollection 'ChainParametersV1)

instance FromProto Proto.ChainParameters where
    type Output' Proto.ChainParameters = ChainParameterOutput
    fromProto cParams = do
        cp <- cParams ^. Proto.maybe'parameters
        case cp of
            Proto.ChainParameters'V0 v0 -> fromProto v0
            Proto.ChainParameters'V1 v1 -> fromProto v1

instance FromProto Proto.CryptographicParameters where
    type Output' Proto.CryptographicParameters = CryptographicParameters
    fromProto cParams =
        do
            genString <- cParams ^? ProtoFields.genesisString
            bpGens <- cParams ^? ProtoFields.bulletproofGenerators
            occKey <- cParams ^? ProtoFields.onChainCommitmentKey
            createGlobalContext genString bpGens occKey

data SendBlockItemInput
    = AccountTransaction !Transactions.AccountTransaction
    | AccountCreation !Transactions.AccountCreation
    | UpdateInstruction !Updates.UpdateInstruction

instance ToProto SendBlockItemInput where
    type Output SendBlockItemInput = Proto.SendBlockItemRequest
    toProto sbi = Proto.make $
        case sbi of
            AccountTransaction aTransaction ->
                ProtoFields.accountTransaction .= toProto aTransaction
            AccountCreation aCreation ->
                ProtoFields.credentialDeployment .= toProto aCreation
            UpdateInstruction uInstruction ->
                ProtoFields.updateInstruction .= toProto uInstruction

-- |Get cryptographic parameters in a given block.
getCryptographicParametersV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe CryptographicParameters))
getCryptographicParametersV2 bHash = withUnaryCoreV2 (callV2 @"getCryptographicParameters") msg ((fmap . fmap) fromProto)
  where
    msg = toProto bHash

-- |Get values of chain parameters in a given block.
getBlockChainParametersV2 ::
    (MonadIO m) =>
    BlockHashInput ->
    ClientMonad m (GRPCResult (Maybe ChainParameterOutput))
getBlockChainParametersV2 bHash = withUnaryCoreV2 (callV2 @"getBlockChainParameters") msg ((fmap . fmap) fromProto)
  where
    msg = toProto bHash

-- |Get information about the node. See `NodeInfo` for details.
getNodeInfoV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe NodeInfo))
getNodeInfoV2 = withUnaryCoreV2 (callV2 @"getNodeInfo") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage

-- Get a list of the peers that the node is connected to and network-related information for each peer.
getPeersInfoV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe PeersInfo))
getPeersInfoV2 = withUnaryCoreV2 (callV2 @"getPeersInfo") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage

-- |Get a summary of the finalization data in a given block.
getBlockFinalizationSummaryV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe FinalizationSummary))
getBlockFinalizationSummaryV2 tHash = withUnaryCoreV2 (callV2 @"getBlockFinalizationSummary") msg ((fmap . fmap) fromProto)
  where
    msg = toProto tHash

-- |Get the status of and information about a specific block item (transaction).
getBlockItemStatusV2 :: (MonadIO m) => TransactionHash -> ClientMonad m (GRPCResult (Maybe QueryTypes.TransactionStatus))
getBlockItemStatusV2 tHash = withUnaryCoreV2 (callV2 @"getBlockItemStatus") msg ((fmap . fmap) fromProto)
  where
    msg = toProto tHash

-- |Send a block item. A block item is either an @AccountTransaction@, which is
-- a transaction signed and paid for by an account, a @CredentialDeployment@,
-- which creates a new account, or an @UpdateInstruction@, which is an
-- instruction to change some parameters of the chain. Update instructions can
-- only be sent by the governance committee.
--
-- Returns a hash of the block item, which can be used with
-- `GetBlockItemStatus`.
sendBlockItemV2 :: (MonadIO m) => SendBlockItemInput -> ClientMonad m (GRPCResult (Maybe TransactionHash))
sendBlockItemV2 sbiInput = withUnaryCoreV2 (callV2 @"sendBlockItem") msg ((fmap . fmap) fromProto)
  where
    msg = toProto sbiInput

-- FIXME: Ergonomics: Should input, resp. output be ByteString?'

-- |Get the value at a specific key of a contract state. In contrast to
-- `GetInstanceState` this is more efficient, but requires the user to know
-- the specific key to look for.
instanceStateLookupV2 ::
    (MonadIO m) =>
    BlockHashInput ->
    ContractAddress ->
    ByteString ->
    ClientMonad m (GRPCResult (Maybe ByteString))
instanceStateLookupV2 blockHash cAddr key =
    withUnaryCoreV2 (callV2 @"instanceStateLookup") msg ((fmap . fmap) fromProto)
  where
    msg =
        defMessage
            & ProtoFields.blockHash .~ toProto blockHash
            & ProtoFields.address .~ toProto cAddr
            & ProtoFields.key .~ key

-- |Stop dumping packets.
-- This feature is enabled if the node was built with the `network_dump` feature.
-- Returns a GRPC error if the network dump could not be stopped.
dumpStopV2 :: (MonadIO m) => ClientMonad m (GRPCResult ())
dumpStopV2 = withUnaryCoreV2 (callV2 @"dumpStop") defMessage ((fmap . fmap . const) ())

-- |Start dumping network packets into the specified file.
-- This feature is enabled if the node was built with the `network_dump` feature.
-- Returns a GRPC error if the network dump failed to start.
dumpStartV2 :: (MonadIO m) => Text -> Bool -> ClientMonad m (GRPCResult ())
dumpStartV2 file raw = withUnaryCoreV2 (callV2 @"dumpStart") msg ((fmap . fmap . const) ())
  where
    msg = defMessage & ProtoFields.file .~ file & ProtoFields.raw .~ raw

-- |Unban the given peer. Returns a GRPC error if the action failed.
unbanPeerV2 :: (MonadIO m) => Peer -> ClientMonad m (GRPCResult ())
unbanPeerV2 peer = withUnaryCoreV2 (callV2 @"unbanPeer") msg ((fmap . fmap . const) ())
  where
    msg = defMessage & ProtoFields.ipAddress .~ toProto peer

-- |Ban the given peer. Returns a GRPC error if the action failed.
banPeerV2 :: (MonadIO m) => Peer -> ClientMonad m (GRPCResult ())
banPeerV2 peer = withUnaryCoreV2 (callV2 @"banPeer") msg ((fmap . fmap . const) ())
  where
    msg = defMessage & ProtoFields.ipAddress .~ toProto peer

-- |Get a list of peers banned by the node.
getBannedPeersV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe [Peer]))
getBannedPeersV2 = withUnaryCoreV2 (callV2 @"getBannedPeers") defMessage ((fmap . fmap) fromProto)

-- |Ask the node to disconnect from the peer with the submitted details.
-- On success, the peer is removed from the peer-list of the node and a
-- @GRPCResponse@ is returned. Otherwise a GRPC error is returned.
peerDisconnectV2 :: (MonadIO m) => IpAddress -> IpPort -> ClientMonad m (GRPCResult ())
peerDisconnectV2 ip port = withUnaryCoreV2 (callV2 @"peerDisconnect") msg ((fmap . fmap . const) ())
  where
    msg = defMessage & ProtoFields.ip .~ toProto ip & ProtoFields.port .~ toProto port

-- |Ask a peer to connect to the peer with the submitted details.
-- On success, the peer is added in the peer-list of the node and a
-- @GRPCResponse@ is returned. Otherwise a GRPC error is returned.
-- Note that the peer may not be connected instantly.
peerConnectV2 :: (MonadIO m) => IpAddress -> IpPort -> ClientMonad m (GRPCResult ())
peerConnectV2 ip port = withUnaryCoreV2 (callV2 @"peerConnect") msg ((fmap . fmap . const) ())
  where
    msg = defMessage & ProtoFields.ip .~ toProto ip & ProtoFields.port .~ toProto port

-- |Shut down the node. Returns a GRPC error if the shutdown failed.
shutdownV2 :: (MonadIO m) => ClientMonad m (GRPCResult ())
shutdownV2 = withUnaryCoreV2 (callV2 @"shutdown") defMessage ((fmap . fmap . const) ()) -- FIXME: Consider wrapper which just ignores output for these.

-- |Get next available sequence numbers for updating chain parameters after a given block.
getNextUpdateSequenceNumbersV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.NextUpdateSequenceNumbers))
getNextUpdateSequenceNumbersV2 blockHash = withUnaryCoreV2 (callV2 @"getNextUpdateSequenceNumbers") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHash

-- |Get information related to the baker election for a particular block.
getElectionInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.BlockBirkParameters))
getElectionInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getElectionInfo") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHash

-- |Get the current branches of blocks starting from and including the last finalized block.
getBranchesV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe QueryTypes.Branch))
getBranchesV2 = withUnaryCoreV2 (callV2 @"getBranches") defMessage ((fmap . fmap) fromProto)

-- |Run the smart contract entrypoint in a given context and in the state at the end of the given block.
invokeInstanceV2 :: (MonadIO m) => InvokeInstanceInput -> ClientMonad m (GRPCResult (Maybe InvokeContract.InvokeContractResult))
invokeInstanceV2 iiInput = withUnaryCoreV2 (callV2 @"invokeInstance") msg ((fmap . fmap) fromProto)
  where
    msg = toProto iiInput

-- |Get information about tokenomics at the end of a given block.
getTokenomicsInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.RewardStatus))
getTokenomicsInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getTokenomicsInfo") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHash

-- |Get a list of live blocks at a given height.
getBlocksAtHeightV2 :: (MonadIO m) => BlockHeightInput -> ClientMonad m (GRPCResult (Maybe [BlockHash]))
getBlocksAtHeightV2 blockHeight = withUnaryCoreV2 (callV2 @"getBlocksAtHeight") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHeight

-- |Get information about the passive delegators at the end of a given block.
getPassiveDelegationInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.PoolStatus))
getPassiveDelegationInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getPassiveDelegationInfo") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHash

-- |Get information about a given pool at the end of a given block.
getPoolInfoV2 :: (MonadIO m) => BlockHashInput -> BakerId -> ClientMonad m (GRPCResult (Maybe QueryTypes.PoolStatus))
getPoolInfoV2 blockHash baker = withUnaryCoreV2 (callV2 @"getPoolInfo") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.baker .~ toProto baker

-- |Get information, such as height, timings, and transaction counts for the given block.
getBlockInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.BlockInfo))
getBlockInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getBlockInfo") msg ((fmap . fmap) fromProto)
  where
    msg = toProto blockHash

-- |Get information about the current state of consensus.
getConsensusInfoV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe Wasm.WasmModule))
getConsensusInfoV2 = withUnaryCoreV2 (callV2 @"getModuleSource") defMessage ((fmap . fmap) fromProto)

-- |Get the source of a smart contract module.
getModuleSourceV2 :: (MonadIO m) => ModuleRef -> BlockHashInput -> ClientMonad m (GRPCResult (Maybe Wasm.WasmModule))
getModuleSourceV2 modRef hash = withUnaryCoreV2 (callV2 @"getModuleSource") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto hash & ProtoFields.moduleRef .~ toProto modRef

-- |Retrieve the account information from the chain.
getAccountInfoV2 ::
    MonadIO m =>
    -- |Account identifier, address, index or credential registration id.
    AccountIdentifierInput ->
    -- |Block hash
    BlockHashInput ->
    ClientMonad m (GRPCResult (Maybe Concordium.Types.AccountInfo))
getAccountInfoV2 account blockHash = withUnaryCoreV2 (callV2 @"getAccountInfo") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.accountIdentifier .~ toProto account

getInstanceInfoV2 :: (MonadIO m) => ContractAddress -> BlockHashInput -> ClientMonad m (GRPCResult (Maybe Wasm.InstanceInfo))
getInstanceInfoV2 cAddress blockHash = withUnaryCoreV2 (callV2 @"getInstanceInfo") msg ((fmap . fmap) fromProto)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.address .~ toProto cAddress

getNextSequenceNumberV2 :: (MonadIO m) => AccountAddress -> ClientMonad m (GRPCResult (Maybe QueryTypes.NextAccountNonce))
getNextSequenceNumberV2 accAddress = withUnaryCoreV2 (callV2 @"getNextAccountSequenceNumber") msg ((fmap . fmap) fromProto)
  where
    msg = toProto accAddress

-- |Setup the GRPC client and run a rawUnary call with the provided message to the provided method,
-- the output is interpreted using the function given in the third parameter.
withUnaryCoreV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    MethodInput CS.Queries m ->
    (GRPCResult (MethodOutput CS.Queries m) -> b) ->
    ClientMonad n b
withUnaryCoreV2 method message k = do
    clientRef <- asks grpc
    cfg <- asks config
    lock <- asks rwlock
    logm <- asks logger
    cookies <- ClientMonad (lift get)
    mv <- asks killConnection
    let Timeout timeoutSeconds = _grpcClientConfigTimeout cfg

    -- try to establish a connection
    let tryEstablish :: Int -> IO (Maybe GrpcClient)
        tryEstablish n = do
            logm $ "Trying to establish connection, n = " <> pack (show n)
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
                        return (0, Nothing)
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
                                        runExceptT (rawUnary method client' message)
                                            >>= \case
                                                Left err -> Nothing <$ logm ("Network error: " <> fromString (show err)) -- client error
                                                Right (Left err) -> Nothing <$ logm ("Too much concurrency: " <> fromString (show err))
                                                Right (Right x) -> return (Just x)
                                race (race (readMVar mv) (threadDelay (timeoutSeconds * 1000000))) runRPC
                                    >>= \case
                                        Left (Left ()) -> (gen, Nothing) <$ logm "Terminating query because GOAWAY received."
                                        Left (Right ()) -> (gen, Nothing) <$ logm "Terminating query because it timed out."
                                        Right x -> return (gen, x)
                            Just () -> return (gen, Nothing) -- fail this round, go again after the client is established.
    ret <- liftIO tryRun

    case ret of
        (usedGen, Nothing) -> do
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
                    let response = outputGRPC ret'
                    addHeaders response
                    return (k response)
                else return (k (Left "Cannot establish connection to GRPC endpoint."))
        (_, Just v) ->
            let response = outputGRPC' v
            in  do
                    addHeaders response
                    return (k response)
  where
    addHeaders response = case response of
        Right GRPCResponse{..} -> do
            ClientMonad $ do
                forM_ grpcHeaders $ \(hn, hv) ->
                    when (hn == "set-cookie") $
                        let c = Cookie.parseSetCookie hv
                        in  modify' (Map.insert (Cookie.setCookieName c) (Cookie.setCookieValue c))
        Left _ -> return ()

-- |Setup the GRPC client and run a rawUnary call to the provided method.
withUnaryCoreNoMsgV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    (GRPCResult (MethodOutput CS.Queries m) -> (CIHeaderList, b)) ->
    ClientMonad n (CIHeaderList, b)
withUnaryCoreNoMsgV2 method = withUnaryCoreV2 method defMessage

-- |Call a method with a given message and use a Getter on the output.
withUnaryV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    MethodInput CS.Queries m ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryV2 method message k = withUnaryCoreV2 method message (\x -> (^. k) <$> x)

-- |Call a method with a given message using the `id` lens on the output.
withUnaryV2' ::
    forall m n.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    MethodInput CS.Queries m ->
    ClientMonad n (GRPCResult (MethodOutput CS.Queries m))
withUnaryV2' method message = withUnaryV2 method message (to id)

-- |Call a method without a message using the given lens
withUnaryNoMsgV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryNoMsgV2 method = withUnaryV2 method defMessage

-- |Call a method with a message that has `blockHash` as a field and use the given lens on the output
withUnaryBlockV2 ::
    forall m n b.
    ( HasMethod CS.Queries m,
      MonadIO n,
      Field.HasField (MethodInput CS.Queries m) "blockHash" Text
    ) =>
    RPC CS.Queries m ->
    Text ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryBlockV2 method hash = withUnaryV2 method (defMessage & ProtoFields.blockHash .~ hash)

-- |Call a method with an empty message using the `id` lens on the output.
withUnaryNoMsgV2' ::
    forall m n.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    ClientMonad n (GRPCResult (MethodOutput CS.Queries m))
withUnaryNoMsgV2' method = withUnaryV2' method defMessage

callV2 :: forall m. RPC CS.Queries m
callV2 = RPC @CS.Queries @m