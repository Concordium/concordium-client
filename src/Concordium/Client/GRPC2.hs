{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |Part of the implementation of the GRPC2 interface. This module constructs
    responses to queries that are handled by the Haskell part of the code.

   This module only provides foreign exports, and should not be imported from
   other Haskell code.
-}
module Concordium.Client.GRPC2 (
    ToProto (..),
    FromProto (..),
    getAccountInfoV2,
) where

-- Refactor these after migrating to GRPC V2; here for now due to many namespace conflicts.
import Control.Concurrent
import Data.Bits (shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.ByteString.Unsafe qualified as BS
import Data.Coerce
import Data.FixedByteString qualified as FBS
import Data.Int
import Data.Map.Strict qualified as Map
import Data.ProtoLens qualified as Proto
import Data.ProtoLens.Combinators qualified as Proto
import Data.ProtoLens.Field qualified
import Data.Ratio qualified as Ratio
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Data.Word
import Foreign hiding (shiftL, shiftR)
import Lens.Micro.Platform

import Proto.V2.Concordium.Types qualified as Proto
import Proto.V2.Concordium.Types_Fields qualified as ProtoFields

import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Queries qualified as QueryTypes
import Concordium.Types.Transactions qualified as Transactions
import Concordium.Types.Transactions qualified as TxTypes

import Concordium.Common.Time
import Concordium.Common.Version
import Concordium.Crypto.SHA256 (DigestSize, Hash (Hash))
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
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)

import Concordium.Client.GRPC
import Concordium.Client.Runner.Helper
import Control.Exception
import Network.GRPC.Client
import Network.GRPC.Client.Helpers hiding (Address)
import Network.GRPC.HTTP2.ProtoLens
import Network.HTTP2.Client
import Web.Cookie qualified as Cookie

import Concordium.Types.Accounts qualified as Concordium.Types
import Concordium.Types.Parameters (CryptographicParameters)
import Concordium.Types.Queries (NextAccountNonce)
import Concordium.Wasm (ModuleSource)
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (fromRight)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe, catMaybes, mapMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding.Bytes qualified as S
import Data.ProtoLens.Field qualified as Field
import Data.ProtoLens.Service.Types
import Data.Sequence (Seq (Empty))
import Data.String
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Proto.V2.Concordium.Service qualified as CS
import qualified Control.Monad.Cont as Wasm
import Proto.V2.Concordium.Types_Fields (schedule)
import qualified Proto.V2.Concordium.Types_Fields as Proto

{- |A helper function that can be used to construct a value of a protobuf
 "wrapper" type by serializing the provided value @a@ using its serialize
 instance.

 More concretely, the wrapper type should be of the form

 > message Wrapper {
 >    bytes value = 1
 > }

 where the name @Wrapper@ can be arbitrary, but the @value@ field must exist,
 and it must have type @bytes@.
-}
mkSerialize ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        BS.ByteString
    , S.Serialize a
    ) =>
    a ->
    b
mkSerialize ek = Proto.make (ProtoFields.value .= S.encode ek)

{- |Like 'mkSerialize' above, but used to set a wrapper type whose @value@ field
 has type @uint64@. The supplied value must be coercible to a 'Word64'.
 Coercible here means that the value is a newtype wrapper (possibly repeated)
 of a Word64.
-}
mkWord64 ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word64
    , Coercible a Word64
    ) =>
    a ->
    b
mkWord64 a = Proto.make (ProtoFields.value .= coerce a)

-- |Like 'mkWord64', but for 32-bit integers instead of 64.
mkWord32 ::
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    , Coercible a Word32
    ) =>
    a ->
    b
mkWord32 a = Proto.make (ProtoFields.value .= coerce a)

{- |Like 'mkWord32', but the supplied value must be coercible to
 'Word16'.
-}
mkWord16 ::
    forall a b.
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    , Coercible a Word16
    ) =>
    a ->
    b
mkWord16 a = Proto.make (ProtoFields.value .= (fromIntegral (coerce a :: Word16) :: Word32))

{- |Like 'mkWord32', but the supplied value must be coercible to
 'Word8'.
-}
mkWord8 ::
    forall a b.
    ( Proto.Message b
    , Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    , Coercible a Word8
    ) =>
    a ->
    b
mkWord8 a = Proto.make (ProtoFields.value .= (fromIntegral (coerce a :: Word8) :: Word32))

{- |A helper function that serves as an inverse to `mkSerialize`,

 Converts a protocol buffer wrapper type to a native Haskell type
 which is a member of `Serialize`.

 More concretely, the wrapper type should be of the form

 > message Wrapper {
 >    bytes value = 1
 > }

 where the name @Wrapper@ can be arbitrary, but the @value@ field must exist,
 and it must have type @bytes@.
-}
deMkSerialize ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        BS.ByteString
    , S.Serialize a
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
    Word16
deMkWord16 val = fromIntegral $ shiftR (shiftL (val ^. ProtoFields.value) 16) 16

-- |Like 'mkWord32', but for Word8 fields instead.
deMkWord8 ::
    ( Data.ProtoLens.Field.HasField
        b
        "value"
        Word32
    ) =>
    b ->
    Word8
deMkWord8 val = fromIntegral $ shiftR (shiftL (val ^. ProtoFields.value) 24) 24

{- |A helper class analogous to something like Aeson's FromJSON.
 It exists to make it more manageable to convert the Protobuf types to
 their internal Haskell type equivalents.
-}
class FromProto a where
    -- |The corresponding Haskell type.
    type Output' a

    -- |A conversion function from the protobuf type to its Haskell equivalent.
    fromProto :: a -> Maybe (Output' a)

fromProtoError :: Maybe a
fromProtoError = Nothing

{- |A helper class analogous to something like Aeson's ToJSON.
 It exists to make it more manageable to convert the internal Haskell types to
 their Protobuf equivalents.
-}
class ToProto a where
    -- |The corresponding Proto type.
    type Output a

    -- |A conversion function from the type to its protobuf equivalent.
    toProto :: a -> Output a

instance ToProto Amount where
    type Output Amount = Proto.Amount
    toProto = mkWord64

instance ToProto BlockHash where
    type Output BlockHash = Proto.BlockHash
    toProto = mkSerialize

instance ToProto Hash where
    type Output Hash = Proto.Sha256Hash
    toProto = mkSerialize

instance ToProto TransactionHashV0 where
    type Output TransactionHashV0 = Proto.TransactionHash
    toProto = mkSerialize

instance ToProto ModuleRef where
    type Output ModuleRef = Proto.ModuleRef
    toProto = mkSerialize

instance ToProto Wasm.WasmModule where
    type Output Wasm.WasmModule = Proto.VersionedModuleSource
    toProto (Wasm.WasmModuleV0 modul) =
        Proto.make
            ( ProtoFields.v0
                .= Proto.make (ProtoFields.value .= Wasm.moduleSource (Wasm.wmvSource modul))
            )
    toProto (Wasm.WasmModuleV1 modul) =
        Proto.make
            ( ProtoFields.v1
                .= Proto.make (ProtoFields.value .= Wasm.moduleSource (Wasm.wmvSource modul))
            )

instance ToProto Wasm.InstanceInfo where
    type Output Wasm.InstanceInfo = Proto.InstanceInfo
    toProto Wasm.InstanceInfoV0{..} =
        Proto.make
            ( ProtoFields.v0
                .= Proto.make
                    ( do
                        ProtoFields.owner .= mkSerialize iiOwner
                        ProtoFields.amount .= mkWord64 iiAmount
                        ProtoFields.methods .= (toProto <$> Set.toList iiMethods)
                        ProtoFields.name .= toProto iiName
                        ProtoFields.sourceModule .= toProto iiSourceModule
                        ProtoFields.model .= toProto iiModel
                    )
            )
    toProto Wasm.InstanceInfoV1{..} =
        Proto.make
            ( ProtoFields.v1
                .= Proto.make
                    ( do
                        ProtoFields.owner .= mkSerialize iiOwner
                        ProtoFields.amount .= mkWord64 iiAmount
                        ProtoFields.methods .= (toProto <$> Set.toList iiMethods)
                        ProtoFields.name .= toProto iiName
                        ProtoFields.sourceModule .= toProto iiSourceModule
                    )
            )

instance ToProto Wasm.ReceiveName where
    type Output Wasm.ReceiveName = Proto.ReceiveName
    toProto name = Proto.make $ ProtoFields.value .= Wasm.receiveName name

instance ToProto Wasm.InitName where
    type Output Wasm.InitName = Proto.InitName
    toProto name = Proto.make $ ProtoFields.value .= Wasm.initName name

instance ToProto Wasm.ContractState where
    type Output Wasm.ContractState = Proto.ContractStateV0
    toProto Wasm.ContractState{..} = Proto.make $ ProtoFields.value .= contractState

instance ToProto ContractAddress where
    type Output ContractAddress = Proto.ContractAddress
    toProto ContractAddress{..} = Proto.make $ do
        ProtoFields.index .= _contractIndex contractIndex
        ProtoFields.subindex .= _contractSubindex contractSubindex

instance ToProto BlockHeight where
    type Output BlockHeight = Proto.BlockHeight
    toProto = mkWord64

instance ToProto AbsoluteBlockHeight where
    type Output AbsoluteBlockHeight = Proto.AbsoluteBlockHeight
    toProto = mkWord64

instance ToProto AccountAddress where
    type Output AccountAddress = Proto.AccountAddress
    toProto = mkSerialize

instance ToProto Nonce where
    type Output Nonce = Proto.SequenceNumber
    toProto = mkWord64

instance ToProto UTCTime where
    type Output UTCTime = Proto.Timestamp
    toProto time = mkWord64 $ utcTimeToTimestamp time

instance ToProto Duration where
    type Output Duration = Proto.Duration
    toProto = mkWord64

instance ToProto GenesisIndex where
    type Output GenesisIndex = Proto.GenesisIndex
    toProto = mkWord32

instance ToProto ProtocolVersion where
    type Output ProtocolVersion = Proto.ProtocolVersion
    toProto P1 = Proto.PROTOCOL_VERSION_1
    toProto P2 = Proto.PROTOCOL_VERSION_2
    toProto P3 = Proto.PROTOCOL_VERSION_3
    toProto P4 = Proto.PROTOCOL_VERSION_4
    toProto P5 = Proto.PROTOCOL_VERSION_5
    toProto P6 = Proto.PROTOCOL_VERSION_6

instance ToProto QueryTypes.NextAccountNonce where
    type Output QueryTypes.NextAccountNonce = Proto.NextAccountSequenceNumber
    toProto QueryTypes.NextAccountNonce{..} = Proto.make $ do
        ProtoFields.sequenceNumber .= toProto nanNonce
        ProtoFields.allFinal .= nanAllFinal

instance ToProto QueryTypes.ConsensusStatus where
    type Output QueryTypes.ConsensusStatus = Proto.ConsensusInfo
    toProto QueryTypes.ConsensusStatus{..} = Proto.make $ do
        ProtoFields.bestBlock .= toProto csBestBlock
        ProtoFields.genesisBlock .= toProto csGenesisBlock
        ProtoFields.genesisTime .= toProto csGenesisTime
        ProtoFields.slotDuration .= toProto csSlotDuration
        ProtoFields.epochDuration .= toProto csEpochDuration
        ProtoFields.lastFinalizedBlock .= toProto csLastFinalizedBlock
        ProtoFields.bestBlockHeight .= toProto csBestBlockHeight
        ProtoFields.lastFinalizedBlockHeight .= toProto csLastFinalizedBlockHeight
        ProtoFields.blocksReceivedCount .= fromIntegral csBlocksReceivedCount
        ProtoFields.maybe'blockLastReceivedTime .= fmap toProto csBlockLastReceivedTime
        ProtoFields.blockReceiveLatencyEma .= csBlockReceiveLatencyEMA
        ProtoFields.blockReceiveLatencyEmsd .= csBlockReceiveLatencyEMSD
        ProtoFields.maybe'blockReceivePeriodEma .= csBlockReceivePeriodEMA
        ProtoFields.maybe'blockReceivePeriodEmsd .= csBlockReceivePeriodEMSD
        ProtoFields.blocksVerifiedCount .= fromIntegral csBlocksVerifiedCount
        ProtoFields.maybe'blockLastArrivedTime .= fmap toProto csBlockLastArrivedTime
        ProtoFields.blockArriveLatencyEma .= csBlockArriveLatencyEMA
        ProtoFields.blockArriveLatencyEmsd .= csBlockArriveLatencyEMSD
        ProtoFields.maybe'blockArrivePeriodEma .= csBlockArrivePeriodEMA
        ProtoFields.maybe'blockArrivePeriodEmsd .= csBlockArrivePeriodEMSD
        ProtoFields.transactionsPerBlockEma .= csTransactionsPerBlockEMA
        ProtoFields.transactionsPerBlockEmsd .= csTransactionsPerBlockEMSD
        ProtoFields.finalizationCount .= fromIntegral csFinalizationCount
        ProtoFields.maybe'lastFinalizedTime .= fmap toProto csLastFinalizedTime
        ProtoFields.maybe'finalizationPeriodEma .= csFinalizationPeriodEMA
        ProtoFields.maybe'finalizationPeriodEmsd .= csFinalizationPeriodEMSD
        ProtoFields.protocolVersion .= toProto csProtocolVersion
        ProtoFields.genesisIndex .= toProto csGenesisIndex
        ProtoFields.currentEraGenesisBlock .= toProto csCurrentEraGenesisBlock
        ProtoFields.currentEraGenesisTime .= toProto csCurrentEraGenesisTime

instance ToProto AccountThreshold where
    type Output AccountThreshold = Proto.AccountThreshold
    toProto = mkWord8

instance ToProto SignatureThreshold where
    type Output SignatureThreshold = Proto.SignatureThreshold
    toProto = mkWord8

instance ToProto Threshold where
    type Output Threshold = Proto.ArThreshold
    toProto = mkWord8

instance ToProto AccountIndex where
    type Output AccountIndex = Proto.AccountIndex
    toProto = mkWord64

instance ToProto BakerId where
    type Output BakerId = Proto.BakerId
    toProto = mkWord64

instance ToProto DelegatorId where
    type Output DelegatorId = Proto.DelegatorId
    toProto v = Proto.make $ ProtoFields.id .= toProto (delegatorAccountIndex v)

instance ToProto EncryptedAmount where
    type Output EncryptedAmount = Proto.EncryptedAmount
    toProto = mkSerialize

instance ToProto AccountEncryptedAmount where
    type Output AccountEncryptedAmount = Proto.EncryptedBalance
    toProto encBal =
        case _aggregatedAmount encBal of
            Nothing -> Proto.make mkEncryptedBalance
            Just (aggAmount, numAgg) -> Proto.make $ do
                mkEncryptedBalance
                ProtoFields.aggregatedAmount .= toProto aggAmount
                ProtoFields.numAggregated .= numAgg
      where
        mkEncryptedBalance = do
            ProtoFields.selfAmount .= toProto (_selfAmount encBal)
            ProtoFields.startIndex .= coerce (_startIndex encBal)

instance ToProto AccountReleaseSummary where
    type Output AccountReleaseSummary = Proto.ReleaseSchedule
    toProto ars = Proto.make $ do
        ProtoFields.total .= toProto (releaseTotal ars)
        ProtoFields.schedules .= (toProto <$> releaseSchedule ars)

instance ToProto ScheduledRelease where
    type Output ScheduledRelease = Proto.Release
    toProto r = Proto.make $ do
        ProtoFields.timestamp .= mkWord64 (releaseTimestamp r)
        ProtoFields.amount .= toProto (releaseAmount r)
        ProtoFields.transactions .= (toProto <$> releaseTransactions r)

instance ToProto (Timestamp, Amount) where
    type Output (Timestamp, Amount) = Proto.NewRelease
    toProto (t, a) = Proto.make $ do
        ProtoFields.timestamp .= toProto t
        ProtoFields.amount .= toProto a

instance ToProto Timestamp where
    type Output Timestamp = Proto.Timestamp
    toProto timestamp = mkWord64 $ tsMillis timestamp

instance ToProto (StakePendingChange' UTCTime) where
    type Output (StakePendingChange' UTCTime) = Maybe Proto.StakePendingChange
    toProto NoChange = Nothing
    toProto (ReduceStake newStake effectiveTime) =
        Just . Proto.make $
            ( ProtoFields.reduce
                .= Proto.make
                    ( do
                        ProtoFields.newStake .= toProto newStake
                        ProtoFields.effectiveTime .= toProto effectiveTime
                    )
            )
    toProto (RemoveStake effectiveTime) =
        Just . Proto.make $ (ProtoFields.remove .= toProto effectiveTime)

instance ToProto (StakePendingChange' Timestamp) where
    type Output (StakePendingChange' Timestamp) = Maybe Proto.StakePendingChange
    toProto NoChange = Nothing
    toProto (ReduceStake newStake effectiveTime) =
        Just $
            Proto.make
                ( ProtoFields.reduce
                    .= Proto.make
                        ( do
                            ProtoFields.newStake .= toProto newStake
                            ProtoFields.effectiveTime .= toProto effectiveTime
                        )
                )
    toProto (RemoveStake effectiveTime) =
        Just $ Proto.make (ProtoFields.remove .= toProto effectiveTime)

instance ToProto BakerInfo where
    type Output BakerInfo = Proto.BakerInfo
    toProto BakerInfo{..} =
        Proto.make
            ( do
                ProtoFields.bakerId .= mkWord64 _bakerIdentity
                ProtoFields.electionKey .= mkSerialize _bakerElectionVerifyKey
                ProtoFields.signatureKey .= mkSerialize _bakerSignatureVerifyKey
                ProtoFields.aggregationKey .= mkSerialize _bakerAggregationVerifyKey
            )

instance ToProto OpenStatus where
    type Output OpenStatus = Proto.OpenStatus
    toProto OpenForAll = Proto.OPEN_STATUS_OPEN_FOR_ALL
    toProto ClosedForNew = Proto.OPEN_STATUS_CLOSED_FOR_NEW
    toProto ClosedForAll = Proto.OPEN_STATUS_CLOSED_FOR_ALL

instance ToProto UrlText where
    type Output UrlText = Text
    toProto (UrlText s) = s

instance ToProto AmountFraction where
    type Output AmountFraction = Proto.AmountFraction
    toProto (AmountFraction ppht) = Proto.make (ProtoFields.partsPerHundredThousand .= fromIntegral ppht)

instance ToProto ElectionDifficulty where
    type Output ElectionDifficulty = Proto.ElectionDifficulty
    toProto (ElectionDifficulty ppht) = Proto.make $ ProtoFields.value . ProtoFields.partsPerHundredThousand .= fromIntegral ppht

instance ToProto CommissionRates where
    type Output CommissionRates = Proto.CommissionRates
    toProto CommissionRates{..} = Proto.make $ do
        ProtoFields.finalization .= toProto _finalizationCommission
        ProtoFields.baking .= toProto _bakingCommission
        ProtoFields.transaction .= toProto _transactionCommission

instance ToProto BakerPoolInfo where
    type Output BakerPoolInfo = Proto.BakerPoolInfo
    toProto BakerPoolInfo{..} = Proto.make $ do
        ProtoFields.openStatus .= toProto _poolOpenStatus
        ProtoFields.url .= toProto _poolMetadataUrl
        ProtoFields.commissionRates .= toProto _poolCommissionRates

instance ToProto AccountStakingInfo where
    type Output AccountStakingInfo = Maybe Proto.AccountStakingInfo
    toProto AccountStakingNone = Nothing
    toProto AccountStakingBaker{..} =
        Just . Proto.make $
            ( do
                ProtoFields.baker
                    .= Proto.make
                        ( do
                            ProtoFields.stakedAmount .= toProto asiStakedAmount
                            ProtoFields.restakeEarnings .= asiStakeEarnings
                            ProtoFields.bakerInfo .= toProto asiBakerInfo
                            ProtoFields.maybe'pendingChange .= toProto asiPendingChange
                            case asiPoolInfo of
                                Nothing -> return ()
                                Just asipi -> ProtoFields.poolInfo .= toProto asipi
                        )
            )
    toProto AccountStakingDelegated{..} =
        Just . Proto.make $
            ( do
                ProtoFields.delegator
                    .= Proto.make
                        ( do
                            ProtoFields.stakedAmount .= mkWord64 asiStakedAmount
                            ProtoFields.restakeEarnings .= asiStakeEarnings
                            ProtoFields.target .= toProto asiDelegationTarget
                            ProtoFields.maybe'pendingChange .= toProto asiDelegationPendingChange
                        )
            )

instance ToProto DelegationTarget where
    type Output DelegationTarget = Proto.DelegationTarget
    toProto DelegatePassive = Proto.make $ ProtoFields.passive .= Proto.defMessage
    toProto (DelegateToBaker bi) = Proto.make $ ProtoFields.baker .= toProto bi

instance ToProto (Map.Map CredentialIndex (Versioned RawAccountCredential)) where
    type Output (Map.Map CredentialIndex (Versioned RawAccountCredential)) = Map.Map Word32 Proto.AccountCredential
    toProto = Map.fromAscList . map (\(k, v) -> (fromIntegral k, toProto (vValue v))) . Map.toAscList

instance ToProto CredentialPublicKeys where
    type Output CredentialPublicKeys = Proto.CredentialPublicKeys
    toProto CredentialPublicKeys{..} = Proto.make $ do
        ProtoFields.threshold .= mkWord8 credThreshold
        ProtoFields.keys .= (Map.fromAscList . map convertKey . Map.toAscList $ credKeys)
      where
        convertKey (ki, VerifyKeyEd25519 key) = (fromIntegral ki, Proto.make $ ProtoFields.ed25519Key .= S.encode key)

instance ToProto Policy where
    type Output Policy = Proto.Policy
    toProto Policy{..} = Proto.make $ do
        ProtoFields.createdAt .= toProto pCreatedAt
        ProtoFields.validTo .= toProto pValidTo
        ProtoFields.attributes .= mkAttributes pItems
      where
        mkAttributes =
            Map.fromAscList
                . map (\(AttributeTag tag, value) -> (fromIntegral tag, S.runPut (S.putShortByteString (coerce value))))
                . Map.toAscList

instance ToProto YearMonth where
    type Output YearMonth = Proto.YearMonth
    toProto YearMonth{..} = Proto.make $ do
        ProtoFields.year .= fromIntegral ymYear
        ProtoFields.month .= fromIntegral ymMonth

instance ToProto RawCredentialRegistrationID where
    type Output RawCredentialRegistrationID = Proto.CredentialRegistrationId
    toProto = mkSerialize

instance ToProto CredentialRegistrationID where
    type Output CredentialRegistrationID = Proto.CredentialRegistrationId
    toProto = toProto . toRawCredRegId

instance ToProto IdentityProviderIdentity where
    type Output IdentityProviderIdentity = Proto.IdentityProviderIdentity
    toProto = mkWord32

instance ToProto Commitment where
    type Output Commitment = Proto.Commitment
    toProto = mkSerialize

instance ToProto CredentialDeploymentCommitments where
    type Output CredentialDeploymentCommitments = Proto.CredentialCommitments
    toProto CredentialDeploymentCommitments{..} = Proto.make $ do
        ProtoFields.prf .= toProto cmmPrf
        ProtoFields.credCounter .= toProto cmmCredCounter
        ProtoFields.maxAccounts .= toProto cmmMaxAccounts
        ProtoFields.attributes
            .= ( Map.fromAscList
                    . map (\(AttributeTag tag, v) -> (fromIntegral tag :: Word32, toProto v))
                    . Map.toAscList
               )
                cmmAttributes
        ProtoFields.idCredSecSharingCoeff .= map toProto cmmIdCredSecSharingCoeff

instance ToProto (Map.Map ArIdentity ChainArData) where
    type Output (Map.Map ArIdentity ChainArData) = Map.Map Word32 Proto.ChainArData
    toProto = Map.fromAscList . map (\(k, v) -> (coerce k, dataToProto v)) . Map.toAscList
      where
        dataToProto d = Proto.make (ProtoFields.encIdCredPubShare .= S.encode d)

instance ToProto RawAccountCredential where
    type Output RawAccountCredential = Proto.AccountCredential
    toProto (InitialAC InitialCredentialDeploymentValues{..}) =
        Proto.make $
            ProtoFields.initial
                .= Proto.make
                    ( do
                        ProtoFields.keys .= toProto icdvAccount
                        ProtoFields.credId .= toProto icdvRegId
                        ProtoFields.ipId .= toProto icdvIpId
                        ProtoFields.policy .= toProto icdvPolicy
                    )
    toProto (NormalAC CredentialDeploymentValues{..} commitments) =
        Proto.make $
            ProtoFields.normal
                .= Proto.make
                    ( do
                        ProtoFields.keys .= toProto cdvPublicKeys
                        ProtoFields.credId .= toProto cdvCredId
                        ProtoFields.ipId .= toProto cdvIpId
                        ProtoFields.policy .= toProto cdvPolicy
                        ProtoFields.arThreshold .= toProto cdvThreshold
                        ProtoFields.arData .= toProto cdvArData
                        ProtoFields.commitments .= toProto commitments
                    )

instance ToProto AccountEncryptionKey where
    type Output AccountEncryptionKey = Proto.EncryptionKey
    toProto = mkSerialize

instance ToProto AccountInfo where
    type Output AccountInfo = Proto.AccountInfo
    toProto AccountInfo{..} = Proto.make $ do
        ProtoFields.sequenceNumber .= toProto aiAccountNonce
        ProtoFields.amount .= toProto aiAccountAmount
        ProtoFields.schedule .= toProto aiAccountReleaseSchedule
        ProtoFields.creds .= toProto aiAccountCredentials
        ProtoFields.threshold .= toProto aiAccountThreshold
        ProtoFields.encryptedBalance .= toProto aiAccountEncryptedAmount
        ProtoFields.encryptionKey .= toProto aiAccountEncryptionKey
        ProtoFields.index .= toProto aiAccountIndex
        ProtoFields.address .= toProto aiAccountAddress
        ProtoFields.maybe'stake .= toProto aiStakingInfo

instance ToProto Wasm.Parameter where
    type Output Wasm.Parameter = Proto.Parameter
    toProto Wasm.Parameter{..} = Proto.make $ ProtoFields.value .= BSS.fromShort parameter

instance ToProto RejectReason where
    type Output RejectReason = Proto.RejectReason
    toProto r = case r of
        ModuleNotWF -> Proto.make $ ProtoFields.moduleNotWf .= Proto.defMessage
        ModuleHashAlreadyExists moduleRef -> Proto.make $ ProtoFields.moduleHashAlreadyExists .= toProto moduleRef
        InvalidAccountReference addr -> Proto.make $ ProtoFields.invalidAccountReference .= toProto addr
        InvalidInitMethod moduleRef initName ->
            Proto.make $
                ProtoFields.invalidInitMethod
                    .= Proto.make
                        ( do
                            ProtoFields.moduleRef .= toProto moduleRef
                            ProtoFields.initName .= toProto initName
                        )
        InvalidReceiveMethod moduleRef receiveName ->
            Proto.make $
                ProtoFields.invalidReceiveMethod
                    .= Proto.make
                        ( do
                            ProtoFields.moduleRef .= toProto moduleRef
                            ProtoFields.receiveName .= toProto receiveName
                        )
        InvalidModuleReference moduleRef -> Proto.make $ ProtoFields.invalidModuleReference .= toProto moduleRef
        InvalidContractAddress addr -> Proto.make $ ProtoFields.invalidContractAddress .= toProto addr
        RuntimeFailure -> Proto.make $ ProtoFields.runtimeFailure .= Proto.defMessage
        AmountTooLarge addr amount ->
            Proto.make $
                ProtoFields.amountTooLarge
                    .= Proto.make
                        ( do
                            ProtoFields.address .= toProto addr
                            ProtoFields.amount .= toProto amount
                        )
        SerializationFailure -> Proto.make $ ProtoFields.serializationFailure .= Proto.defMessage
        OutOfEnergy -> Proto.make $ ProtoFields.outOfEnergy .= Proto.defMessage
        RejectedInit{..} -> Proto.make $ ProtoFields.rejectedInit . ProtoFields.rejectReason .= rejectReason
        RejectedReceive{..} ->
            Proto.make $
                ProtoFields.rejectedReceive
                    .= Proto.make
                        ( do
                            ProtoFields.rejectReason .= rejectReason
                            ProtoFields.contractAddress .= toProto contractAddress
                            ProtoFields.receiveName .= toProto receiveName
                            ProtoFields.parameter .= toProto parameter
                        )
        InvalidProof -> Proto.make $ ProtoFields.invalidProof .= Proto.defMessage
        AlreadyABaker bakerId -> Proto.make $ ProtoFields.alreadyABaker .= toProto bakerId
        NotABaker addr -> Proto.make $ ProtoFields.notABaker .= toProto addr
        InsufficientBalanceForBakerStake -> Proto.make $ ProtoFields.insufficientBalanceForBakerStake .= Proto.defMessage
        StakeUnderMinimumThresholdForBaking -> Proto.make $ ProtoFields.stakeUnderMinimumThresholdForBaking .= Proto.defMessage
        BakerInCooldown -> Proto.make $ ProtoFields.bakerInCooldown .= Proto.defMessage
        DuplicateAggregationKey k -> Proto.make $ ProtoFields.duplicateAggregationKey .= mkSerialize k
        NonExistentCredentialID -> Proto.make $ ProtoFields.nonExistentCredentialId .= Proto.defMessage
        KeyIndexAlreadyInUse -> Proto.make $ ProtoFields.keyIndexAlreadyInUse .= Proto.defMessage
        InvalidAccountThreshold -> Proto.make $ ProtoFields.invalidAccountThreshold .= Proto.defMessage
        InvalidCredentialKeySignThreshold -> Proto.make $ ProtoFields.invalidCredentialKeySignThreshold .= Proto.defMessage
        InvalidEncryptedAmountTransferProof -> Proto.make $ ProtoFields.invalidEncryptedAmountTransferProof .= Proto.defMessage
        InvalidTransferToPublicProof -> Proto.make $ ProtoFields.invalidTransferToPublicProof .= Proto.defMessage
        EncryptedAmountSelfTransfer addr -> Proto.make $ ProtoFields.encryptedAmountSelfTransfer .= toProto addr
        InvalidIndexOnEncryptedTransfer -> Proto.make $ ProtoFields.invalidIndexOnEncryptedTransfer .= Proto.defMessage
        ZeroScheduledAmount -> Proto.make $ ProtoFields.zeroScheduledAmount .= Proto.defMessage
        NonIncreasingSchedule -> Proto.make $ ProtoFields.nonIncreasingSchedule .= Proto.defMessage
        FirstScheduledReleaseExpired -> Proto.make $ ProtoFields.firstScheduledReleaseExpired .= Proto.defMessage
        ScheduledSelfTransfer addr -> Proto.make $ ProtoFields.scheduledSelfTransfer .= toProto addr
        InvalidCredentials -> Proto.make $ ProtoFields.invalidCredentials .= Proto.defMessage
        DuplicateCredIDs ids -> Proto.make $ ProtoFields.duplicateCredIds . ProtoFields.ids .= (toProto <$> ids)
        NonExistentCredIDs ids -> Proto.make $ ProtoFields.nonExistentCredIds . ProtoFields.ids .= (toProto <$> ids)
        RemoveFirstCredential -> Proto.make $ ProtoFields.removeFirstCredential .= Proto.defMessage
        CredentialHolderDidNotSign -> Proto.make $ ProtoFields.credentialHolderDidNotSign .= Proto.defMessage
        NotAllowedMultipleCredentials -> Proto.make $ ProtoFields.notAllowedMultipleCredentials .= Proto.defMessage
        NotAllowedToReceiveEncrypted -> Proto.make $ ProtoFields.notAllowedToReceiveEncrypted .= Proto.defMessage
        NotAllowedToHandleEncrypted -> Proto.make $ ProtoFields.notAllowedToHandleEncrypted .= Proto.defMessage
        MissingBakerAddParameters -> Proto.make $ ProtoFields.missingBakerAddParameters .= Proto.defMessage
        FinalizationRewardCommissionNotInRange -> Proto.make $ ProtoFields.finalizationRewardCommissionNotInRange .= Proto.defMessage
        BakingRewardCommissionNotInRange -> Proto.make $ ProtoFields.bakingRewardCommissionNotInRange .= Proto.defMessage
        TransactionFeeCommissionNotInRange -> Proto.make $ ProtoFields.transactionFeeCommissionNotInRange .= Proto.defMessage
        AlreadyADelegator -> Proto.make $ ProtoFields.alreadyADelegator .= Proto.defMessage
        InsufficientBalanceForDelegationStake -> Proto.make $ ProtoFields.insufficientBalanceForDelegationStake .= Proto.defMessage
        MissingDelegationAddParameters -> Proto.make $ ProtoFields.missingDelegationAddParameters .= Proto.defMessage
        InsufficientDelegationStake -> Proto.make $ ProtoFields.insufficientDelegationStake .= Proto.defMessage
        DelegatorInCooldown -> Proto.make $ ProtoFields.delegatorInCooldown .= Proto.defMessage
        NotADelegator addr -> Proto.make $ ProtoFields.notADelegator .= toProto addr
        DelegationTargetNotABaker bakerId -> Proto.make $ ProtoFields.delegationTargetNotABaker .= toProto bakerId
        StakeOverMaximumThresholdForPool -> Proto.make $ ProtoFields.stakeOverMaximumThresholdForPool .= Proto.defMessage
        PoolWouldBecomeOverDelegated -> Proto.make $ ProtoFields.poolWouldBecomeOverDelegated .= Proto.defMessage
        PoolClosed -> Proto.make $ ProtoFields.poolClosed .= Proto.defMessage

{- |Attempt to convert the node's TransactionStatus type into the protobuf BlockItemStatus type.
  The protobuf type is better structured and removes the need for handling impossible cases.
  For example the case of an account transfer resulting in a smart contract update, which is a
  technical possibility in the way that the node's trx status is defined.
-}
toBlockItemStatus :: QueryTypes.TransactionStatus -> Either ConversionError Proto.BlockItemStatus
toBlockItemStatus ts = case ts of
    QueryTypes.Received -> Right . Proto.make $ ProtoFields.received .= Proto.defMessage
    QueryTypes.Finalized bh trx -> do
        bis <- toBis trx
        trxInBlock <- toTrxInBlock bh bis
        Right . Proto.make $ ProtoFields.finalized . ProtoFields.outcome .= trxInBlock
    QueryTypes.Committed trxs -> do
        outcomes <- mapM (\(bh, trx) -> toTrxInBlock bh =<< toBis trx) $ Map.toList trxs
        Right . Proto.make $ ProtoFields.committed . ProtoFields.outcomes .= outcomes
  where
    -- \|Convert a transaction summary to a proto block item summary.
    --  The transaction summary can technically be Nothing, but it should never occur.
    toBis :: Maybe TransactionSummary -> Either ConversionError Proto.BlockItemSummary
    toBis Nothing = Left CEInvalidTransactionResult
    toBis (Just t) = toBlockItemSummary t

    toTrxInBlock bh bis = Right . Proto.make $ do
        ProtoFields.blockHash .= toProto bh
        ProtoFields.outcome .= bis

{- |Attempt to convert a TransactionSummary type into the protobuf BlockItemSummary type.
  See @toBlockItemStatus@ for more context.
-}
toBlockItemSummary :: TransactionSummary -> Either ConversionError Proto.BlockItemSummary
toBlockItemSummary TransactionSummary{..} = case tsType of
    TSTAccountTransaction tty -> do
        sender <- case tsSender of
            Nothing -> Left CEInvalidTransactionResult
            Just acc -> Right acc
        details <- convertAccountTransaction tty tsCost sender tsResult
        Right . Proto.make $ do
            ProtoFields.index .= mkWord64 tsIndex
            ProtoFields.energyCost .= toProto tsEnergyCost
            ProtoFields.hash .= toProto tsHash
            ProtoFields.accountTransaction .= details
    TSTCredentialDeploymentTransaction ct -> case tsResult of
        TxReject _ -> Left CEFailedAccountCreation
        TxSuccess events -> case events of
            [AccountCreated addr, CredentialDeployed{..}] ->
                let
                    details = Proto.make $ do
                        ProtoFields.credentialType .= toProto ct
                        ProtoFields.address .= toProto addr
                        ProtoFields.regId .= toProto ecdRegId
                 in
                    Right . Proto.make $ do
                        ProtoFields.index .= mkWord64 tsIndex
                        ProtoFields.energyCost .= toProto tsEnergyCost
                        ProtoFields.hash .= toProto tsHash
                        ProtoFields.accountCreation .= details
            _ -> Left CEInvalidAccountCreation
    TSTUpdateTransaction ut -> case tsResult of
        TxReject _ -> Left CEFailedUpdate
        TxSuccess events -> case events of
            [UpdateEnqueued{..}] -> do
                payload <- convertUpdatePayload ut uePayload
                details <- Right . Proto.make $ do
                    ProtoFields.effectiveTime .= toProto ueEffectiveTime
                    ProtoFields.payload .= payload
                Right . Proto.make $ do
                    ProtoFields.index .= mkWord64 tsIndex
                    ProtoFields.energyCost .= toProto tsEnergyCost
                    ProtoFields.hash .= toProto tsHash
                    ProtoFields.update .= details
            _ -> Left CEInvalidUpdateResult

instance ToProto Updates.ProtocolUpdate where
    type Output Updates.ProtocolUpdate = Proto.ProtocolUpdate
    toProto Updates.ProtocolUpdate{..} = Proto.make $ do
        ProtoFields.message .= puMessage
        ProtoFields.specificationUrl .= puSpecificationURL
        ProtoFields.specificationHash .= toProto puSpecificationHash
        ProtoFields.specificationAuxiliaryData .= puSpecificationAuxiliaryData

instance ToProto (Parameters.MintDistribution 'ChainParametersV0) where
    type Output (Parameters.MintDistribution 'ChainParametersV0) = Proto.MintDistributionCpv0
    toProto md = Proto.make $ do
        ProtoFields.mintPerSlot .= toProto (md ^. Parameters.mdMintPerSlot . Parameters.mpsMintPerSlot)
        ProtoFields.bakingReward .= toProto (Parameters._mdBakingReward md)
        ProtoFields.finalizationReward .= toProto (Parameters._mdFinalizationReward md)

instance ToProto (Parameters.MintDistribution 'ChainParametersV1) where
    type Output (Parameters.MintDistribution 'ChainParametersV1) = Proto.MintDistributionCpv1
    toProto md = Proto.make $ do
        ProtoFields.bakingReward .= toProto (Parameters._mdBakingReward md)
        ProtoFields.finalizationReward .= toProto (Parameters._mdFinalizationReward md)

instance ToProto Parameters.TransactionFeeDistribution where
    type Output Parameters.TransactionFeeDistribution = Proto.TransactionFeeDistribution
    toProto Parameters.TransactionFeeDistribution{..} = Proto.make $ do
        ProtoFields.baker .= toProto _tfdBaker
        ProtoFields.gasAccount .= toProto _tfdGASAccount

instance ToProto Parameters.GASRewards where
    type Output Parameters.GASRewards = Proto.GasRewards
    toProto Parameters.GASRewards{..} = Proto.make $ do
        ProtoFields.baker .= toProto _gasBaker
        ProtoFields.finalizationProof .= toProto _gasFinalizationProof
        ProtoFields.accountCreation .= toProto _gasAccountCreation
        ProtoFields.chainUpdate .= toProto _gasChainUpdate

instance ToProto (Parameters.PoolParameters 'ChainParametersV0) where
    type Output (Parameters.PoolParameters 'ChainParametersV0) = Proto.BakerStakeThreshold
    toProto pp = Proto.make $ ProtoFields.bakerStakeThreshold .= toProto (pp ^. Parameters.ppBakerStakeThreshold)

instance ToProto (Parameters.PoolParameters 'ChainParametersV1) where
    type Output (Parameters.PoolParameters 'ChainParametersV1) = Proto.PoolParametersCpv1
    toProto pp = Proto.make $ do
        ProtoFields.passiveFinalizationCommission .= toProto (pp ^. Parameters.ppPassiveCommissions . finalizationCommission)
        ProtoFields.passiveBakingCommission .= toProto (pp ^. Parameters.ppPassiveCommissions . bakingCommission)
        ProtoFields.passiveTransactionCommission .= toProto (pp ^. Parameters.ppPassiveCommissions . transactionCommission)
        ProtoFields.commissionBounds
            .= Proto.make
                ( do
                    ProtoFields.finalization .= toProto (pp ^. Parameters.ppCommissionBounds . Parameters.finalizationCommissionRange)
                    ProtoFields.baking .= toProto (pp ^. Parameters.ppCommissionBounds . Parameters.bakingCommissionRange)
                    ProtoFields.transaction .= toProto (pp ^. Parameters.ppCommissionBounds . Parameters.transactionCommissionRange)
                )
        ProtoFields.minimumEquityCapital .= toProto (pp ^. Parameters.ppMinimumEquityCapital)
        ProtoFields.capitalBound .= toProto (pp ^. Parameters.ppCapitalBound)
        ProtoFields.leverageBound .= toProto (pp ^. Parameters.ppLeverageBound)

instance ToProto (Parameters.CooldownParameters 'ChainParametersV1) where
    type Output (Parameters.CooldownParameters 'ChainParametersV1) = Proto.CooldownParametersCpv1
    toProto (Parameters.CooldownParametersV1{..}) = Proto.make $ do
        ProtoFields.poolOwnerCooldown .= toProto _cpPoolOwnerCooldown
        ProtoFields.delegatorCooldown .= toProto _cpDelegatorCooldown

instance ToProto (Parameters.TimeParameters 'ChainParametersV1) where
    type Output (Parameters.TimeParameters 'ChainParametersV1) = Proto.TimeParametersCpv1
    toProto Parameters.TimeParametersV1{..} = Proto.make $ do
        ProtoFields.rewardPeriodLength .= toProto _tpRewardPeriodLength
        ProtoFields.mintPerPayday .= toProto _tpMintPerPayday

{- |Attempt to construct the protobuf updatepayload.
  See @toBlockItemStatus@ for more context.
-}
convertUpdatePayload :: Updates.UpdateType -> Updates.UpdatePayload -> Either ConversionError Proto.UpdatePayload
convertUpdatePayload ut pl = case (ut, pl) of
    (Updates.UpdateProtocol, Updates.ProtocolUpdatePayload pu) -> Right . Proto.make $ ProtoFields.protocolUpdate .= toProto pu
    (Updates.UpdateElectionDifficulty, Updates.ElectionDifficultyUpdatePayload ed) -> Right . Proto.make $ ProtoFields.electionDifficultyUpdate .= toProto ed
    (Updates.UpdateEuroPerEnergy, Updates.EuroPerEnergyUpdatePayload er) -> Right . Proto.make $ ProtoFields.euroPerEnergyUpdate .= toProto er
    (Updates.UpdateMicroGTUPerEuro, Updates.MicroGTUPerEuroUpdatePayload er) -> Right . Proto.make $ ProtoFields.microCcdPerEuroUpdate .= toProto er
    (Updates.UpdateFoundationAccount, Updates.FoundationAccountUpdatePayload addr) -> Right . Proto.make $ ProtoFields.foundationAccountUpdate .= toProto addr
    (Updates.UpdateMintDistribution, Updates.MintDistributionUpdatePayload md) -> Right . Proto.make $ ProtoFields.mintDistributionUpdate .= toProto md
    (Updates.UpdateTransactionFeeDistribution, Updates.TransactionFeeDistributionUpdatePayload tfd) ->
        Right . Proto.make $ ProtoFields.transactionFeeDistributionUpdate .= toProto tfd
    (Updates.UpdateGASRewards, Updates.GASRewardsUpdatePayload gr) -> Right . Proto.make $ ProtoFields.gasRewardsUpdate .= toProto gr
    (Updates.UpdatePoolParameters, Updates.BakerStakeThresholdUpdatePayload pp) ->
        Right . Proto.make $ ProtoFields.bakerStakeThresholdUpdate .= toProto pp
    (Updates.UpdateRootKeys, Updates.RootUpdatePayload ru@(Updates.RootKeysRootUpdate{})) -> Right . Proto.make $ ProtoFields.rootUpdate .= toProto ru
    (Updates.UpdateLevel1Keys, Updates.RootUpdatePayload ru@(Updates.Level1KeysRootUpdate{})) -> Right . Proto.make $ ProtoFields.rootUpdate .= toProto ru
    (Updates.UpdateLevel2Keys, Updates.RootUpdatePayload ru@(Updates.Level2KeysRootUpdate{})) -> Right . Proto.make $ ProtoFields.rootUpdate .= toProto ru
    (Updates.UpdateLevel2Keys, Updates.RootUpdatePayload ru@(Updates.Level2KeysRootUpdateV1{})) -> Right . Proto.make $ ProtoFields.rootUpdate .= toProto ru
    (Updates.UpdateLevel1Keys, Updates.Level1UpdatePayload u@(Updates.Level1KeysLevel1Update{})) -> Right . Proto.make $ ProtoFields.level1Update .= toProto u
    (Updates.UpdateLevel2Keys, Updates.Level1UpdatePayload u@(Updates.Level2KeysLevel1Update{})) -> Right . Proto.make $ ProtoFields.level1Update .= toProto u
    (Updates.UpdateLevel2Keys, Updates.Level1UpdatePayload u@(Updates.Level2KeysLevel1UpdateV1{})) -> Right . Proto.make $ ProtoFields.level1Update .= toProto u
    (Updates.UpdateAddAnonymityRevoker, Updates.AddAnonymityRevokerUpdatePayload ai) -> Right . Proto.make $ ProtoFields.addAnonymityRevokerUpdate .= toProto ai
    (Updates.UpdateAddIdentityProvider, Updates.AddIdentityProviderUpdatePayload ip) -> Right . Proto.make $ ProtoFields.addIdentityProviderUpdate .= toProto ip
    (Updates.UpdateCooldownParameters, Updates.CooldownParametersCPV1UpdatePayload cp) -> Right $ Proto.make $ ProtoFields.cooldownParametersCpv1Update .= toProto cp
    (Updates.UpdatePoolParameters, Updates.PoolParametersCPV1UpdatePayload pp) -> Right . Proto.make $ ProtoFields.poolParametersCpv1Update .= toProto pp
    (Updates.UpdateTimeParameters, Updates.TimeParametersCPV1UpdatePayload tp) -> Right . Proto.make $ ProtoFields.timeParametersCpv1Update .= toProto tp
    (Updates.UpdateMintDistribution, Updates.MintDistributionCPV1UpdatePayload md) -> Right . Proto.make $ ProtoFields.mintDistributionCpv1Update .= toProto md
    _ -> Left CEInvalidUpdateResult

-- |The different conversions errors possible in @toBlockItemStatus@ (and the helper to* functions it calls).
data ConversionError
    = -- |An account creation failed.
      CEFailedAccountCreation
    | -- |An account creation transaction occurred but was malformed and could not be converted.
      CEInvalidAccountCreation
    | -- |An update transaction failed.
      CEFailedUpdate
    | -- |An update transaction occurred but was malformed and could not be converted.
      CEInvalidUpdateResult
    | -- |An account transaction occurred but was malformed and could not be converted.
      CEInvalidTransactionResult
    deriving (Eq)

instance Show ConversionError where
    show e = case e of
        CEFailedAccountCreation -> "An account creation failed."
        CEInvalidAccountCreation -> "An account creation transaction occurred but was malformed and could not be converted."
        CEFailedUpdate -> "An update transaction failed."
        CEInvalidUpdateResult -> "An update transaction occurred but was malformed and could not be converted."
        CEInvalidTransactionResult -> "An account transaction occurred but was malformed and could not be converted."

instance ToProto TransactionTime where
    type Output TransactionTime = Proto.TransactionTime
    toProto = mkWord64

instance ToProto ExchangeRate where
    type Output ExchangeRate = Proto.ExchangeRate
    toProto (ExchangeRate r) = Proto.make $ ProtoFields.value .= toProto r

instance ToProto (Ratio.Ratio Word64) where
    type Output (Ratio.Ratio Word64) = Proto.Ratio
    toProto r = Proto.make $ do
        ProtoFields.numerator .= Ratio.numerator r
        ProtoFields.denominator .= Ratio.denominator r

instance ToProto (Parameters.InclusiveRange AmountFraction) where
    type Output (Parameters.InclusiveRange AmountFraction) = Proto.InclusiveRangeAmountFraction
    toProto Parameters.InclusiveRange{..} = Proto.make $ do
        ProtoFields.min .= toProto irMin
        ProtoFields.max .= toProto irMax

instance ToProto Parameters.CapitalBound where
    type Output Parameters.CapitalBound = Proto.CapitalBound
    toProto Parameters.CapitalBound{..} = Proto.make $ ProtoFields.value .= toProto theCapitalBound

instance ToProto Parameters.LeverageFactor where
    type Output Parameters.LeverageFactor = Proto.LeverageFactor
    toProto Parameters.LeverageFactor{..} = Proto.make $ ProtoFields.value .= toProto theLeverageFactor

instance ToProto RewardPeriodLength where
    type Output RewardPeriodLength = Proto.RewardPeriodLength
    toProto rpl = Proto.make $ ProtoFields.value .= mkWord64 rpl

instance ToProto DurationSeconds where
    type Output DurationSeconds = Proto.DurationSeconds
    toProto = mkWord64

instance ToProto ArInfo.ArInfo where
    type Output ArInfo.ArInfo = Proto.ArInfo
    toProto ai = Proto.make $ do
        ProtoFields.identity .= mkWord32 (ArInfo.arIdentity ai)
        ProtoFields.description
            .= Proto.make
                ( do
                    ProtoFields.name .= ArInfo.arName ai
                    ProtoFields.url .= ArInfo.arUrl ai
                    ProtoFields.description .= ArInfo.arDescription ai
                )
        ProtoFields.publicKey . ProtoFields.value .= ArInfo.arPublicKey ai

instance ToProto IpInfo.IpInfo where
    type Output IpInfo.IpInfo = Proto.IpInfo
    toProto ii = Proto.make $ do
        ProtoFields.identity .= mkWord32 (IpInfo.ipIdentity ii)
        ProtoFields.description
            .= Proto.make
                ( do
                    ProtoFields.name .= IpInfo.ipName ii
                    ProtoFields.url .= IpInfo.ipUrl ii
                    ProtoFields.description .= IpInfo.ipDescription ii
                )
        ProtoFields.verifyKey . ProtoFields.value .= IpInfo.ipVerifyKey ii
        ProtoFields.cdiVerifyKey . ProtoFields.value .= IpInfo.ipCdiVerifyKey ii

instance ToProto Updates.Level1Update where
    type Output Updates.Level1Update = Proto.Level1Update
    toProto Updates.Level1KeysLevel1Update{..} = Proto.make $ ProtoFields.level1KeysUpdate .= toProto l1kl1uKeys
    toProto Updates.Level2KeysLevel1Update{..} = Proto.make $ ProtoFields.level2KeysUpdateV0 .= toProto l2kl1uAuthorizations
    toProto Updates.Level2KeysLevel1UpdateV1{..} = Proto.make $ ProtoFields.level2KeysUpdateV1 .= toProto l2kl1uAuthorizationsV1

instance ToProto Updates.RootUpdate where
    type Output Updates.RootUpdate = Proto.RootUpdate
    toProto ru = case ru of
        Updates.RootKeysRootUpdate{..} -> Proto.make $ ProtoFields.rootKeysUpdate .= toProto rkruKeys
        Updates.Level1KeysRootUpdate{..} -> Proto.make $ ProtoFields.level1KeysUpdate .= toProto l1kruKeys
        Updates.Level2KeysRootUpdate{..} -> Proto.make $ ProtoFields.level2KeysUpdateV0 .= toProto l2kruAuthorizations
        Updates.Level2KeysRootUpdateV1{..} -> Proto.make $ ProtoFields.level2KeysUpdateV1 .= toProto l2kruAuthorizationsV1

instance ToProto (Updates.HigherLevelKeys kind) where
    type Output (Updates.HigherLevelKeys kind) = Proto.HigherLevelKeys
    toProto keys = Proto.make $ do
        ProtoFields.keys .= map toProto (Vec.toList $ Updates.hlkKeys keys)
        ProtoFields.threshold .= toProto (Updates.hlkThreshold keys)

instance IsChainParametersVersion cpv => ToProto (Updates.Authorizations cpv) where
    type Output (Updates.Authorizations cpv) = AuthorizationsFamily cpv
    toProto auth =
        let
            v0 :: Proto.AuthorizationsV0
            v0 = Proto.make $ do
                ProtoFields.keys .= map toProto (Vec.toList $ Updates.asKeys auth)
                ProtoFields.emergency .= toProto (Updates.asEmergency auth)
                ProtoFields.protocol .= toProto (Updates.asProtocol auth)
                ProtoFields.parameterElectionDifficulty .= toProto (Updates.asParamElectionDifficulty auth)
                ProtoFields.parameterEuroPerEnergy .= toProto (Updates.asParamEuroPerEnergy auth)
                ProtoFields.parameterMicroCCDPerEuro .= toProto (Updates.asParamMicroGTUPerEuro auth)
                ProtoFields.parameterFoundationAccount .= toProto (Updates.asParamFoundationAccount auth)
                ProtoFields.parameterMintDistribution .= toProto (Updates.asParamMintDistribution auth)
                ProtoFields.parameterTransactionFeeDistribution .= toProto (Updates.asParamTransactionFeeDistribution auth)
                ProtoFields.parameterGasRewards .= toProto (Updates.asParamGASRewards auth)
                ProtoFields.poolParameters .= toProto (Updates.asPoolParameters auth)
                ProtoFields.addAnonymityRevoker .= toProto (Updates.asAddAnonymityRevoker auth)
                ProtoFields.addIdentityProvider .= toProto (Updates.asAddIdentityProvider auth)
         in
            case chainParametersVersion @cpv of
                SCPV0 -> v0
                SCPV1 -> Proto.make $ do
                    ProtoFields.v0 .= v0
                    case Updates.asCooldownParameters auth of
                        JustForCPV1 as -> ProtoFields.parameterCooldown .= toProto as
                    case Updates.asTimeParameters auth of
                        JustForCPV1 as -> ProtoFields.parameterTime .= toProto as

-- |Defines a type family that is used in the ToProto instance for Updates.Authorizations.
type family AuthorizationsFamily cpv where
    AuthorizationsFamily 'ChainParametersV0 = Proto.AuthorizationsV0
    AuthorizationsFamily 'ChainParametersV1 = Proto.AuthorizationsV1

instance ToProto Updates.AccessStructure where
    type Output Updates.AccessStructure = Proto.AccessStructure
    toProto Updates.AccessStructure{..} = Proto.make $ do
        ProtoFields.accessPublicKeys .= map toProtoUpdateKeysIndex (Set.toList accessPublicKeys)
        ProtoFields.accessThreshold .= toProto accessThreshold
      where
        toProtoUpdateKeysIndex i = Proto.make $ ProtoFields.value .= fromIntegral i

instance ToProto Updates.UpdatePublicKey where
    type Output Updates.UpdatePublicKey = Proto.UpdatePublicKey
    toProto (VerifyKeyEd25519 key) = Proto.make $ ProtoFields.value .= S.encode key

instance ToProto Updates.UpdateKeysThreshold where
    type Output Updates.UpdateKeysThreshold = Proto.UpdateKeysThreshold
    toProto Updates.UpdateKeysThreshold{..} = Proto.make $ ProtoFields.value .= fromIntegral uktTheThreshold

instance ToProto MintRate where
    type Output MintRate = Proto.MintRate
    toProto MintRate{..} = Proto.make $ do
        ProtoFields.mantissa .= mrMantissa
        ProtoFields.exponent .= fromIntegral mrExponent

instance ToProto Wasm.WasmVersion where
    type Output Wasm.WasmVersion = Proto.ContractVersion
    toProto Wasm.V0 = Proto.V0
    toProto Wasm.V1 = Proto.V1

instance ToProto Wasm.ContractEvent where
    type Output Wasm.ContractEvent = Proto.ContractEvent
    toProto (Wasm.ContractEvent shortBS) = Proto.make $ ProtoFields.value .= BSS.fromShort shortBS

instance ToProto CredentialType where
    type Output CredentialType = Proto.CredentialType
    toProto Initial = Proto.CREDENTIAL_TYPE_INITIAL
    toProto Normal = Proto.CREDENTIAL_TYPE_NORMAL

type BakerAddedEvent = (BakerKeysEvent, Amount, Bool)

instance ToProto BakerAddedEvent where
    type Output BakerAddedEvent = Proto.BakerEvent'BakerAdded
    toProto (keysEvent, stake, restakeEarnings) = Proto.make $ do
        ProtoFields.keysEvent .= toProto keysEvent
        ProtoFields.stake .= toProto stake
        ProtoFields.restakeEarnings .= restakeEarnings

type BakerKeysEvent = (BakerId, AccountAddress, BakerSignVerifyKey, BakerElectionVerifyKey, BakerAggregationVerifyKey)
instance ToProto BakerKeysEvent where
    type Output BakerKeysEvent = Proto.BakerKeysEvent
    toProto (bakerId, addr, signKey, electionKey, aggregationKey) = Proto.make $ do
        ProtoFields.bakerId .= toProto bakerId
        ProtoFields.account .= toProto addr
        ProtoFields.signKey .= toProto signKey
        ProtoFields.electionKey .= toProto electionKey
        ProtoFields.aggregationKey .= toProto aggregationKey

instance ToProto BakerSignVerifyKey where
    type Output BakerSignVerifyKey = Proto.BakerSignatureVerifyKey
    toProto = mkSerialize

instance ToProto BakerElectionVerifyKey where
    type Output BakerElectionVerifyKey = Proto.BakerElectionVerifyKey
    toProto = mkSerialize

instance ToProto BakerAggregationVerifyKey where
    type Output BakerAggregationVerifyKey = Proto.BakerAggregationVerifyKey
    toProto = mkSerialize

instance ToProto Memo where
    type Output Memo = Proto.Memo
    toProto (Memo shortBS) = Proto.make $ ProtoFields.value .= BSS.fromShort shortBS

instance ToProto RegisteredData where
    type Output RegisteredData = Proto.RegisteredData
    toProto (RegisteredData shortBS) = Proto.make $ ProtoFields.value .= BSS.fromShort shortBS

convertContractRelatedEvents :: Event -> Either ConversionError Proto.ContractTraceElement
convertContractRelatedEvents event = case event of
    Updated{..} ->
        Right . Proto.make $
            ProtoFields.updated
                .= Proto.make
                    ( do
                        ProtoFields.contractVersion .= toProto euContractVersion
                        ProtoFields.address .= toProto euAddress
                        ProtoFields.instigator .= toProto euInstigator
                        ProtoFields.amount .= toProto euAmount
                        ProtoFields.parameter .= toProto euMessage
                        ProtoFields.receiveName .= toProto euReceiveName
                        ProtoFields.events .= map toProto euEvents
                    )
    Transferred{..} -> do
        sender' <- case etFrom of
            AddressAccount _ -> Left CEInvalidTransactionResult
            AddressContract addr -> Right addr
        receiver <- case etTo of
            AddressAccount addr -> Right addr
            AddressContract _ -> Left CEInvalidTransactionResult
        Right . Proto.make $
            ProtoFields.transferred
                .= Proto.make
                    ( do
                        ProtoFields.sender .= toProto sender'
                        ProtoFields.amount .= toProto etAmount
                        ProtoFields.receiver .= toProto receiver
                    )
    Interrupted{..} ->
        Right . Proto.make $
            ProtoFields.interrupted
                .= Proto.make
                    ( do
                        ProtoFields.address .= toProto iAddress
                        ProtoFields.events .= map toProto iEvents
                    )
    Resumed{..} ->
        Right . Proto.make $
            ProtoFields.resumed
                .= Proto.make
                    ( do
                        ProtoFields.address .= toProto rAddress
                        ProtoFields.success .= rSuccess
                    )
    Upgraded{..} ->
        Right . Proto.make $
            ProtoFields.upgraded
                .= Proto.make
                    ( do
                        ProtoFields.address .= toProto euAddress
                        ProtoFields.from .= toProto euFrom
                        ProtoFields.to .= toProto euTo
                    )
    _ -> Left CEInvalidTransactionResult

{- |Attempt to construct the protobuf type AccounTransactionType.
 See @toBlockItemStatus@ for more context.
-}
convertAccountTransaction ::
    -- | The transaction type. @Nothing@ means that the transaction was serialized incorrectly.
    Maybe TransactionType ->
    -- | The cost of the transaction.
    Amount ->
    -- | The sender of the transaction.
    AccountAddress ->
    -- | The result of the transaction. If the transaction was rejected, it contains the reject reason.
    --   Otherwise it contains the events.
    ValidResult ->
    Either ConversionError Proto.AccountTransactionDetails
convertAccountTransaction ty cost sender result = case ty of
    Nothing -> Right . mkNone $ SerializationFailure
    Just ty' -> case result of
        TxReject rejectReason -> Right . mkNone $ rejectReason
        TxSuccess events -> case ty' of
            TTDeployModule ->
                mkSuccess <$> do
                    v <- case events of
                        [ModuleDeployed moduleRef] -> Right $ toProto moduleRef
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.moduleDeployed .= v
            TTInitContract ->
                mkSuccess <$> do
                    v <- case events of
                        [ContractInitialized{..}] -> Right $ Proto.make $ do
                            ProtoFields.contractVersion .= toProto ecContractVersion
                            ProtoFields.originRef .= toProto ecRef
                            ProtoFields.address .= toProto ecAddress
                            ProtoFields.amount .= toProto ecAmount
                            ProtoFields.initName .= toProto ecInitName
                            ProtoFields.events .= map toProto ecEvents
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.contractInitialized .= v
            TTUpdate ->
                mkSuccess <$> do
                    v <- mapM convertContractRelatedEvents events
                    Right . Proto.make $ ProtoFields.contractUpdateIssued . ProtoFields.effects .= v
            TTTransfer ->
                mkSuccess <$> do
                    v <- case events of
                        [Transferred{..}] -> case etTo of
                            AddressContract _ -> Left CEInvalidTransactionResult
                            AddressAccount receiver -> Right . Proto.make $ do
                                ProtoFields.amount .= toProto etAmount
                                ProtoFields.receiver .= toProto receiver
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.accountTransfer .= v
            TTTransferWithMemo ->
                mkSuccess <$> do
                    v <- case events of
                        [Transferred{..}, TransferMemo{..}] -> case etTo of
                            AddressContract _ -> Left CEInvalidTransactionResult
                            AddressAccount receiver -> Right . Proto.make $ do
                                ProtoFields.amount .= toProto etAmount
                                ProtoFields.receiver .= toProto receiver
                                ProtoFields.memo .= toProto tmMemo
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.accountTransfer .= v
            TTAddBaker ->
                mkSuccess <$> do
                    v <- case events of
                        [BakerAdded{..}] -> Right $ toProto ((ebaBakerId, ebaAccount, ebaSignKey, ebaElectionKey, ebaAggregationKey), ebaStake, ebaRestakeEarnings)
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.bakerAdded .= v
            TTRemoveBaker ->
                mkSuccess <$> do
                    v <- case events of
                        [BakerRemoved{..}] -> Right $ toProto ebrBakerId
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.bakerRemoved .= v
            TTUpdateBakerStake ->
                mkSuccess <$> do
                    v <- case events of
                        [] -> Right Nothing
                        [BakerStakeIncreased{..}] -> Right . Just . Proto.make $ do
                            ProtoFields.bakerId .= toProto ebsiBakerId
                            ProtoFields.newStake .= toProto ebsiNewStake
                            ProtoFields.increased .= True
                        [BakerStakeDecreased{..}] -> Right . Just . Proto.make $ do
                            ProtoFields.bakerId .= toProto ebsiBakerId
                            ProtoFields.newStake .= toProto ebsiNewStake
                            ProtoFields.increased .= False
                        _ -> Left CEInvalidTransactionResult
                    case v of
                        Nothing -> Right . Proto.make $ ProtoFields.bakerStakeUpdated .= Proto.defMessage
                        Just val -> Right . Proto.make $ ProtoFields.bakerStakeUpdated . ProtoFields.update .= val
            TTUpdateBakerRestakeEarnings ->
                mkSuccess <$> do
                    v <- case events of
                        [BakerSetRestakeEarnings{..}] -> Right $ Proto.make $ do
                            ProtoFields.bakerId .= toProto ebsreBakerId
                            ProtoFields.restakeEarnings .= ebsreRestakeEarnings
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.bakerRestakeEarningsUpdated .= v
            TTUpdateBakerKeys ->
                mkSuccess <$> do
                    v <- case events of
                        [BakerKeysUpdated{..}] -> Right $ toProto (ebkuBakerId, ebkuAccount, ebkuSignKey, ebkuElectionKey, ebkuAggregationKey)
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.bakerKeysUpdated .= v
            TTEncryptedAmountTransfer ->
                mkSuccess <$> do
                    v <- case events of
                        [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}] ->
                            let
                                removed = Proto.make $ do
                                    ProtoFields.account .= toProto earAccount
                                    ProtoFields.newAmount .= toProto earNewAmount
                                    ProtoFields.inputAmount .= toProto earInputAmount
                                    ProtoFields.upToIndex .= theAggIndex earUpToIndex
                                added = Proto.make $ do
                                    ProtoFields.receiver .= toProto neaAccount
                                    ProtoFields.newIndex .= theIndex neaNewIndex
                                    ProtoFields.encryptedAmount .= toProto neaEncryptedAmount
                             in
                                Right . Proto.make $ do
                                    ProtoFields.removed .= removed
                                    ProtoFields.added .= added
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.encryptedAmountTransferred .= v
            TTEncryptedAmountTransferWithMemo ->
                mkSuccess <$> do
                    v <- case events of
                        [EncryptedAmountsRemoved{..}, NewEncryptedAmount{..}, TransferMemo{..}] ->
                            let
                                removed = Proto.make $ do
                                    ProtoFields.account .= toProto earAccount
                                    ProtoFields.newAmount .= toProto earNewAmount
                                    ProtoFields.inputAmount .= toProto earInputAmount
                                    ProtoFields.upToIndex .= theAggIndex earUpToIndex
                                added = Proto.make $ do
                                    ProtoFields.receiver .= toProto neaAccount
                                    ProtoFields.newIndex .= theIndex neaNewIndex
                                    ProtoFields.encryptedAmount .= toProto neaEncryptedAmount
                             in
                                Right . Proto.make $ do
                                    ProtoFields.removed .= removed
                                    ProtoFields.added .= added
                                    ProtoFields.memo .= toProto tmMemo
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.encryptedAmountTransferred .= v
            TTTransferToEncrypted ->
                mkSuccess <$> do
                    v <- case events of
                        [EncryptedSelfAmountAdded{..}] -> Right . Proto.make $ do
                            ProtoFields.account .= toProto eaaAccount
                            ProtoFields.newAmount .= toProto eaaNewAmount
                            ProtoFields.amount .= toProto eaaAmount
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.transferredToEncrypted .= v
            TTTransferToPublic ->
                mkSuccess <$> do
                    v <- case events of
                        [EncryptedAmountsRemoved{..}, AmountAddedByDecryption{..}] ->
                            let
                                removed = Proto.make $ do
                                    ProtoFields.account .= toProto earAccount
                                    ProtoFields.newAmount .= toProto earNewAmount
                                    ProtoFields.inputAmount .= toProto earInputAmount
                                    ProtoFields.upToIndex .= theAggIndex earUpToIndex
                             in
                                Right . Proto.make $ do
                                    ProtoFields.removed .= removed
                                    ProtoFields.amount .= toProto aabdAmount
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.transferredToPublic .= v
            TTTransferWithSchedule ->
                mkSuccess <$> do
                    v <- case events of
                        [TransferredWithSchedule{..}] -> Right . Proto.make $ do
                            ProtoFields.receiver .= toProto etwsTo
                            ProtoFields.amount .= map toProto etwsAmount
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.transferredWithSchedule .= v
            TTTransferWithScheduleAndMemo ->
                mkSuccess <$> do
                    v <- case events of
                        [TransferredWithSchedule{..}, TransferMemo{..}] -> Right . Proto.make $ do
                            ProtoFields.receiver .= toProto etwsTo
                            ProtoFields.amount .= map toProto etwsAmount
                            ProtoFields.memo .= toProto tmMemo
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.transferredWithSchedule .= v
            TTUpdateCredentialKeys ->
                mkSuccess <$> do
                    v <- case events of
                        [CredentialKeysUpdated{..}] -> Right $ toProto ckuCredId
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.credentialKeysUpdated .= v
            TTUpdateCredentials ->
                mkSuccess <$> do
                    v <- case events of
                        [CredentialsUpdated{..}] -> Right . Proto.make $ do
                            ProtoFields.newCredIds .= map toProto cuNewCredIds
                            ProtoFields.removedCredIds .= map toProto cuRemovedCredIds
                            ProtoFields.newThreshold .= toProto cuNewThreshold
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.credentialsUpdated .= v
            TTRegisterData ->
                mkSuccess <$> do
                    v <- case events of
                        [DataRegistered{..}] -> Right $ toProto drData
                        _ -> Left CEInvalidTransactionResult
                    Right . Proto.make $ ProtoFields.dataRegistered .= v
            TTConfigureBaker ->
                mkSuccess <$> do
                    let toBakerEvent = \case
                            BakerAdded{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerAdded
                                        .= toProto ((ebaBakerId, ebaAccount, ebaSignKey, ebaElectionKey, ebaAggregationKey), ebaStake, ebaRestakeEarnings)
                            BakerRemoved{..} -> Right . Proto.make $ ProtoFields.bakerRemoved .= toProto ebrBakerId
                            BakerStakeIncreased{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerStakeIncreased
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsiBakerId
                                                ProtoFields.newStake .= toProto ebsiNewStake
                                            )
                            BakerStakeDecreased{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerStakeDecreased
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsiBakerId
                                                ProtoFields.newStake .= toProto ebsiNewStake
                                            )
                            BakerSetRestakeEarnings{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerRestakeEarningsUpdated
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsreBakerId
                                                ProtoFields.restakeEarnings .= ebsreRestakeEarnings
                                            )
                            BakerKeysUpdated{..} -> Right . Proto.make $ ProtoFields.bakerKeysUpdated .= toProto (ebkuBakerId, ebkuAccount, ebkuSignKey, ebkuElectionKey, ebkuAggregationKey)
                            BakerSetOpenStatus{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerSetOpenStatus
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsosBakerId
                                                ProtoFields.openStatus .= toProto ebsosOpenStatus
                                            )
                            BakerSetMetadataURL{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerSetMetadataUrl
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsmuBakerId
                                                ProtoFields.url .= toProto ebsmuMetadataURL
                                            )
                            BakerSetTransactionFeeCommission{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerSetTransactionFeeCommission
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebstfcBakerId
                                                ProtoFields.transactionFeeCommission .= toProto ebstfcTransactionFeeCommission
                                            )
                            BakerSetBakingRewardCommission{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerSetBakingRewardCommission
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsbrcBakerId
                                                ProtoFields.bakingRewardCommission .= toProto ebsbrcBakingRewardCommission
                                            )
                            BakerSetFinalizationRewardCommission{..} ->
                                Right . Proto.make $
                                    ProtoFields.bakerSetFinalizationRewardCommission
                                        .= Proto.make
                                            ( do
                                                ProtoFields.bakerId .= toProto ebsfrcBakerId
                                                ProtoFields.finalizationRewardCommission .= toProto ebsfrcFinalizationRewardCommission
                                            )
                            _ -> Left CEInvalidTransactionResult
                    v <- mapM toBakerEvent events
                    Right . Proto.make $ ProtoFields.bakerConfigured . ProtoFields.events .= v
            TTConfigureDelegation ->
                mkSuccess <$> do
                    let toDelegationEvent = \case
                            DelegationStakeIncreased{..} ->
                                Right . Proto.make $
                                    ProtoFields.delegationStakeIncreased
                                        .= Proto.make
                                            ( do
                                                ProtoFields.delegatorId .= toProto edsiDelegatorId
                                                ProtoFields.newStake .= toProto edsiNewStake
                                            )
                            DelegationStakeDecreased{..} ->
                                Right . Proto.make $
                                    ProtoFields.delegationStakeDecreased
                                        .= Proto.make
                                            ( do
                                                ProtoFields.delegatorId .= toProto edsdDelegatorId
                                                ProtoFields.newStake .= toProto edsdNewStake
                                            )
                            DelegationSetRestakeEarnings{..} ->
                                Right . Proto.make $
                                    ProtoFields.delegationSetRestakeEarnings
                                        .= Proto.make
                                            ( do
                                                ProtoFields.delegatorId .= toProto edsreDelegatorId
                                                ProtoFields.restakeEarnings .= edsreRestakeEarnings
                                            )
                            DelegationSetDelegationTarget{..} ->
                                Right . Proto.make $
                                    ProtoFields.delegationSetDelegationTarget
                                        .= Proto.make
                                            ( do
                                                ProtoFields.delegatorId .= toProto edsdtDelegatorId
                                                ProtoFields.delegationTarget .= toProto edsdtDelegationTarget
                                            )
                            DelegationAdded{..} -> Right . Proto.make $ ProtoFields.delegationAdded .= toProto edaDelegatorId
                            DelegationRemoved{..} -> Right . Proto.make $ ProtoFields.delegationRemoved .= toProto edrDelegatorId
                            _ -> Left CEInvalidTransactionResult
                    v <- mapM toDelegationEvent events
                    Right . Proto.make $ ProtoFields.delegationConfigured . ProtoFields.events .= v
  where
    mkSuccess :: Proto.AccountTransactionEffects -> Proto.AccountTransactionDetails
    mkSuccess effects = Proto.make $ do
        ProtoFields.cost .= toProto cost
        ProtoFields.sender .= toProto sender
        ProtoFields.effects .= effects

    mkNone :: RejectReason -> Proto.AccountTransactionDetails
    mkNone rr = Proto.make $ do
        ProtoFields.cost .= toProto cost
        ProtoFields.sender .= toProto sender
        ProtoFields.effects
            . ProtoFields.none
            .= ( Proto.make $ do
                    ProtoFields.rejectReason .= toProto rr
                    case ty of
                        Nothing -> return ()
                        Just ty' -> ProtoFields.transactionType .= toProto ty'
               )

instance ToProto Address where
    type Output Address = Proto.Address
    toProto (AddressAccount addr) = Proto.make $ ProtoFields.account .= toProto addr
    toProto (AddressContract addr) = Proto.make $ ProtoFields.contract .= toProto addr

instance ToProto Updates.UpdateType where
    type Output Updates.UpdateType = Proto.UpdateType
    toProto Updates.UpdateProtocol = Proto.UPDATE_PROTOCOL
    toProto Updates.UpdateElectionDifficulty = Proto.UPDATE_ELECTION_DIFFICULTY
    toProto Updates.UpdateEuroPerEnergy = Proto.UPDATE_EURO_PER_ENERGY
    toProto Updates.UpdateMicroGTUPerEuro = Proto.UPDATE_MICRO_CCD_PER_EURO
    toProto Updates.UpdateFoundationAccount = Proto.UPDATE_FOUNDATION_ACCOUNT
    toProto Updates.UpdateMintDistribution = Proto.UPDATE_MINT_DISTRIBUTION
    toProto Updates.UpdateTransactionFeeDistribution = Proto.UPDATE_TRANSACTION_FEE_DISTRIBUTION
    toProto Updates.UpdateGASRewards = Proto.UPDATE_GAS_REWARDS
    toProto Updates.UpdatePoolParameters = Proto.UPDATE_POOL_PARAMETERS
    toProto Updates.UpdateAddAnonymityRevoker = Proto.ADD_ANONYMITY_REVOKER
    toProto Updates.UpdateAddIdentityProvider = Proto.ADD_IDENTITY_PROVIDER
    toProto Updates.UpdateRootKeys = Proto.UPDATE_ROOT_KEYS
    toProto Updates.UpdateLevel1Keys = Proto.UPDATE_LEVEL1_KEYS
    toProto Updates.UpdateLevel2Keys = Proto.UPDATE_LEVEL2_KEYS
    toProto Updates.UpdateCooldownParameters = Proto.UPDATE_COOLDOWN_PARAMETERS
    toProto Updates.UpdateTimeParameters = Proto.UPDATE_TIME_PARAMETERS

instance ToProto TransactionType where
    type Output TransactionType = Proto.TransactionType
    toProto TTDeployModule = Proto.DEPLOY_MODULE
    toProto TTInitContract = Proto.INIT_CONTRACT
    toProto TTUpdate = Proto.UPDATE
    toProto TTTransfer = Proto.TRANSFER
    toProto TTAddBaker = Proto.ADD_BAKER
    toProto TTRemoveBaker = Proto.REMOVE_BAKER
    toProto TTUpdateBakerStake = Proto.UPDATE_BAKER_STAKE
    toProto TTUpdateBakerRestakeEarnings = Proto.UPDATE_BAKER_RESTAKE_EARNINGS
    toProto TTUpdateBakerKeys = Proto.UPDATE_BAKER_KEYS
    toProto TTUpdateCredentialKeys = Proto.UPDATE_CREDENTIAL_KEYS
    toProto TTEncryptedAmountTransfer = Proto.ENCRYPTED_AMOUNT_TRANSFER
    toProto TTTransferToEncrypted = Proto.TRANSFER_TO_ENCRYPTED
    toProto TTTransferToPublic = Proto.TRANSFER_TO_PUBLIC
    toProto TTTransferWithSchedule = Proto.TRANSFER_WITH_SCHEDULE
    toProto TTUpdateCredentials = Proto.UPDATE_CREDENTIALS
    toProto TTRegisterData = Proto.REGISTER_DATA
    toProto TTTransferWithMemo = Proto.TRANSFER_WITH_MEMO
    toProto TTEncryptedAmountTransferWithMemo = Proto.ENCRYPTED_AMOUNT_TRANSFER_WITH_MEMO
    toProto TTTransferWithScheduleAndMemo = Proto.TRANSFER_WITH_SCHEDULE_AND_MEMO
    toProto TTConfigureBaker = Proto.CONFIGURE_BAKER
    toProto TTConfigureDelegation = Proto.CONFIGURE_DELEGATION

instance ToProto Energy where
    type Output Energy = Proto.Energy
    toProto = mkWord64

instance ToProto InvokeContract.InvokeContractResult where
    -- Since this is a conversion that may fail we use Either in the output type
    -- here so that we can forward errors, which is not in-line with other
    -- instances which are not fallible. The caller is meant to catch the error.
    type Output InvokeContract.InvokeContractResult = Either ConversionError Proto.InvokeInstanceResponse
    toProto InvokeContract.Failure{..} =
        return $
            Proto.make $
                ProtoFields.failure
                    .= Proto.make
                        ( do
                            ProtoFields.maybe'returnValue .= rcrReturnValue
                            ProtoFields.usedEnergy .= toProto rcrUsedEnergy
                            ProtoFields.reason .= toProto rcrReason
                        )
    toProto InvokeContract.Success{..} = do
        effects <- mapM convertContractRelatedEvents rcrEvents
        return $
            Proto.make $
                ProtoFields.success
                    .= Proto.make
                        ( do
                            ProtoFields.maybe'returnValue .= rcrReturnValue
                            ProtoFields.usedEnergy .= toProto rcrUsedEnergy
                            ProtoFields.effects .= effects
                        )

instance ToProto Slot where
    type Output Slot = Proto.Slot
    toProto = mkWord64

instance ToProto StateHash where
    type Output StateHash = Proto.StateHash
    toProto = mkSerialize

instance ToProto QueryTypes.BlockInfo where
    type Output QueryTypes.BlockInfo = Proto.BlockInfo
    toProto QueryTypes.BlockInfo{..} = Proto.make $ do
        ProtoFields.hash .= toProto biBlockHash
        ProtoFields.height .= toProto biBlockHeight
        ProtoFields.parentBlock .= toProto biBlockParent
        ProtoFields.lastFinalizedBlock .= toProto biBlockLastFinalized
        ProtoFields.genesisIndex .= toProto biGenesisIndex
        ProtoFields.eraBlockHeight .= toProto biEraBlockHeight
        ProtoFields.receiveTime .= toProto biBlockReceiveTime
        ProtoFields.arriveTime .= toProto biBlockArriveTime
        ProtoFields.slotNumber .= toProto biBlockSlot
        ProtoFields.slotTime .= toProto biBlockSlotTime
        ProtoFields.maybe'baker .= fmap toProto biBlockBaker
        ProtoFields.finalized .= biFinalized
        ProtoFields.transactionCount .= fromIntegral biTransactionCount
        ProtoFields.transactionsEnergyCost .= toProto biTransactionEnergyCost
        ProtoFields.transactionsSize .= fromIntegral biTransactionsSize
        ProtoFields.stateHash .= toProto biBlockStateHash

instance ToProto QueryTypes.PoolStatus where
    type Output QueryTypes.PoolStatus = Either Proto.PoolInfoResponse Proto.PassiveDelegationInfo
    toProto QueryTypes.BakerPoolStatus{..} = Left $ Proto.make $ do
        ProtoFields.baker .= toProto psBakerId
        ProtoFields.address .= toProto psBakerAddress
        ProtoFields.equityCapital .= toProto psBakerEquityCapital
        ProtoFields.delegatedCapital .= toProto psDelegatedCapital
        ProtoFields.delegatedCapitalCap .= toProto psDelegatedCapitalCap
        ProtoFields.poolInfo .= toProto psPoolInfo
        ProtoFields.maybe'equityPendingChange .= toProto psBakerStakePendingChange
        ProtoFields.maybe'currentPaydayInfo .= fmap toProto psCurrentPaydayStatus
        ProtoFields.allPoolTotalCapital .= toProto psAllPoolTotalCapital
    toProto QueryTypes.PassiveDelegationStatus{..} = Right $ Proto.make $ do
        ProtoFields.delegatedCapital .= toProto psDelegatedCapital
        ProtoFields.commissionRates .= toProto psCommissionRates
        ProtoFields.currentPaydayTransactionFeesEarned .= toProto psCurrentPaydayTransactionFeesEarned
        ProtoFields.currentPaydayDelegatedCapital .= toProto psCurrentPaydayDelegatedCapital
        ProtoFields.allPoolTotalCapital .= toProto psAllPoolTotalCapital

instance ToProto QueryTypes.PoolPendingChange where
    type Output QueryTypes.PoolPendingChange = Maybe Proto.PoolPendingChange
    toProto QueryTypes.PPCNoChange = Nothing
    toProto QueryTypes.PPCReduceBakerCapital{..} =
        Just $
            Proto.make $
                ProtoFields.reduce
                    .= Proto.make
                        ( do
                            ProtoFields.reducedEquityCapital .= toProto ppcBakerEquityCapital
                            ProtoFields.effectiveTime .= toProto ppcEffectiveTime
                        )
    toProto QueryTypes.PPCRemovePool{..} =
        Just $
            Proto.make $
                ProtoFields.remove
                    .= Proto.make
                        (ProtoFields.effectiveTime .= toProto ppcEffectiveTime)

instance ToProto QueryTypes.CurrentPaydayBakerPoolStatus where
    type Output QueryTypes.CurrentPaydayBakerPoolStatus = Proto.PoolCurrentPaydayInfo
    toProto QueryTypes.CurrentPaydayBakerPoolStatus{..} = Proto.make $ do
        ProtoFields.blocksBaked .= fromIntegral bpsBlocksBaked
        ProtoFields.finalizationLive .= bpsFinalizationLive
        ProtoFields.transactionFeesEarned .= toProto bpsTransactionFeesEarned
        ProtoFields.effectiveStake .= toProto bpsEffectiveStake
        ProtoFields.lotteryPower .= bpsLotteryPower
        ProtoFields.bakerEquityCapital .= toProto bpsBakerEquityCapital
        ProtoFields.delegatedCapital .= toProto bpsDelegatedCapital

instance ToProto QueryTypes.RewardStatus where
    type Output QueryTypes.RewardStatus = Proto.TokenomicsInfo
    toProto QueryTypes.RewardStatusV0{..} =
        Proto.make
            ( ProtoFields.v0
                .= Proto.make
                    ( do
                        ProtoFields.totalAmount .= toProto rsTotalAmount
                        ProtoFields.totalEncryptedAmount .= toProto rsTotalEncryptedAmount
                        ProtoFields.bakingRewardAccount .= toProto rsBakingRewardAccount
                        ProtoFields.finalizationRewardAccount .= toProto rsFinalizationRewardAccount
                        ProtoFields.gasAccount .= toProto rsGasAccount
                        ProtoFields.protocolVersion .= toProto rsProtocolVersion
                    )
            )
    toProto QueryTypes.RewardStatusV1{..} =
        Proto.make
            ( ProtoFields.v1
                .= Proto.make
                    ( do
                        ProtoFields.totalAmount .= toProto rsTotalAmount
                        ProtoFields.totalEncryptedAmount .= toProto rsTotalEncryptedAmount
                        ProtoFields.bakingRewardAccount .= toProto rsBakingRewardAccount
                        ProtoFields.finalizationRewardAccount .= toProto rsFinalizationRewardAccount
                        ProtoFields.gasAccount .= toProto rsGasAccount
                        ProtoFields.foundationTransactionRewards .= toProto rsFoundationTransactionRewards
                        ProtoFields.nextPaydayTime .= toProto rsNextPaydayTime
                        ProtoFields.nextPaydayMintRate .= toProto rsNextPaydayMintRate
                        ProtoFields.totalStakedCapital .= toProto rsTotalStakedCapital
                        ProtoFields.protocolVersion .= toProto rsProtocolVersion
                    )
            )

instance ToProto QueryTypes.Branch where
    type Output QueryTypes.Branch = Proto.Branch
    toProto QueryTypes.Branch{..} = Proto.make $ do
        ProtoFields.blockHash .= toProto branchBlockHash
        ProtoFields.children .= fmap toProto branchChildren

instance ToProto QueryTypes.BlockBirkParameters where
    type Output QueryTypes.BlockBirkParameters = Maybe Proto.ElectionInfo
    toProto QueryTypes.BlockBirkParameters{..} = do
        bakerElectionInfo <- mapM toProto (Vec.toList bbpBakers)
        Just $ Proto.make $ do
            ProtoFields.electionDifficulty .= toProto bbpElectionDifficulty
            ProtoFields.electionNonce .= mkSerialize bbpElectionNonce
            ProtoFields.bakerElectionInfo .= bakerElectionInfo

instance ToProto QueryTypes.BakerSummary where
    type Output QueryTypes.BakerSummary = Maybe Proto.ElectionInfo'Baker
    toProto QueryTypes.BakerSummary{..} = do
        bakerAccount <- bsBakerAccount
        Just $ Proto.make $ do
            ProtoFields.baker .= toProto bsBakerId
            ProtoFields.account .= toProto bakerAccount
            ProtoFields.lotteryPower .= bsBakerLotteryPower

instance ToProto Transactions.TransactionHeader where
    type Output Transactions.TransactionHeader = Proto.AccountTransactionHeader

    toProto Transactions.TransactionHeader{..} = Proto.make $ do
        ProtoFields.sender .= toProto thSender
        ProtoFields.sequenceNumber .= toProto thNonce
        ProtoFields.energyAmount .= toProto thEnergyAmount
        ProtoFields.expiry .= toProto thExpiry

instance ToProto Signature where
    type Output Signature = Proto.Signature

    toProto (Signature bss) = Proto.make $ do
        ProtoFields.value .= BSS.fromShort bss

instance ToProto Transactions.TransactionSignature where
    type Output Transactions.TransactionSignature = Proto.AccountTransactionSignature

    toProto Transactions.TransactionSignature{..} = Proto.make $ do
        ProtoFields.signatures .= (Map.fromAscList . map mk . Map.toAscList $ tsSignatures)
      where
        mk (k, s) = (fromIntegral k, mkSingleSig s)
        mkSingleSig sigs = Proto.make $ do
            ProtoFields.signatures .= (Map.fromAscList . map (\(ki, sig) -> (fromIntegral ki, toProto sig)) . Map.toAscList $ sigs)

instance ToProto Transactions.AccountTransaction where
    type Output Transactions.AccountTransaction = Proto.AccountTransaction

    toProto Transactions.AccountTransaction{..} = Proto.make $ do
        ProtoFields.signature .= toProto atrSignature
        ProtoFields.header .= toProto atrHeader
        ProtoFields.payload
            .= Proto.make
                ( ProtoFields.rawPayload .= BSS.fromShort (_spayload atrPayload)
                )

instance ToProto Transactions.AccountCreation where
    type Output Transactions.AccountCreation = Proto.CredentialDeployment

    toProto Transactions.AccountCreation{..} = Proto.make $ do
        ProtoFields.messageExpiry .= toProto messageExpiry
        ProtoFields.rawPayload .= S.encode credential

instance ToProto Updates.UpdateInstructionSignatures where
    type Output Updates.UpdateInstructionSignatures = Proto.SignatureMap

    toProto Updates.UpdateInstructionSignatures{..} = Proto.make $ do
        ProtoFields.signatures .= (Map.fromAscList . map mk . Map.toAscList $ signatures)
      where
        mk (k, s) = (fromIntegral k, toProto s)

instance ToProto Updates.UpdateHeader where
    type Output Updates.UpdateHeader = Proto.UpdateInstructionHeader

    toProto Updates.UpdateHeader{..} = Proto.make $ do
        -- since UpdateSequenceNumber is an alias for Nonce in Haskell, but not in
        -- the .proto file we have to use mkWord64 or similar, and not toProto since
        -- that one is defined for the Nonce.
        ProtoFields.sequenceNumber .= mkWord64 updateSeqNumber
        ProtoFields.effectiveTime .= toProto updateEffectiveTime
        ProtoFields.timeout .= toProto updateTimeout

instance ToProto Updates.UpdateInstruction where
    type Output Updates.UpdateInstruction = Proto.UpdateInstruction

    toProto Updates.UpdateInstruction{..} = Proto.make $ do
        ProtoFields.signatures .= toProto uiSignatures
        ProtoFields.header .= toProto uiHeader
        ProtoFields.payload
            .= Proto.make
                ( ProtoFields.rawPayload .= S.runPut (Updates.putUpdatePayload uiPayload)
                )

instance ToProto Transactions.BlockItem where
    type Output Transactions.BlockItem = Proto.BlockItem
    toProto bi = Proto.make $ do
        ProtoFields.hash .= toProto (Transactions.wmdHash bi)
        case Transactions.wmdData bi of
            Transactions.NormalTransaction accTx -> do
                ProtoFields.accountTransaction .= toProto accTx
            Transactions.CredentialDeployment cred ->
                ProtoFields.credentialDeployment .= toProto cred
            Transactions.ChainUpdate cu ->
                ProtoFields.updateInstruction .= toProto cu

instance ToProto TxTypes.AccountAmounts where
    type Output TxTypes.AccountAmounts = Proto.BlockSpecialEvent'AccountAmounts
    toProto TxTypes.AccountAmounts{..} = Proto.make $ ProtoFields.entries .= fmap mapper (Map.toList accountAmounts)
      where
        mapper (account, amount) = Proto.make $ do
            ProtoFields.account .= toProto account
            ProtoFields.amount .= toProto amount

instance ToProto TxTypes.SpecialTransactionOutcome where
    type Output TxTypes.SpecialTransactionOutcome = Proto.BlockSpecialEvent
    toProto TxTypes.BakingRewards{..} =
        Proto.make $
            ProtoFields.bakingRewards
                .= Proto.make
                    ( do
                        ProtoFields.bakerRewards .= toProto stoBakerRewards
                        ProtoFields.remainder .= toProto stoRemainder
                    )
    toProto TxTypes.Mint{..} =
        Proto.make $
            ProtoFields.mint
                .= Proto.make
                    ( do
                        ProtoFields.mintBakingReward .= toProto stoMintBakingReward
                        ProtoFields.mintFinalizationReward .= toProto stoMintFinalizationReward
                        ProtoFields.mintPlatformDevelopmentCharge .= toProto stoMintPlatformDevelopmentCharge
                        ProtoFields.foundationAccount .= toProto stoFoundationAccount
                    )
    toProto TxTypes.FinalizationRewards{..} =
        Proto.make $
            ProtoFields.finalizationRewards
                .= Proto.make
                    ( do
                        ProtoFields.finalizationRewards .= toProto stoFinalizationRewards
                        ProtoFields.remainder .= toProto stoRemainder
                    )
    toProto TxTypes.BlockReward{..} =
        Proto.make $
            ProtoFields.blockReward
                .= Proto.make
                    ( do
                        ProtoFields.transactionFees .= toProto stoTransactionFees
                        ProtoFields.oldGasAccount .= toProto stoOldGASAccount
                        ProtoFields.newGasAccount .= toProto stoNewGASAccount
                        ProtoFields.bakerReward .= toProto stoBakerReward
                        ProtoFields.foundationCharge .= toProto stoFoundationCharge
                        ProtoFields.baker .= toProto stoBaker
                        ProtoFields.foundationAccount .= toProto stoFoundationAccount
                    )
    toProto TxTypes.PaydayFoundationReward{..} =
        Proto.make $
            ProtoFields.paydayFoundationReward
                .= Proto.make
                    ( do
                        ProtoFields.foundationAccount .= toProto stoFoundationAccount
                        ProtoFields.developmentCharge .= toProto stoDevelopmentCharge
                    )
    toProto TxTypes.PaydayAccountReward{..} =
        Proto.make $
            ProtoFields.paydayAccountReward
                .= Proto.make
                    ( do
                        ProtoFields.account .= toProto stoAccount
                        ProtoFields.transactionFees .= toProto stoTransactionFees
                        ProtoFields.bakerReward .= toProto stoBakerReward
                        ProtoFields.finalizationReward .= toProto stoFinalizationReward
                    )
    toProto TxTypes.BlockAccrueReward{..} =
        Proto.make $
            ProtoFields.blockAccrueReward
                .= Proto.make
                    ( do
                        ProtoFields.transactionFees .= toProto stoTransactionFees
                        ProtoFields.oldGasAccount .= toProto stoOldGASAccount
                        ProtoFields.newGasAccount .= toProto stoNewGASAccount
                        ProtoFields.bakerReward .= toProto stoBakerReward
                        ProtoFields.passiveReward .= toProto stoPassiveReward
                        ProtoFields.foundationCharge .= toProto stoFoundationCharge
                        ProtoFields.baker .= toProto stoBakerId
                    )
    toProto TxTypes.PaydayPoolReward{..} =
        Proto.make $
            ProtoFields.paydayPoolReward
                .= Proto.make
                    ( do
                        ProtoFields.maybe'poolOwner .= fmap toProto stoPoolOwner
                        ProtoFields.transactionFees .= toProto stoTransactionFees
                        ProtoFields.bakerReward .= toProto stoBakerReward
                        ProtoFields.finalizationReward .= toProto stoFinalizationReward
                    )

instance ToProto (TransactionTime, QueryTypes.PendingUpdateEffect) where
    type Output (TransactionTime, QueryTypes.PendingUpdateEffect) = Proto.PendingUpdate
    toProto (time, effect) = Proto.make $ do
        ProtoFields.effectiveTime .= toProto time
        case effect of
            QueryTypes.PUERootKeys keys -> ProtoFields.rootKeys .= toProto keys
            QueryTypes.PUELevel1Keys keys -> ProtoFields.level1Keys .= toProto keys
            QueryTypes.PUELevel2KeysV0 auth -> ProtoFields.level2KeysCpv0 .= toProto auth
            QueryTypes.PUELevel2KeysV1 auth -> ProtoFields.level2KeysCpv1 .= toProto auth
            QueryTypes.PUEProtocol protocolUpdate -> ProtoFields.protocol .= toProto protocolUpdate
            QueryTypes.PUEElectionDifficulty electionDifficulty -> ProtoFields.electionDifficulty .= toProto electionDifficulty
            QueryTypes.PUEEuroPerEnergy euroPerEnergy -> ProtoFields.euroPerEnergy .= toProto euroPerEnergy
            QueryTypes.PUEMicroCCDPerEuro microCcdPerEuro -> ProtoFields.microCcdPerEuro .= toProto microCcdPerEuro
            QueryTypes.PUEFoundationAccount foundationAccount -> ProtoFields.foundationAccount .= toProto foundationAccount
            QueryTypes.PUEMintDistributionV0 mintDistributionCpv0 -> ProtoFields.mintDistributionCpv0 .= toProto mintDistributionCpv0
            QueryTypes.PUEMintDistributionV1 mintDistributionCpv1 -> ProtoFields.mintDistributionCpv1 .= toProto mintDistributionCpv1
            QueryTypes.PUETransactionFeeDistribution transactionFeeDistribution -> ProtoFields.transactionFeeDistribution .= toProto transactionFeeDistribution
            QueryTypes.PUEGASRewards gasRewards -> ProtoFields.gasRewards .= toProto gasRewards
            QueryTypes.PUEPoolParametersV0 poolParametersCpv0 -> ProtoFields.poolParametersCpv0 .= toProto poolParametersCpv0
            QueryTypes.PUEPoolParametersV1 poolParametersCpv1 -> ProtoFields.poolParametersCpv1 .= toProto poolParametersCpv1
            QueryTypes.PUEAddAnonymityRevoker addAnonymityRevoker -> ProtoFields.addAnonymityRevoker .= toProto addAnonymityRevoker
            QueryTypes.PUEAddIdentityProvider addIdentityProvider -> ProtoFields.addIdentityProvider .= toProto addIdentityProvider
            QueryTypes.PUECooldownParameters cooldownParameters -> ProtoFields.cooldownParameters .= toProto cooldownParameters
            QueryTypes.PUETimeParameters timeParameters -> ProtoFields.timeParameters .= toProto timeParameters

instance ToProto QueryTypes.NextUpdateSequenceNumbers where
    type Output QueryTypes.NextUpdateSequenceNumbers = Proto.NextUpdateSequenceNumbers
    toProto QueryTypes.NextUpdateSequenceNumbers{..} = Proto.make $ do
        ProtoFields.rootKeys .= toProto _nusnRootKeys
        ProtoFields.level1Keys .= toProto _nusnLevel1Keys
        ProtoFields.level2Keys .= toProto _nusnLevel2Keys
        ProtoFields.protocol .= toProto _nusnProtocol
        ProtoFields.electionDifficulty .= toProto _nusnElectionDifficulty
        ProtoFields.euroPerEnergy .= toProto _nusnEuroPerEnergy
        ProtoFields.microCcdPerEuro .= toProto _nusnMicroCCDPerEuro
        ProtoFields.foundationAccount .= toProto _nusnFoundationAccount
        ProtoFields.mintDistribution .= toProto _nusnMintDistribution
        ProtoFields.transactionFeeDistribution .= toProto _nusnTransactionFeeDistribution
        ProtoFields.gasRewards .= toProto _nusnGASRewards
        ProtoFields.poolParameters .= toProto _nusnPoolParameters
        ProtoFields.addAnonymityRevoker .= toProto _nusnAddAnonymityRevoker
        ProtoFields.addIdentityProvider .= toProto _nusnAddIdentityProvider
        ProtoFields.cooldownParameters .= toProto _nusnCooldownParameters
        ProtoFields.timeParameters .= toProto _nusnTimeParameters

instance ToProto Epoch where
    type Output Epoch = Proto.Epoch
    toProto = mkWord64

instance ToProto CredentialsPerBlockLimit where
    type Output CredentialsPerBlockLimit = Proto.CredentialsPerBlockLimit
    toProto = mkWord16

instance ToProto FinalizationIndex where
    type Output FinalizationIndex = Proto.FinalizationIndex

    toProto = mkWord64

instance ToProto QueryTypes.FinalizationSummaryParty where
    type Output QueryTypes.FinalizationSummaryParty = Proto.FinalizationSummaryParty

    toProto QueryTypes.FinalizationSummaryParty{..} = Proto.make $ do
        ProtoFields.baker .= toProto fspBakerId
        ProtoFields.weight .= fromIntegral fspWeight
        ProtoFields.signed .= fspSigned

-- | Decode an account address from a foreign ptr. Assumes 32 bytes are available.
decodeAccountAddress :: Ptr Word8 -> IO AccountAddress
decodeAccountAddress accPtr = coerce <$> FBS.create @AccountAddressSize (\p -> copyBytes p accPtr 32)

-- |NB: Assumes the data is at least 32 bytes.
decodeTransactionHashInput :: Ptr Word8 -> IO TransactionHash
decodeTransactionHashInput hsh = coerce <$> FBS.create @DigestSize (\p -> copyBytes p hsh 32)

-- | Decode an account address from a foreign ptr.
decodeAccountIdentifierInput :: Word8 -> Ptr Word8 -> IO AccountIdentifier
decodeAccountIdentifierInput 0 dta = AccAddress <$> decodeAccountAddress dta
decodeAccountIdentifierInput 1 dta = do
    bs <- BS.unsafePackCStringLen (castPtr dta, 48)
    case S.decode bs of
        Left err -> error $ "Precondition violation in FFI call: " ++ err
        Right cid -> return (CredRegID cid)
decodeAccountIdentifierInput 2 dta = AccIndex . AccountIndex <$> peek (castPtr dta)
decodeAccountIdentifierInput n _ = error $ "Unknown account identifier tag: " ++ show n

decodeModuleRefInput :: Ptr Word8 -> IO ModuleRef
decodeModuleRefInput modRef = coerce <$> FBS.create @DigestSize (\p -> copyBytes p modRef 32)

{- |NB: Assumes the data is valid utf8. The caller is expected to guarantee
 this.
-}
decodeText :: Ptr Word8 -> Word32 -> IO Text
decodeText ptr len = Text.decodeUtf8 <$> BS.packCStringLen (castPtr ptr, fromIntegral len)

{- |NB: Assumes the data is valid utf8. Protobuf guarantees this, and Rust/tonic
 does actually implement the validation, so this is safe.
-}
decodeReceiveName :: Ptr Word8 -> Word32 -> IO Wasm.ReceiveName
decodeReceiveName ptr len = Wasm.ReceiveName <$> decodeText ptr len

-- |The result type of a gRPC2 query.
data QueryResult
    = -- | An invalid argument was provided by the client.
      QRInvalidArgument
    | -- | An internal error occured.
      QRInternalError
    | -- | The query succeeded.
      QRSuccess
    | -- | The requested data could not be found.
      QRNotFound

-- |Convert a QueryResult to a result code.
queryResultCode :: QueryResult -> Int64
queryResultCode QRInvalidArgument = -2
queryResultCode QRInternalError = -1
queryResultCode QRSuccess = 0
queryResultCode QRNotFound = 1

-- |If the message is given encode and write it using the provided callback.
returnMessage ::
    (Proto.Message (Output a), ToProto a) =>
    (Ptr Word8 -> Int64 -> IO ()) ->
    -- | The potential message.
    Maybe a ->
    IO Int64
returnMessage copier res = case res of
    Nothing -> return $ queryResultCode QRNotFound
    Just v -> do
        let encoded = Proto.encodeMessage (toProto v)
        BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> copier (castPtr ptr) (fromIntegral len))
        return $ queryResultCode QRSuccess

enqueueMessages :: (Proto.Message (Output a), ToProto a) => (Ptr Word8 -> Int64 -> IO Int32) -> [a] -> IO ThreadId
enqueueMessages callback = enqueueProtoMessages callback . fmap toProto

{- |Spawn a new thread that will invoke the provided callback on the list of
 encoded messages. If the callback response indicates that the channel to
 which the callback is enqueueing is full, the thread will wait. The wait time
 follows exponential backoff strategy to a maximum of 10 seconds.
-}
enqueueProtoMessages :: (Proto.Message a) => (Ptr Word8 -> Int64 -> IO Int32) -> [a] -> IO ThreadId
enqueueProtoMessages callback = forkIO . go 0 . map Proto.encodeMessage
  where
    go _ [] = () <$ callback nullPtr maxBound -- close the sender channel.
    go n msgs@(msg : msgs') =
        BS.unsafeUseAsCStringLen msg $ \(headPtr, len) -> do
            res <- callback (castPtr headPtr) (fromIntegral len)
            if res == 0
                then go 0 msgs' -- reset wait time to 10ms.
                else
                    if res == -1
                        then do
                            let delay = (2 ^ n) * 10_000
                            threadDelay delay
                            go (min (n + 1) (10 :: Int)) msgs -- maximum delay is 10 seconds
                        else return () -- the sender channel is now dropped, so we stop.

-- V2 stuff starts here:

-- |A block hash input
data BlockHashInput = Best | LastFinal | Given BlockHash

-- |An account identifier input
type AccountIdentifierInput = AccountIdentifier

instance FromProto Proto.AccountIndex where
    type Output' Proto.AccountIndex = AccountIndex
    fromProto = return . AccountIndex . deMkWord64
instance FromProto Proto.AccountAddress where
    type Output' Proto.AccountAddress = AccountAddress
    fromProto = deMkSerialize

instance FromProto Proto.CredentialRegistrationId where
    type Output' Proto.CredentialRegistrationId = RawCredentialRegistrationID
    fromProto = deMkSerialize

instance FromProto Proto.AccountIdentifierInput where
    type Output' Proto.AccountIdentifierInput = AccountIdentifierInput
    fromProto a = do
        aii <- a ^. ProtoFields.maybe'accountIdentifierInput
        case aii of
            Proto.AccountIdentifierInput'AccountIndex accIdx -> AccIndex <$> fromProto accIdx
            Proto.AccountIdentifierInput'Address addr -> AccAddress <$> fromProto addr
            Proto.AccountIdentifierInput'CredId cId -> CredRegID <$> fromProto cId

instance ToProto AccountIdentifierInput where
    type Output AccountIdentifierInput = Proto.AccountIdentifierInput
    toProto = \case
        CredRegID cred -> Proto.make $ ProtoFields.credId .= toProto cred
        AccAddress addr -> Proto.make $ ProtoFields.address .= toProto addr
        AccIndex accIdx -> Proto.make $ ProtoFields.accountIndex .= toProto accIdx

instance FromProto Proto.SequenceNumber where
    type Output' Proto.SequenceNumber = Nonce
    fromProto = return . Nonce . deMkWord64

instance FromProto Proto.ReleaseSchedule where
    type Output' Proto.ReleaseSchedule = AccountReleaseSummary
    fromProto ars = do
        releaseTotal <- fromProto =<< ars ^? ProtoFields.total
        releaseSchedule <- do 
            schedules <- ars ^? ProtoFields.schedules
            mapM fromProto schedules
        return AccountReleaseSummary {..}

instance FromProto Proto.Timestamp where
    type Output' Proto.Timestamp = (Timestamp, UTCTime)
    fromProto t = do
        time <- fromIntegral <$> t ^? ProtoFields.value
        let utcTime = posixSecondsToUTCTime (fromIntegral time / 1_000.0)
        return (Timestamp time, utcTime)

instance FromProto Proto.TransactionHash where
    type Output' Proto.TransactionHash = TransactionHashV0
    fromProto = deMkSerialize

instance FromProto Proto.Release where
    type Output' Proto.Release = ScheduledRelease
    fromProto r = do
        releaseAmount <- fromProto =<< r ^? ProtoFields.amount
        releaseTimestamp <- do
            time <- r ^? ProtoFields.timestamp
            (ts, _) <- fromProto time
            return ts
        releaseTransactions <- do
            ts <- r ^? ProtoFields.transactions
            mapM fromProto ts
        return ScheduledRelease {..}

instance FromProto Proto.AccountThreshold where
    type Output' Proto.AccountThreshold = AccountThreshold
    fromProto = return . AccountThreshold . deMkWord8

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
        _aggregatedAmount <- do
            aa <- fromProto =<< eb ^? ProtoFields.aggregatedAmount
            na <- eb ^? ProtoFields.numAggregated
            return $ return (aa, na)
        let _incomingEncryptedAmounts = Empty -- FIXME: Field is not included in other direction. Should it be?
        return AccountEncryptedAmount {..}

instance FromProto Proto.EncryptionKey where
    type Output' Proto.EncryptionKey = AccountEncryptionKey
    fromProto = deMkSerialize

instance FromProto Proto.BakerId where
    type Output' Proto.BakerId = BakerId
    fromProto = return . BakerId . AccountIndex . deMkWord64

instance FromProto Proto.DelegationTarget where
    type Output' Proto.DelegationTarget = DelegationTarget
    fromProto dTarget = do
        dt <- dTarget ^. Proto.maybe'target
        case dt of
            Proto.DelegationTarget'Passive _ -> return DelegatePassive
            Proto.DelegationTarget'Baker baker -> DelegateToBaker <$> fromProto baker

instance FromProto Proto.StakePendingChange where
    type Output' Proto.StakePendingChange = (StakePendingChange' Timestamp, StakePendingChange' UTCTime)
    fromProto pc = do
        spc <- pc ^. Proto.maybe'change
        case spc of
            Proto.StakePendingChange'Reduce' reduce -> do
                ns <- fromProto =<< reduce ^? ProtoFields.newStake
                et <- fromProto =<< reduce ^? ProtoFields.effectiveTime
                return $ bimap (ReduceStake ns) (ReduceStake ns) et
            Proto.StakePendingChange'Remove remove -> do
                rm <- fromProto remove
                return $ bimap RemoveStake RemoveStake rm

instance FromProto Proto.BakerInfo where
    type Output' Proto.BakerInfo = BakerInfo
    fromProto bInfo = do
        _bakerIdentity <- fromProto =<< bInfo ^? ProtoFields.bakerId
        _bakerElectionVerifyKey <- fromProto =<< bInfo ^? ProtoFields.electionKey
        _bakerSignatureVerifyKey <- fromProto =<< bInfo ^? ProtoFields.signatureKey
        _bakerAggregationVerifyKey <- fromProto =<< bInfo ^? ProtoFields.aggregationKey
        return BakerInfo {..}

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
        Proto.OPEN_STATUS_OPEN_FOR_ALL ->
            return OpenForAll
        Proto.OPEN_STATUS_CLOSED_FOR_NEW ->
            return ClosedForNew
        Proto.OPEN_STATUS_CLOSED_FOR_ALL ->
            return ClosedForAll
        _ -> fromProtoError

instance FromProto Text where
    type Output' Text = UrlText
    fromProto = return . UrlText

instance FromProto Proto.BakerPoolInfo where
    type Output' Proto.BakerPoolInfo = BakerPoolInfo
    fromProto poolInfo = do
        _poolOpenStatus <- fromProto =<< poolInfo ^? ProtoFields.openStatus
        _poolMetadataUrl <- fromProto =<< poolInfo ^? ProtoFields.url
        _poolCommissionRates <- fromProto =<< poolInfo ^? ProtoFields.commissionRates
        return BakerPoolInfo {..}

instance FromProto Proto.AmountFraction where
    type Output' Proto.AmountFraction = AmountFraction
    fromProto amountFraction = makeAmountFraction <$> amountFraction ^? ProtoFields.partsPerHundredThousand

instance FromProto Proto.CommissionRates where
    type Output' Proto.CommissionRates = CommissionRates
    fromProto commissionRates = do
        _finalizationCommission <- fromProto =<< commissionRates ^? ProtoFields.finalization
        _bakingCommission <- fromProto =<< commissionRates ^? ProtoFields.baking
        _transactionCommission <- fromProto =<< commissionRates ^? ProtoFields.transaction
        return CommissionRates {..}

instance FromProto Proto.AccountStakingInfo where
    type Output' Proto.AccountStakingInfo = AccountStakingInfo
    fromProto si = do
        asi <- si ^. Proto.maybe'stakingInfo
        case asi of
            Proto.AccountStakingInfo'Delegator' delegator -> do
                asiStakedAmount <- fromProto =<< delegator ^? ProtoFields.stakedAmount
                asiStakeEarnings <- delegator ^? ProtoFields.restakeEarnings
                asiDelegationTarget <- fromProto =<< delegator ^? ProtoFields.target
                asiDelegationPendingChange <- do
                    ch <- fromProto =<< delegator ^? ProtoFields.pendingChange
                    return $ snd ch
                return AccountStakingDelegated {..}
            Proto.AccountStakingInfo'Baker' baker -> do
                asiStakedAmount <- fromProto =<< baker ^? ProtoFields.stakedAmount
                asiStakeEarnings <- baker ^? ProtoFields.restakeEarnings
                asiBakerInfo <- fromProto =<< baker ^? ProtoFields.bakerInfo
                asiPendingChange <- do
                    pc <- fromProto =<< baker ^? ProtoFields.pendingChange
                    return $ snd pc
                asiPoolInfo <- fromProto <$> baker ^? ProtoFields.poolInfo
                return AccountStakingBaker  {..}

instance FromProto Proto.ArThreshold where
    type Output' Proto.ArThreshold = Threshold
    fromProto = return . Threshold . deMkWord8

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
        return CredentialDeploymentCommitments {..}
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
                icdvRegId <- fromProto =<< initial ^? ProtoFields.credId
                icdvIpId <- fromProto =<< initial ^? ProtoFields.ipId
                icdvPolicy <- fromProto =<< initial ^? ProtoFields.policy
                return $ InitialAC InitialCredentialDeploymentValues {..}
            Proto.AccountCredential'Normal normal -> do
                cdvPublicKeys <- fromProto =<< normal ^? ProtoFields.keys
                cdvCredId <- fromProto =<< normal ^? ProtoFields.credId
                cdvIpId <- fromProto =<< normal ^? ProtoFields.ipId
                cdvPolicy <- fromProto =<< normal ^? ProtoFields.policy
                cdvThreshold <- fromProto =<< normal ^? ProtoFields.arThreshold
                cdvArData <- fromProto =<< normal ^? ProtoFields.arData
                commitments <- fromProto =<< normal ^? ProtoFields.commitments
                return $ NormalAC CredentialDeploymentValues {..} commitments

instance FromProto Proto.YearMonth where
    type Output' Proto.YearMonth = YearMonth
    fromProto yearMonth = do
        ymYear <- fromIntegral <$> yearMonth ^? ProtoFields.year
        ymMonth <- fromIntegral <$> yearMonth ^? ProtoFields.month
        return YearMonth {..}

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
        return Policy {..}
      where
        versionTag = 0 -- FIXME: Which version?
        convert (aTag, aVal) = do
            val <- case S.runGet (S.getShortByteString versionTag) aVal of
                     Left _ -> Nothing
                     Right v -> Just v
            return (AttributeTag $ fromIntegral aTag , AttributeValue val)

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
            SignatureThreshold
            . deMkWord8
            <$> cpk ^? ProtoFields.threshold
        return CredentialPublicKeys {..}
      where
        convert (ki, pKey) = do
            key <- pKey ^? ProtoFields.ed25519Key
            k <- case S.decode key of
                   Left _ -> Nothing
                   Right k -> return k
            return (fromIntegral ki, VerifyKeyEd25519 k)

instance FromProto Proto.AccountInfo where
    type Output' Proto.AccountInfo = AccountInfo
    fromProto ai = do
        aiAccountNonce <- fromProto =<< ai ^? ProtoFields.sequenceNumber
        aiAccountAmount <- fromProto =<< ai ^? ProtoFields.amount
        aiAccountReleaseSchedule <- fromProto =<< ai ^? ProtoFields.schedule
        aiAccountCredentials <- do 
                fmap Map.fromAscList
                . mapM convert
                . Map.toAscList
                =<< ai ^? ProtoFields.creds
        aiAccountThreshold <- fromProto =<< ai ^? ProtoFields.threshold
        aiAccountEncryptedAmount <- fromProto =<< ai ^? ProtoFields.encryptedBalance
        aiAccountEncryptionKey <- fromProto =<< ai ^? ProtoFields.encryptionKey
        aiAccountIndex <- fromProto =<< ai ^? ProtoFields.index
        aiStakingInfo <- fromProto =<< ai ^. ProtoFields.maybe'stake
        aiAccountAddress <- fromProto =<< ai ^? ProtoFields.address
        return AccountInfo {..}
      where
        versionTag = 0
        convert (key, val) = do
            v <- fromProto val
            return (CredentialIndex $ fromIntegral key, Versioned versionTag v)

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
        modRef <- mr ^? ProtoFields.value
        let moduleRef = Hash $ FBS.fromByteString modRef
        return ModuleRef {..}

instance FromProto Proto.VersionedModuleSource where
    type Output' Proto.VersionedModuleSource = Wasm.WasmModule
    fromProto vmSource = do
        vms <- vmSource ^. Proto.maybe'module'
        case vms of
            Proto.VersionedModuleSource'V0 v0 -> Wasm.WasmModuleV0 <$> deMkSerialize v0
            Proto.VersionedModuleSource'V1 v1 -> Wasm.WasmModuleV0 <$> deMkSerialize v1

instance FromProto Proto.ContractStateV0 where
    type Output' Proto.ContractStateV0 = Wasm.ContractState
    fromProto cs = do Wasm.ContractState <$> cs ^? ProtoFields.value
instance FromProto Proto.ReceiveName where
    type Output' Proto.ReceiveName = Wasm.ReceiveName
    fromProto name = Wasm.ReceiveName <$> name ^? ProtoFields.value

instance FromProto Proto.InitName where
    type Output' Proto.InitName = Wasm.InitName
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
                return Wasm.InstanceInfoV0 {..}
            Proto.InstanceInfo'V1' v1  -> do
                iiOwner <- fromProto =<< v1 ^? ProtoFields.owner
                iiAmount <- fromProto =<< v1 ^? ProtoFields.amount
                iiMethods <- do
                    methods <- v1 ^? ProtoFields.methods
                    Set.fromList <$> mapM fromProto methods
                iiName <- fromProto =<< v1 ^? ProtoFields.name
                iiSourceModule <- fromProto =<< v1 ^? ProtoFields.sourceModule
                return Wasm.InstanceInfoV1 {..}

instance FromProto Proto.NextAccountSequenceNumber where
    type Output' Proto.NextAccountSequenceNumber = QueryTypes.NextAccountNonce
    fromProto asn = do
        nanNonce <- fromProto =<< asn ^? ProtoFields.sequenceNumber
        nanAllFinal <- asn ^? ProtoFields.allFinal
        return QueryTypes.NextAccountNonce {..}

instance FromProto Proto.ProtocolVersion where
    type Output' Proto.ProtocolVersion = ProtocolVersion
    fromProto = \case
        Proto.PROTOCOL_VERSION_1 -> return P1
        Proto.PROTOCOL_VERSION_2 -> return P2
        Proto.PROTOCOL_VERSION_3 -> return P3
        Proto.PROTOCOL_VERSION_4 -> return P4
        Proto.PROTOCOL_VERSION_5 -> return P5
        Proto.PROTOCOL_VERSION_6 -> return P6
        Proto.ProtocolVersion'Unrecognized _ -> fromProtoError

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
        csGenesisTime <- do
            gt <- fromProto =<< ci ^? ProtoFields.genesisTime
            return $ snd gt
        csSlotDuration <- fromProto =<< ci ^? ProtoFields.slotDuration
        csEpochDuration <- fromProto =<< ci ^? ProtoFields.epochDuration
        csLastFinalizedBlock <- fromProto =<< ci ^? ProtoFields.lastFinalizedBlock
        csBestBlockHeight <- fromProto =<< ci ^? ProtoFields.bestBlockHeight
        csLastFinalizedBlockHeight <- fromProto =<< ci ^? ProtoFields.lastFinalizedBlockHeight
        csBlocksReceivedCount <- fromIntegral <$> ci ^? ProtoFields.blocksReceivedCount
        csBlocksVerifiedCount <- fromIntegral <$> ci ^? ProtoFields.blocksVerifiedCount
        let csBlockLastReceivedTime = fmap snd $ fromProto =<< ci ^. ProtoFields.maybe'blockLastReceivedTime
        csBlockReceiveLatencyEMA <- ci ^? ProtoFields.blockReceiveLatencyEma
        csBlockReceiveLatencyEMSD <- ci ^? ProtoFields.blockReceiveLatencyEmsd
        csBlockReceivePeriodEMA <- ci ^? ProtoFields.maybe'blockReceivePeriodEma
        csBlockReceivePeriodEMSD <- ci ^? ProtoFields.maybe'blockReceivePeriodEmsd
        let csBlockLastArrivedTime = fmap snd $ fromProto =<< ci ^. ProtoFields.maybe'blockLastArrivedTime
        csBlockArriveLatencyEMA <- ci ^? ProtoFields.blockArriveLatencyEma
        csBlockArriveLatencyEMSD <- ci ^? ProtoFields.blockArriveLatencyEmsd
        csBlockArrivePeriodEMA <- ci ^? ProtoFields.maybe'blockArrivePeriodEma
        csBlockArrivePeriodEMSD <- ci ^? ProtoFields.maybe'blockArrivePeriodEmsd
        csTransactionsPerBlockEMA <- ci ^? ProtoFields.transactionsPerBlockEma
        csTransactionsPerBlockEMSD <- ci ^? ProtoFields.transactionsPerBlockEmsd
        csFinalizationCount <- fromIntegral <$> ci ^? ProtoFields.finalizationCount
        let csLastFinalizedTime = fmap snd $ fromProto =<< ci ^. ProtoFields.maybe'lastFinalizedTime
        csFinalizationPeriodEMA <- ci ^? ProtoFields.maybe'finalizationPeriodEma
        csFinalizationPeriodEMSD <- ci ^? ProtoFields.maybe'finalizationPeriodEmsd
        csProtocolVersion <- fromProto =<< ci ^? ProtoFields.protocolVersion
        csGenesisIndex <- fromProto =<< ci ^? ProtoFields.genesisIndex
        csCurrentEraGenesisBlock <- fromProto =<< ci ^? ProtoFields.currentEraGenesisBlock
        csCurrentEraGenesisTime <- fmap snd $ fromProto =<< ci ^? ProtoFields.currentEraGenesisTime
        return QueryTypes.ConsensusStatus {..}

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
        biBlockReceiveTime <- fmap snd $ fromProto =<< bi ^? ProtoFields.receiveTime
        biBlockArriveTime <- fmap snd $ fromProto =<< bi ^? ProtoFields.arriveTime
        biBlockSlot <- fromProto =<< bi ^? ProtoFields.slotNumber
        biBlockSlotTime <- fmap snd $ fromProto =<< bi ^? ProtoFields.slotTime
        biBlockBaker <- fmap fromProto =<< bi ^? ProtoFields.maybe'baker
        biFinalized <- bi ^? ProtoFields.finalized
        biTransactionCount <- fromIntegral <$> bi ^? ProtoFields.transactionCount
        biTransactionEnergyCost <- fromProto =<< bi ^? ProtoFields.transactionsEnergyCost
        biTransactionsSize <- fromIntegral <$> bi ^? ProtoFields.transactionsSize
        biBlockStateHash <- fromProto =<< bi ^? ProtoFields.stateHash
        return QueryTypes.BlockInfo {..}

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
        return QueryTypes.CurrentPaydayBakerPoolStatus {..}

instance FromProto Proto.PoolInfoResponse where
    type Output' Proto.PoolInfoResponse = QueryTypes.PoolStatus
    fromProto pir = do
        psBakerId <- fromProto =<< pir ^? ProtoFields.baker
        psBakerAddress <- fromProto =<< pir ^? ProtoFields.address
        psBakerEquityCapital <- fromProto =<< pir ^? ProtoFields.equityCapital
        psDelegatedCapital <- fromProto =<< pir ^? ProtoFields.delegatedCapital
        psDelegatedCapitalCap <- fromProto =<< pir ^? ProtoFields.delegatedCapitalCap
        psPoolInfo <- fromProto =<< pir ^? ProtoFields.poolInfo
        psBakerStakePendingChange <- fromJust $ fmap fromProto =<<pir ^? ProtoFields.maybe'equityPendingChange
        psCurrentPaydayStatus <- fmap fromProto =<< pir ^? ProtoFields.maybe'currentPaydayInfo
        psAllPoolTotalCapital <- fromProto =<< pir ^? ProtoFields.allPoolTotalCapital
        return QueryTypes.BakerPoolStatus {..}

instance FromProto Proto.PassiveDelegationInfo where
    type Output' Proto.PassiveDelegationInfo = QueryTypes.PoolStatus
    fromProto pdi = do
        psDelegatedCapital <- fromProto =<< pdi ^? ProtoFields.delegatedCapital
        psCommissionRates <- fromProto =<< pdi ^? ProtoFields.commissionRates
        psCurrentPaydayTransactionFeesEarned <- fromProto =<< pdi ^? ProtoFields.currentPaydayTransactionFeesEarned
        psCurrentPaydayDelegatedCapital <- fromProto =<< pdi ^? ProtoFields.currentPaydayDelegatedCapital
        psAllPoolTotalCapital <- fromProto =<< pdi ^? ProtoFields.allPoolTotalCapital
        return QueryTypes.PassiveDelegationStatus {..}   

instance FromProto (Maybe Proto.PoolPendingChange) where
    type Output' (Maybe Proto.PoolPendingChange) = QueryTypes.PoolPendingChange
    fromProto = maybe (return QueryTypes.PPCNoChange) fromProto

instance FromProto Proto.PoolPendingChange where
    type Output' Proto.PoolPendingChange = QueryTypes.PoolPendingChange
    fromProto ppChange = do
        ppc <- ppChange ^. Proto.maybe'change
        case ppc of
            Proto.PoolPendingChange'Reduce' reduce -> do
                ppcBakerEquityCapital <- fromProto =<< reduce ^? ProtoFields.reducedEquityCapital
                ppcEffectiveTime <- fmap snd $ fromProto =<< reduce ^? ProtoFields.effectiveTime
                return QueryTypes.PPCReduceBakerCapital {..}
            Proto.PoolPendingChange'Remove' remove -> do
                ppcEffectiveTime <- fmap snd $ fromProto =<< remove ^? ProtoFields.effectiveTime
                return QueryTypes.PPCRemovePool {..}

instance FromProto Proto.BlocksAtHeightResponse where
    type Output' Proto.BlocksAtHeightResponse = [BlockHash]
    fromProto bahr = mapM fromProto =<< bahr ^? ProtoFields.blocks

data BlockHeightInput
    = Relative
        { rGenesisIndex :: GenesisIndex
        , rBlockHeight :: BlockHeight
        , rRestrict :: Bool
        }
    | Absolute
        { aBlockHeight :: AbsoluteBlockHeight
        }

instance ToProto BlockHeightInput where
    type Output BlockHeightInput = Proto.BlocksAtHeightRequest
    toProto Relative {..} =
        Proto.make $
          ProtoFields.relative .= Proto.make ( do
            ProtoFields.genesisIndex .= toProto rGenesisIndex
            ProtoFields.height .= toProto rBlockHeight
            ProtoFields.restrict .= rRestrict )
    toProto Absolute {..} =
        Proto.make $
            ProtoFields.absolute .= Proto.make (ProtoFields.height .= toProto aBlockHeight)

instance FromProto Proto.MintRate where
    type Output' Proto.MintRate = MintRate
    fromProto mr = do
        mrMantissa <- mr ^? ProtoFields.mantissa
        mrExponent <- fromIntegral <$> mr ^? ProtoFields.exponent
        return MintRate {..}

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
                return QueryTypes.RewardStatusV0 {..}
            Proto.TokenomicsInfo'V1' v1 -> do
                rsTotalAmount <- fromProto =<< v1 ^? ProtoFields.totalAmount
                rsTotalEncryptedAmount <- fromProto =<< v1 ^? ProtoFields.totalEncryptedAmount
                rsBakingRewardAccount <- fromProto =<< v1 ^? ProtoFields.bakingRewardAccount
                rsFinalizationRewardAccount <- fromProto =<< v1 ^? ProtoFields.finalizationRewardAccount
                rsGasAccount <- fromProto =<< v1 ^? ProtoFields.gasAccount
                rsFoundationTransactionRewards <- fromProto =<< v1 ^? ProtoFields.foundationTransactionRewards
                rsNextPaydayTime <- fmap snd $ fromProto =<< v1 ^? ProtoFields.nextPaydayTime
                rsNextPaydayMintRate <- fromProto =<< v1 ^? ProtoFields.nextPaydayMintRate
                rsTotalStakedCapital <- fromProto =<< v1 ^? ProtoFields.totalStakedCapital
                rsProtocolVersion <- fromProto =<< v1 ^? ProtoFields.protocolVersion
                return QueryTypes.RewardStatusV1 {..}

data InvokeInstanceInput = InvokeInstanceInput
    { iiBlockHash :: BlockHashInput
    , iiInvoker :: Maybe Address
    , iiInstance :: ContractAddress
    , iiAmount :: Amount
    , iiEntrypoint :: Wasm.ReceiveName
    , iiParameter :: Wasm.Parameter
    , iiEnergy :: Energy
    }

instance FromProto Proto.ContractEvent where
    type Output' Proto.ContractEvent = Wasm.ContractEvent
    fromProto ce = return . Wasm.ContractEvent . BSS.toShort $ ce ^. ProtoFields.value


instance FromProto Proto.Parameter where
    type Output' Proto.Parameter = Wasm.Parameter
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
        return ContractAddress {..}

instance FromProto Proto.ContractVersion where
    type Output' Proto.ContractVersion = Wasm.WasmVersion
    fromProto cv = case cv of
        Proto.V0 -> return Wasm.V0
        Proto.V1 -> return Wasm.V1
        Proto.ContractVersion'Unrecognized _ -> fromProtoError

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
                return Updated {..}
            Proto.ContractTraceElement'Transferred' transferred -> do
                etFrom <- fmap AddressContract $ fromProto =<< transferred ^? ProtoFields.sender
                etAmount <- fromProto =<< transferred ^? ProtoFields.amount
                etTo <- fmap AddressAccount $ fromProto =<< transferred ^? ProtoFields.receiver
                return Transferred{..}
            Proto.ContractTraceElement'Interrupted' interrupted -> do
                iAddress <- fromProto =<< interrupted ^? ProtoFields.address
                iEvents <- mapM fromProto =<< interrupted ^? ProtoFields.events        
                return Interrupted {..}
            Proto.ContractTraceElement'Resumed' resumed -> do
                rAddress <- fromProto =<< resumed ^? ProtoFields.address
                rSuccess <- resumed ^? ProtoFields.success        
                return Resumed {..}
            Proto.ContractTraceElement'Upgraded' upgraded -> do
                euAddress <- fromProto =<< upgraded ^? ProtoFields.address
                euFrom <- fromProto =<< upgraded ^? ProtoFields.from
                euTo <- fromProto =<< upgraded ^? ProtoFields.to
                return Upgraded {..}


{- 
instance FromProto Proto.RejectReason where
    type Output' Proto.RejectReason = RejectReason
    fromProto rr
        | Just _ <- rr ^? ProtoFields.moduleNotWf = ModuleNotWF
        | Just ae <- rr ^? ProtoFields.moduleHashAlreadyExists =  ModuleHashAlreadyExists $ fromProto ae
        | Just ar <- rr ^? ProtoFields.invalidAccountReference = InvalidAccountReference . fromProto $ ar
        | Just iim $ rr ^? ProtoFields.invalidInitMethod =
            InvalidInitMethod {
                moduleRef = fromProto $ iim ^. ProtoFields.moduleRef,
                Wasm.initName = fromProto $ iim ^. ProtoFields.initName
            }
        | Just iim <- rr ^? ProtoFields.invalidReceiveMethod =
            InvalidInitMethod {
                moduleRef = fromProto $ iim ^. ProtoFields.moduleRef,
                receiveName = fromProto $ iim ^. ProtoFields.receiveName
            }
        | Just imr <- rr ^? ProtoFields.invalidModuleReference =
            InvalidModuleReference {
                moduleRef = fromProto imr
            }
        | Just ica <- rr ^? ProtoFields.invalidContractAddress =
            InvalidContractAddress {
                contractAddress = fromProto ica
            }
        | Just rf <- rr ^? ProtoFields.runtimeFailure = RuntimeFailure
        | Just atl <- rr ^? ProtoFields.amountTooLarge =
            AmountTooLarge {
                addr = atl ^. ProtoFields.address,
                amount = atl ^. ProtoFields. amount
            }
        | Just  <- rr ^? ProtoFields.invalidReceiveMethod =
            InvalidReceiveMethod {
                    moduleRef = fromProto $ bi ^. ProtoFields.moduleRef,
                    receiveName = fromProto $ bi ^. ProtoFields.receiveName
            }
        | Just sf <- rr ^? ProtoFields.serializationFailure = SerializationFailure
        | Just ooe <- rr ^? ProtoFields.outOfEnergy = OutOfEnergy 
        | Just ri <- rr ^? ProtoFields.rejectedInit =
            RejectedInit {
                rejectReason = ri ^. ProtoFields.rejectReason ^. ProtoFields.rejectedInit
            }
        | Just  <- rr ^? ProtoFields. =
             {
                
            }
        | Just  <- rr ^? ProtoFields. =
             {
                
            }
        RejectedReceive{..} ->
            Proto.make $
                ProtoFields.rejectedReceive
                    .= Proto.make
                        ( do
                            ProtoFields.rejectReason .= rejectReason
                            ProtoFields.contractAddress .= toProto contractAddress
                            ProtoFields.receiveName .= toProto receiveName
                            ProtoFields.parameter .= toProto parameter
                        )
        InvalidProof -> Proto.make $ ProtoFields.invalidProof .= Proto.defMessage
        AlreadyABaker bakerId -> Proto.make $ ProtoFields.alreadyABaker .= toProto bakerId
        NotABaker addr -> Proto.make $ ProtoFields.notABaker .= toProto addr
        InsufficientBalanceForBakerStake -> Proto.make $ ProtoFields.insufficientBalanceForBakerStake .= Proto.defMessage
        StakeUnderMinimumThresholdForBaking -> Proto.make $ ProtoFields.stakeUnderMinimumThresholdForBaking .= Proto.defMessage
        BakerInCooldown -> Proto.make $ ProtoFields.bakerInCooldown .= Proto.defMessage
        DuplicateAggregationKey k -> Proto.make $ ProtoFields.duplicateAggregationKey .= mkSerialize k
        NonExistentCredentialID -> Proto.make $ ProtoFields.nonExistentCredentialId .= Proto.defMessage
        KeyIndexAlreadyInUse -> Proto.make $ ProtoFields.keyIndexAlreadyInUse .= Proto.defMessage
        InvalidAccountThreshold -> Proto.make $ ProtoFields.invalidAccountThreshold .= Proto.defMessage
        InvalidCredentialKeySignThreshold -> Proto.make $ ProtoFields.invalidCredentialKeySignThreshold .= Proto.defMessage
        InvalidEncryptedAmountTransferProof -> Proto.make $ ProtoFields.invalidEncryptedAmountTransferProof .= Proto.defMessage
        InvalidTransferToPublicProof -> Proto.make $ ProtoFields.invalidTransferToPublicProof .= Proto.defMessage
        EncryptedAmountSelfTransfer addr -> Proto.make $ ProtoFields.encryptedAmountSelfTransfer .= toProto addr
        InvalidIndexOnEncryptedTransfer -> Proto.make $ ProtoFields.invalidIndexOnEncryptedTransfer .= Proto.defMessage
        ZeroScheduledAmount -> Proto.make $ ProtoFields.zeroScheduledAmount .= Proto.defMessage
        NonIncreasingSchedule -> Proto.make $ ProtoFields.nonIncreasingSchedule .= Proto.defMessage
        FirstScheduledReleaseExpired -> Proto.make $ ProtoFields.firstScheduledReleaseExpired .= Proto.defMessage
        ScheduledSelfTransfer addr -> Proto.make $ ProtoFields.scheduledSelfTransfer .= toProto addr
        InvalidCredentials -> Proto.make $ ProtoFields.invalidCredentials .= Proto.defMessage
        DuplicateCredIDs ids -> Proto.make $ ProtoFields.duplicateCredIds . ProtoFields.ids .= (toProto <$> ids)
        NonExistentCredIDs ids -> Proto.make $ ProtoFields.nonExistentCredIds . ProtoFields.ids .= (toProto <$> ids)
        RemoveFirstCredential -> Proto.make $ ProtoFields.removeFirstCredential .= Proto.defMessage
        CredentialHolderDidNotSign -> Proto.make $ ProtoFields.credentialHolderDidNotSign .= Proto.defMessage
        NotAllowedMultipleCredentials -> Proto.make $ ProtoFields.notAllowedMultipleCredentials .= Proto.defMessage
        NotAllowedToReceiveEncrypted -> Proto.make $ ProtoFields.notAllowedToReceiveEncrypted .= Proto.defMessage
        NotAllowedToHandleEncrypted -> Proto.make $ ProtoFields.notAllowedToHandleEncrypted .= Proto.defMessage
        MissingBakerAddParameters -> Proto.make $ ProtoFields.missingBakerAddParameters .= Proto.defMessage
        FinalizationRewardCommissionNotInRange -> Proto.make $ ProtoFields.finalizationRewardCommissionNotInRange .= Proto.defMessage
        BakingRewardCommissionNotInRange -> Proto.make $ ProtoFields.bakingRewardCommissionNotInRange .= Proto.defMessage
        TransactionFeeCommissionNotInRange -> Proto.make $ ProtoFields.transactionFeeCommissionNotInRange .= Proto.defMessage
        AlreadyADelegator -> Proto.make $ ProtoFields.alreadyADelegator .= Proto.defMessage
        InsufficientBalanceForDelegationStake -> Proto.make $ ProtoFields.insufficientBalanceForDelegationStake .= Proto.defMessage
        MissingDelegationAddParameters -> Proto.make $ ProtoFields.missingDelegationAddParameters .= Proto.defMessage
        InsufficientDelegationStake -> Proto.make $ ProtoFields.insufficientDelegationStake .= Proto.defMessage
        DelegatorInCooldown -> Proto.make $ ProtoFields.delegatorInCooldown .= Proto.defMessage
        NotADelegator addr -> Proto.make $ ProtoFields.notADelegator .= toProto addr
        DelegationTargetNotABaker bakerId -> Proto.make $ ProtoFields.delegationTargetNotABaker .= toProto bakerId
        StakeOverMaximumThresholdForPool -> Proto.make $ ProtoFields.stakeOverMaximumThresholdForPool .= Proto.defMessage
        PoolWouldBecomeOverDelegated -> Proto.make $ ProtoFields.poolWouldBecomeOverDelegated .= Proto.defMessage
        PoolClosed -> Proto.make $ ProtoFields.poolClosed .= Proto.defMessage

      
instance FromProto Proto.InvokeInstanceResponse where
    type Output' Proto.InvokeInstanceResponse = InvokeContract.InvokeContractResult
    fromProto iir =
        case (iir ^? ProtoFields.success, iir ^? ProtoFields.failure) of
            (Just success, Nothing) ->
                InvokeContract.Success
                    { rcrReturnValue = success ^. ProtoFields.maybe'returnValue
                    , rcrUsedEnergy = fromProto $ success ^. ProtoFields.usedEnergy
                    , rcrEvents = fmap fromProto $ success ^. ProtoFields.effects
                    }
            (Nothing, Just failure) ->
                InvokeContract.Failure
                    { rcrReturnValue = failure ^. ProtoFields.maybe'returnValue
                    , rcrUsedEnergy = fromProto $ failure ^. ProtoFields.usedEnergy
                    , rcrReason = fromProto $ failure ^. ProtoFields.reason
                    }
            _ -> error "ouchie"

invokeInstanceV2 :: (MonadIO m) => InvokeInstanceInput -> ClientMonad m (GRPCResult (Either ConversionError InvokeContract.InvokeContractResult))
invokeInstanceV2 iiInput = withUnaryCoreV2 (callV2 @"invokeInstance") msg (fmap fromProto <$>)
  where
    msg = toProto iiInput
-}

getTokenomicsInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.RewardStatus))
getTokenomicsInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getTokenomicsInfo") msg (fmap fromProto <$>)
  where
    msg = toProto blockHash

getBlocksAtHeightV2 :: (MonadIO m) => BlockHeightInput -> ClientMonad m (GRPCResult (Maybe [BlockHash]))
getBlocksAtHeightV2 blockHeight = withUnaryCoreV2 (callV2 @"getBlocksAtHeight") msg (fmap fromProto <$>)
  where
    msg = toProto blockHeight

getPassiveDelegationInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.PoolStatus))
getPassiveDelegationInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getPassiveDelegationInfo") msg (fmap fromProto <$>)
  where
    msg = toProto blockHash

getPoolInfoV2 :: (MonadIO m) => BlockHashInput -> BakerId -> ClientMonad m (GRPCResult (Maybe QueryTypes.PoolStatus))
getPoolInfoV2 blockHash baker = withUnaryCoreV2 (callV2 @"getPoolInfo") msg (fmap fromProto <$>)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.baker .~ toProto baker

getBlockInfoV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult (Maybe QueryTypes.BlockInfo))
getBlockInfoV2 blockHash = withUnaryCoreV2 (callV2 @"getBlockInfo") msg (fmap fromProto <$>)
  where
    msg = toProto blockHash

getConsensusInfoV2 :: (MonadIO m) => ClientMonad m (GRPCResult (Maybe Wasm.WasmModule))
getConsensusInfoV2 = withUnaryCoreV2 (callV2 @"getModuleSource") defMessage (fmap fromProto <$>)

getModuleSourceV2 :: (MonadIO m) => ModuleRef -> BlockHashInput -> ClientMonad m (GRPCResult (Maybe Wasm.WasmModule))
getModuleSourceV2 modRef hash = withUnaryCoreV2 (callV2 @"getModuleSource") msg (fmap fromProto <$>)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto hash & ProtoFields.moduleRef .~ toProto modRef

-- |Retrieve the account information from the chain.
getAccountInfoV2 ::
    MonadIO m =>
    -- | Account identifier, address, index or credential registration id.
    AccountIdentifierInput ->
    -- | Block hash
    BlockHashInput ->
    ClientMonad m (GRPCResult (Maybe Concordium.Types.AccountInfo))
getAccountInfoV2 account blockHash = withUnaryCoreV2 (callV2 @"getAccountInfo") msg (fmap fromProto <$>)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.accountIdentifier .~ toProto account

getInstanceInfoV2 :: (MonadIO m) => ContractAddress -> BlockHashInput -> ClientMonad m (GRPCResult (Maybe Wasm.InstanceInfo))
getInstanceInfoV2 cAddress blockHash = withUnaryCoreV2 (callV2 @"getInstanceInfo") msg (fmap fromProto <$>)
  where
    msg = defMessage & ProtoFields.blockHash .~ toProto blockHash & ProtoFields.address .~ toProto cAddress

getNextSequenceNumberV2 :: (MonadIO m) => AccountAddress -> ClientMonad m (GRPCResult (Maybe QueryTypes.NextAccountNonce))
getNextSequenceNumberV2 accAddress = withUnaryCoreV2 (callV2 @"getNextAccountSequenceNumber") msg (fmap fromProto <$>)
  where
    msg = toProto accAddress

{- FIXME: Should we still use GlobalContext for these internally?
getCryptographicParametersV2 :: (MonadIO m) => BlockHashInput -> ClientMonad m (GRPCResult CryptographicParameters)
getCryptographicParametersV2 blockHash = withUnaryCoreV2 (callV2 @"getCryptographicParameters") msg (fmap fromProto <$>)
  where
    msg = toProto blockHash
-}

{- | Setup the GRPC client and run a rawUnary call with the provided message to the provided method,
 the output is interpreted using the function given in the third parameter.
-}
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
                            threadDelay 1000000
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
             in do
                    addHeaders response
                    return (k response)
  where
    addHeaders response = case response of
        Right GRPCResponse{..} -> do
            ClientMonad $ do
                forM_ grpcHeaders $ \(hn, hv) ->
                    when (hn == "set-cookie") $
                        let c = Cookie.parseSetCookie hv
                         in modify' (Map.insert (Cookie.setCookieName c) (Cookie.setCookieValue c))
        Left _ -> return ()

-- | Setup the GRPC client and run a rawUnary call to the provided method.
withUnaryCoreNoMsgV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    (GRPCResult (MethodOutput CS.Queries m) -> (CIHeaderList, b)) ->
    ClientMonad n (CIHeaderList, b)
withUnaryCoreNoMsgV2 method = withUnaryCoreV2 method defMessage

-- | Call a method with a given message and use a Getter on the output.
withUnaryV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    MethodInput CS.Queries m ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryV2 method message k = withUnaryCoreV2 method message (\x -> (^. k) <$> x)

-- | Call a method with a given message using the `id` lens on the output.
withUnaryV2' ::
    forall m n.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    MethodInput CS.Queries m ->
    ClientMonad n (GRPCResult (MethodOutput CS.Queries m))
withUnaryV2' method message = withUnaryV2 method message (to id)

-- | Call a method without a message using the given lens
withUnaryNoMsgV2 ::
    forall m n b.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryNoMsgV2 method = withUnaryV2 method defMessage

-- | Call a method with a message that has `blockHash` as a field and use the given lens on the output
withUnaryBlockV2 ::
    forall m n b.
    ( HasMethod CS.Queries m
    , MonadIO n
    , Field.HasField (MethodInput CS.Queries m) "blockHash" Text
    ) =>
    RPC CS.Queries m ->
    Text ->
    SimpleGetter (GRPCResponse (MethodOutput CS.Queries m)) b ->
    ClientMonad n (Either String b)
withUnaryBlockV2 method hash = withUnaryV2 method (defMessage & ProtoFields.blockHash .~ hash)

-- | Call a method with an empty message using the `id` lens on the output.
withUnaryNoMsgV2' ::
    forall m n.
    (HasMethod CS.Queries m, MonadIO n) =>
    RPC CS.Queries m ->
    ClientMonad n (GRPCResult (MethodOutput CS.Queries m))
withUnaryNoMsgV2' method = withUnaryV2' method defMessage

callV2 :: forall m. RPC CS.Queries m
callV2 = RPC @CS.Queries @m