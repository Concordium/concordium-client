{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Concordium.Client.Types.ConsensusStatus where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (isLower)
import Data.Int
import Data.Word

import qualified Concordium.Crypto.BlockSignature as BlockSignature
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.Types.Block
import Concordium.Types.Queries.KonsensusV1 hiding (
    FinalizerRound,
    QuorumCertificate,
    TimeoutCertificate,
 )
import Concordium.Utils

-- | Index of a finalizer in the finalization committee vector.
newtype FinalizerIndex = FinalizerIndex {theFinalizerIndex :: Word32}
    deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | The message that is multicast by a finalizer when validating and signing a block.
data QuorumMessage = QuorumMessage
    { -- | Signature on the relevant quorum signature message.
      qmSignature :: !QuorumCertificateSignature,
      -- | Hash of the block that is signed.
      qmBlock :: !BlockHash,
      -- | Index of the finalizer signing the message.
      qmFinalizer :: !FinalizerIndex,
      -- | Round of the block.
      qmRound :: !Round,
      -- | Epoch of the block.
      qmEpoch :: !Epoch
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''QuorumMessage)

data QuorumCertificate = QuorumCertificate
    { -- | The hash of the block that the quorum certificate refers to.
      qcBlockHash :: !BlockHash,
      -- | The round of the block.
      qcRound :: !Round,
      -- | The epoch of the block.
      qcEpoch :: !Epoch,
      -- | The aggregated signature by the finalization committee on the block.
      qcAggregateSignature :: !QuorumCertificateSignature,
      -- | A list of the finalizers that formed the quorum certificate
      -- i.e., the ones who have contributed to the 'aggregate_siganture'.
      -- The finalizers are identified by their finalizer index, which refers to the
      -- finalization committee for the epoch.
      qcSignatories :: ![FinalizerIndex]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''QuorumCertificate)

-- | A timeout message including the sender's signature.
data TimeoutMessage = TimeoutMessage
    { -- | Index of the finalizer signing the message.
      tmFinalizer :: !FinalizerIndex,
      -- | Round which timed out.
      tmRound :: !Round,
      -- | Current epoch number of the finalizer sending the timeout message.
      -- This can be different from the epoch of the quorum certificate.
      tmEpoch :: !Epoch,
      -- | Highest quorum certificate known to the finalizer at the time of timeout.
      tmQuorumCertificate :: !QuorumCertificate,
      -- | Signature on the appropriate timeout signature message.
      tmSignature :: !TimeoutCertificateSignature,
      -- | Signature of the finalizer on the timeout message as a whole.
      tmMessageSignature :: !BlockSignature.Signature
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''TimeoutMessage)

data FinalizerRound = FinalizerRound
    { -- | The round that was signed off.
      frRound :: !Round,
      -- | The finalizers (identified by their 'FinalizerIndex') that
      -- signed off the in 'round'.
      frFinalizers :: ![FinalizerIndex]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''FinalizerRound)

-- | A timeout certificate is the certificate that the
-- finalization committee issues when a round times out,
-- thus making it possible for the protocol to proceed to the
-- next round.
data TimeoutCertificate = TimeoutCertificate
    { -- | The round that timed out.
      tcRound :: !Round,
      -- | The minimum epoch of which signatures are included
      -- in the 'aggregate_signature'.
      tcMinEpoch :: !Epoch,
      -- | The rounds of which finalizers have their best
      -- QCs in the 'min_epoch'.
      tcQcRoundsFirstEpoch :: ![FinalizerRound],
      -- | The rounds of which finalizers have their best
      -- QCs in the epoch 'min_epoch' + 1.
      tcQcRoundsSecondEpoch :: ![FinalizerRound],
      -- | The aggregated signature by the finalization committee that witnessed
      -- the 'round' timed out.
      tcAggregateSignature :: !TimeoutCertificateSignature
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''TimeoutCertificate)

data PersistentRoundStatus = PersistentRoundStatus
    { -- | The last signed quorum message by the node.
      prsLastSignedQuorumMessage :: !(Maybe QuorumMessage),
      -- | The last signed timeout message by the node.
      prsLastSignedTimeoutMessage :: !(Maybe TimeoutMessage),
      -- | The last round the node baked in.
      prsLastBakedRound :: !Round,
      -- | The latest timeout certificate seen by the node. May be absent if the node has seen a
      -- quorum certificate for a more recent round.
      prsLatestTimeout :: !(Maybe TimeoutCertificate)
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''PersistentRoundStatus)

data RoundTimeout = RoundTimeout
    { -- | The timeout certificate for the round that timed out.
      rtTimeoutCertificate :: !TimeoutCertificate,
      -- | The highest quorum certificate known when the round timed out.
      rtQuorumCertificate :: !QuorumCertificate
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''RoundTimeout)

data FinalizationEntry = FinalizationEntry
    { -- | The quorum certificate for the finalized block.
      feFinalizedQC :: !QuorumCertificate,
      -- | The quorum certificate for the block that finalizes
      -- the block that 'finalized_qc' points to.
      feSuccessorQC :: !QuorumCertificate,
      -- | A proof that the successor block is an immediate
      -- successor of the finalized block.
      feSuccessorProof :: !SuccessorProof
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''FinalizationEntry)

data RoundStatus = RoundStatus
    { -- | The current round from the perspective of the node.
      -- This should always be higher than the round of the highest certified block.
      -- If the previous round did not timeout, it should be one more than the round of
      -- the `highest_certified_block`. Otherwise, it should be one more than the round of
      -- the `previous_round_timeout`.
      rsCurrentRound :: !Round,
      -- | The quorum certificate for the highest certified block.
      rsHighestCertifiedBlock :: !QuorumCertificate,
      -- | If the last round timed out, this is the timeout certificate for that round and
      -- the highest quorum certificate at the time the round timed out.
      rsPreviousRoundTimeout :: !(Maybe RoundTimeout),
      -- | Flag indicating whether the node should attempt to bake in the current round.
      -- This is set to true when the round is advanced, and set to false once the node has
      -- attempted to bake for the round.
      rsRoundEligibleToBake :: !Bool,
      -- | The current epoch. This should either be the same as the epoch of the last finalized
      -- block (if its timestamp is before the trigger block time) or the next epoch from the last
      -- finalized block (if its timestamp is at least the trigger block time).
      rsCurrentEpoch :: !Epoch,
      -- | If present, an epoch finalization entry for the epoch before `current_epoch`.
      -- An entry must be present if the current epoch is greater than the epoch of the last
      -- finalized block.
      rsLastEpochFinalizationEntry :: !(Maybe FinalizationEntry),
      -- | The current duration the node will wait before a round times out.
      rsCurrentTimeout :: !Duration
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''RoundStatus)

data BlockTableSummary = BlockTableSummary
    { -- | The number of blocks in the dead block cache.
      btsDeadBlockCacheSize :: !Word64,
      -- | The blocks that are currently live (not dead and not finalized).
      btsLiveBlocks :: ![BlockHash]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BlockTableSummary)

data RoundExistingBlock = RoundExistingBlock
    { rebRound :: !Round,
      rebBaker :: !BakerId,
      rebBlock :: !BlockHash
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''RoundExistingBlock)

data RoundExistingQC = RoundExistingQC
    { reqRound :: !Round,
      reqEpoch :: !Epoch
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''RoundExistingQC)

data FullBakerInfo = FullBakerInfo
    { -- | The ID of the baker.
      fbiBakerIdentity :: !BakerId,
      -- | The election verification key of the baker.
      fbiElectionVerifyKey :: !VRF.PublicKey,
      -- | The signature verification key of the baker.
      fbiSignatureVerifyKey :: !BlockSignature.VerifyKey,
      -- | The aggregation verification key of the baker.
      fbiAggregationVerifyKey :: !Bls.PublicKey,
      -- | The stake of the baker.
      fbiStake :: !Amount
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''FullBakerInfo)

data BakersAndFinalizers = BakersAndFinalizers
    { -- | The set of bakers.
      bafBakers :: ![FullBakerInfo],
      -- | The IDs of the bakers that are finalizers.
      -- The order determines the finalizer index.
      bafFinalizers :: ![BakerId],
      -- | The total effective stake of the bakers.
      bafBakerTotalStake :: !Amount,
      -- | The total effective stake of the finalizers.
      bafFinalizerTotalStake :: !Amount,
      -- | The hash of the finalization committee.
      bafFinalizationCommitteeHash :: !Hash.Hash
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BakersAndFinalizers)

data EpochBakers = EpochBakers
    { -- | The bakers and finalizers for the previous epoch.
      --  If the current epoch is 0, then this is the same as the bakers for the current epoch.
      ebPreviousEpochBakers :: !BakersAndFinalizers,
      -- | The bakers and finalizers for the current epoch.
      --  If this is absent, it should be treated as the same as the bakers for the previous epoch.
      ebCurrentEpochBakers :: !(Maybe BakersAndFinalizers),
      -- | The bakers and finalizers for the next epoch.
      --  If this is absent, it should be treated as the same as the bakers for the current epoch.
      ebNextEpochBakers :: !(Maybe BakersAndFinalizers),
      -- | The first epoch of the next payday.
      ebNextPayday :: !Epoch
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''EpochBakers)

data TimeoutMessages = TimeoutMessages
    { -- | The first epoch for which timeout messages are present.
      tmFirstEpoch :: !Epoch,
      -- | The timeout messages for the first epoch.
      -- There should always be at least one.
      tmFirstEpochTimeouts :: ![TimeoutMessage],
      -- | The timeout messages for @first_epoch + 1@.
      tmSecondEpochTimeouts :: ![TimeoutMessage]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''TimeoutMessages)

data ConsensusDetailedStatus = ConsensusDetailedStatus
    { -- | The hash of the genesis block.
      cdsGenesisBlock :: !BlockHash,
      -- | The persisted elements of the round status.
      cdsPersistentRoundStatus :: !PersistentRoundStatus,
      -- | The status of the current round.
      cdsRoundStatus :: !RoundStatus,
      -- | The number of non-finalized transactions.
      cdsNonFinalizedTransactionCount :: !Word64,
      -- | The purge counter for the transaction table.
      cdsTransactionTablePurgeCounter :: !Int64,
      -- | Summary of the block table.
      cdsBlockTable :: !BlockTableSummary,
      -- | The live blocks organized by height after the last finalized block.
      cdsBranches :: ![[BlockHash]],
      -- | Which bakers the node has seen legally-signed blocks with live parents from in non-finalized rounds.
      cdsRoundExistingBlocks :: ![RoundExistingBlock],
      -- | Which non-finalized rounds the node has seen quorum certificates for.
      cdsRoundExistingQCs :: ![RoundExistingQC],
      -- | The absolute block height of the genesis block of the era.
      cdsGenesisBlockHeight :: !AbsoluteBlockHeight,
      -- | The hash of the last finalized block.
      cdsLastFinalizedBlock :: !BlockHash,
      -- | The height of the last finalized block.
      cdsLastFinalizedBlockHeight :: !BlockHeight,
      -- | Unless the last finalized block is the genesis block, this should be a finalization entry for the last finalized block.
      cdsLatestFinalizationEntry :: !(Maybe FinalizationEntry),
      -- | The bakers and finalizers for the previous, current and next epoch, relative to the last finalized block.
      cdsEpochBakers :: !EpochBakers,
      -- | The timeout messages collected by the node for the current round.
      cdsTimeoutMessages :: !(Maybe TimeoutMessages),
      -- | If a protocol update has occurred, this is the hash of the terminal block.
      cdsTerminalBlock :: !(Maybe BlockHash)
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''ConsensusDetailedStatus)
