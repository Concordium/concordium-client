{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Concordium.Client.Types.Contract.BuildInfo (
    BuildInfo (..),
    extractBuildInfo,
    showBuildInfo,
) where

import Concordium.Client.Types.Contract.WasmParseHelpers
import Concordium.Crypto.SHA256
import Concordium.Utils.Serialization (getMaybe)
import qualified Concordium.Wasm as Wasm
import Control.Monad (replicateM, unless)
import Data.Maybe (isJust)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Build information about the smart contract module. This is a Haskell type
--  that corresponds to the data embedded by cargo-concordium. If the latter
--  changes so must this type.
data BuildInfo = BuildInfo
    { -- | The SHA256 hash of the tar file used to build.
      --  Note that this is the hash of the **tar** file alone, not of any
      --  compressed version.
      archiveHash :: Hash,
      -- | The link to where the source code will be located.
      sourceLink :: Maybe Text,
      -- | The build image that was used.
      image :: Text,
      -- | The exact command invocation inside the image that was used to produce
      --  the contract.
      buildCommand :: [Text]
    }

showBuildInfo :: BuildInfo -> [String]
showBuildInfo BuildInfo{..} =
    [ "- Build image used: " ++ Text.unpack image,
      "- Build command used: " ++ unwords (map Text.unpack buildCommand),
      "- Hash of the archive: " ++ show archiveHash,
      case sourceLink of
        Nothing -> "- No link to the source code embedded."
        Just link -> "- Link to source code: " ++ Text.unpack link
    ]

-- | Extract build information from the module if available.
extractBuildInfo :: Wasm.WasmModule -> Either String (Maybe BuildInfo)
extractBuildInfo = S.runGet parser . Wasm.wasmSource
  where
    parser = S.label "ModuleBuildInfo" $ do
        ensureWasmModule
        go Nothing

    go :: Maybe BuildInfo -> S.Get (Maybe BuildInfo)
    go mbi = do
        isEmpty <- S.isEmpty
        if isEmpty
            then -- End of module reached; return the values found.
                return mbi
            else do
                sectionId <- S.label "sectionId" S.getWord8
                sectionSize <- S.label "sectionSize" $ fromIntegral <$> getLEB128Word32le
                case sectionId of
                    -- Custom section (which is where we store the build information)
                    0 -> do
                        -- Remember where we are since we might have to skip the section.
                        curPos <- S.bytesRead
                        name <- S.label "Custom Section Name" getTextWithLEB128Len
                        if name == "concordium-build-info"
                            then
                                if isJust mbi
                                    then fail [i|Module cannot contain multiple custom sections named 'concordium-build-info'.|]
                                    else go . Just =<< getBuildInfo
                            else do
                                afterPos <- S.bytesRead
                                -- Only skip the parts of the section we have not read yet.
                                S.skip (sectionSize - (afterPos - curPos)) *> go mbi
                    -- Any other type of section
                    _ -> S.skip sectionSize *> go mbi

    getBuildInfo :: S.Get BuildInfo
    getBuildInfo = do
        version <- S.getWord8
        unless (version == 0) $ fail "Only V0 build informatio is supported."
        archiveHash <- S.get
        sourceLink <- getMaybe getTextLE
        image <- getTextLE
        buildCommandLen <- S.getWord32le
        buildCommand <- replicateM (fromIntegral buildCommandLen) getTextLE
        return BuildInfo{..}
