import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System

autoConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
autoConfHook desc flags = do
    case buildOS of
        Linux ->
            let extraFlags =
                    if lookupFlagAssignment (mkFlagName "static") (configConfigurationsFlags flags) /= Just True
                        then -- embed RPATH so that the binaries can be run without setting library paths directly.
                        -- ghc embeds RPATH if the --dynamic flag is passed, so this solution might appear needless.
                        -- However profiling builds do not work with --dynamic, whereas this solution works regardless.
                            flags{configProgramArgs = ("ghc", map ("-optl-Wl,-rpath," ++) (configExtraLibDirs flags)) : configProgramArgs flags}
                        else flags
            in  confHook simpleUserHooks desc extraFlags
        _ -> confHook simpleUserHooks desc flags

main =
    defaultMainWithHooks
        simpleUserHooks
            { confHook = autoConfHook
            }

