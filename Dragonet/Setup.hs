import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.PackageDescription
import qualified Distribution.Simple.PackageIndex as PI
import qualified Distribution.InstalledPackageInfo as IPI
import qualified System.Directory as SD
import System.FilePath
import Data.Maybe

main = defaultMainWithHooks simpleUserHooks {
    buildHook = myBuildHook,
    hookedPrograms = hookedPrograms simpleUserHooks ++
        [simpleProgram "clang",
         simpleProgram "llvm-link"]
    }

llvmSources = [ "c_impl/impl_arp.c", "c_impl/impl_ethernet.c",
    "c_impl/impl_icmp.c", "c_impl/impl_ipv4.c", "c_impl/impl_udp.c",
    "c_impl/impl_tcp.c", "c_impl/impl_misc.c", "c_impl/implementation.c",
    "c_impl/support/Ethernet.c", "c_impl/support/Icmp.c", "lib/Util/tap.c" ]
llvmCAdditionalInc = ["../external/bulktransfer/lib/include"]

-- Run clang to get llvm bitcode
llvmClang prog opts incls outD src = do
    let outD' = outD </> dropFileName src
        out_file = outD' </> replaceExtension (takeFileName src) ".bc"
        iopts = map ("-I" ++) incls
        opts' = opts ++ iopts ++ ["-emit-llvm", "-c", src, "-o", out_file]
    SD.createDirectoryIfMissing True outD'
    runProgram verbose prog opts'
    return out_file

-- Link llvm bitcode files together
llvmLink prog opts outD outF srcs = do
    let outP = outD </> outF
        opts = srcs ++ ["-o", outP]
    runProgram verbose prog opts
    return outP


myBuildHook pkg_descr local_bld_info user_hooks bld_flags = do
    let lib = fromJust (library pkg_descr)
        lib_bi = libBuildInfo lib
        inc_dirs = includeDirs lib_bi

    let bld_dir = buildDir local_bld_info
        progs = withPrograms local_bld_info
        (Just clang) = lookupProgram (simpleProgram "clang") progs
        (Just llvmlink) = lookupProgram (simpleProgram "llvm-link") progs
        depIncls = concatMap IPI.includeDirs $ PI.allPackages $
            installedPkgs local_bld_info
        incls = inc_dirs ++ depIncls ++ llvmCAdditionalInc
    -- Build llvm bitcode for C implementation of graph
    llvmBCs <- mapM (llvmClang clang ["-O3"] incls bld_dir) llvmSources
    -- Combine into one bitcode file
    llvmLink llvmlink [] bld_dir "llvm-helpers.bc" llvmBCs
    -- Default build hook
    buildHook simpleUserHooks pkg_descr local_bld_info user_hooks bld_flags
