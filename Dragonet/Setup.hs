-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.PackageDescription
import qualified Distribution.Simple.PackageIndex as PI
import qualified Distribution.InstalledPackageInfo as IPI
import qualified System.Directory as SD
import System.FilePath
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List as L
import System.Environment as ENV

llvmVersions = ["3.4", "3.5"]

forceLlvmVer :: IO (Bool)
forceLlvmVer = isJust <$> ENV.lookupEnv "DRAGONET_FORCE_LLVM_VERSION"

-- Versioned llvm program
--   e.g. llvm-link could be named just llvm-link, or lllvm-link-3.4
--   all versions in llvmVersions are tried
llvmProgram :: Bool -> String -> Program
llvmProgram force_version name = (simpleProgram name) {
                        programFindLocation = findFirst names }
    where
        vnames = (map ((++) (name ++ "-")) llvmVersions)
        names = if force_version
                then vnames
                else name:vnames
        findFirst [] _ _ = return Nothing
        findFirst (n:ns) verb a = do
            prog <- findProgramLocation verb n
            case prog of
                Just p -> return prog
                Nothing -> findFirst ns verb a

main = do
    force_ver <- forceLlvmVer
    defaultMainWithHooks simpleUserHooks {
        buildHook = myBuildHook,
        hookedPrograms = hookedPrograms simpleUserHooks ++ [
            simpleProgram "clang",
            simpleProgram "make",
            llvmProgram force_ver "llvm-link"]
    }

-- Run clang to get llvm bitcode
llvmClang prog verb opts incls outD src = do
    let outD' = outD </> dropFileName src
        out_file = outD' </> replaceExtension (takeFileName src) ".bc"
        iopts = map ("-I" ++) incls
        opts' = opts ++ iopts ++ ["-emit-llvm", "-c", src, "-o", out_file]
    SD.createDirectoryIfMissing True outD'
    runProgram verb prog opts'
    return out_file

-- Link llvm bitcode files together
llvmLink prog verb opts outD outF srcs = do
    let outP = outD </> outF
        opts = srcs ++ ["-o", outP]
    runProgram verb prog opts
    return outP

-- call make for creating helper files
makeHelper verb get_xfield make incs llvmlink bld_dir = do
    case get_xfield "x-helpers-files" of
        Nothing -> return ()
        Just xfiles' -> do
            let xfiles  = parseNameList xfiles'
                xmkfile = case get_xfield "x-helpers-makefile" of
                    Nothing -> error $ "Error: need to specify a makefile (via x-helpers-makefile) for building " ++ (show xfiles)
                    Just x  -> x
                xincs = case get_xfield "x-helpers-includes" of
                    Nothing -> ""
                    Just x  -> L.intercalate "" $ map (\x -> "-I" ++ x ++ " \\\n ") $ ((parseNameList x) ++ incs)
                args = ["LLVM_LINK=" ++ (locationPath $ programLocation llvmlink),
                        "BUILD_DIR=" ++ bld_dir,
                        "CFLAGS=-O3 -Wall -Wno-unused-variable -Wno-unused-function \\\n " ++ xincs,
                        "-f", xmkfile] ++
                        map ((bld_dir ++ "/") ++  ) xfiles
            runProgram verb make args
            return ()

-- Split list by delimiter into multiple lists
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
          f _ _ = undefined

-- Parse a comma separated list of file names
parseNameList :: String -> [String]
parseNameList s = filter (not . null) $ map trim $ splitBy ',' s
    where trim x = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x

-- Build hook that will build llvm-bitcode for our C helpers
myBuildHook pkg_descr local_bld_info user_hooks bld_flags = do
    force_ver <- forceLlvmVer
    let bld_dir = buildDir local_bld_info
        progs = withPrograms local_bld_info
        (Just clang) = lookupProgram (simpleProgram "clang") progs
        (Just llvmlink) = lookupProgram (llvmProgram force_ver "llvm-link") progs
        (Just make) = lookupProgram (simpleProgram "make") progs
        verb = fromFlag $ buildVerbosity bld_flags

    let lib = libBuildInfo <$> library pkg_descr
        bis = maybeToList lib ++ map buildInfo (executables pkg_descr)

    -- Iterate over BuildInfos of libraries and executables
    forM_ bis $ \bi -> do
        let x_fields = customFieldsBI bi
            get_xfield :: String -> Maybe String
            get_xfield xname = snd <$> (L.find ((== xname) . fst) x_fields)
            inc_dirs = includeDirs bi
            depIncls = concatMap IPI.includeDirs $ PI.allPackages $ installedPkgs local_bld_info
            incs = inc_dirs ++ depIncls
            x_incs = map snd $ filter ((== "x-llvm-c-includes") . fst) x_fields
            inc_llvm = concatMap parseNameList x_incs
            x_chelp = map snd $ filter ((== "x-llvm-c-helpers") . fst) x_fields
            c_helpers = map parseNameList x_chelp

        -- new method: using make
        makeHelper verb get_xfield make incs llvmlink bld_dir

        -- old method: call clang/llvm-link from here
        -- Iterate over all x-llvm-c-helpers entries
        forM_ c_helpers  $ \(out_file:c_files) -> do
            let -- TODO: This is a bit too generic, gets includes from all
                --       installed packets
                depIncls = concatMap IPI.includeDirs $ PI.allPackages $
                    installedPkgs local_bld_info
                incls = inc_llvm ++ inc_dirs ++ depIncls
                cOpts = ["-O3","-Wall","-Wno-unused-variable",
                    "-Wno-unused-function"]
            -- Build llvm bitcode for C implementation of graph
            llvmBCs <- mapM (llvmClang clang verb cOpts incls bld_dir) c_files
            -- Combine into one bitcode file
            llvmLink llvmlink verb [] bld_dir out_file llvmBCs

    -- Default build hook
    buildHook simpleUserHooks pkg_descr local_bld_info user_hooks bld_flags
