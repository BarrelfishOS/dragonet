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

llvmVersions = ["3.4", "3.5"]

-- Versioned llvm program
--   e.g. llvm-link could be named just llvm-link, or lllvm-link-3.4
--   all versions in llvmVersions are tried
llvmProgram :: String -> Program
llvmProgram name = (simpleProgram name) {
                        programFindLocation = findFirst names }
    where
        names = name : (map ((++) (name ++ "-")) llvmVersions)
        findFirst [] _ _ = return Nothing
        findFirst (n:ns) verb a = do
            prog <- findProgramLocation verb n
            case prog of
                Just p -> return prog
                Nothing -> findFirst ns verb a

main = defaultMainWithHooks simpleUserHooks {
    buildHook = myBuildHook,
    hookedPrograms = hookedPrograms simpleUserHooks ++
        [simpleProgram "clang",
         llvmProgram "llvm-link"]
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
    let bld_dir = buildDir local_bld_info
        progs = withPrograms local_bld_info
        (Just clang) = lookupProgram (simpleProgram "clang") progs
        (Just llvmlink) = lookupProgram (llvmProgram "llvm-link") progs
        verb = fromFlag $ buildVerbosity bld_flags

    let lib = libBuildInfo <$> library pkg_descr
        bis = maybeToList lib ++ map buildInfo (executables pkg_descr)

    -- Iterate over BuildInfos of libraries and executables
    forM_ bis $ \bi -> do
        let inc_dirs = includeDirs bi
            x_fields = customFieldsBI bi
            x_incs = map snd $ filter ((== "x-llvm-c-includes") . fst) x_fields
            inc_llvm = concatMap parseNameList x_incs
            x_chelp = map snd $ filter ((== "x-llvm-c-helpers") . fst) x_fields
            c_helpers = map parseNameList x_chelp
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
