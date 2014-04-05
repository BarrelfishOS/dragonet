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
import System.Process
import System.Exit
import Distribution.Simple.Utils
import Control.Monad (forM_)

--
-- Part of this is inspired by this: https://gist.github.com/jodonoghue/1301115


dpdkDir = ".." </> ".." </> "dpdk-1.5.0r1"
libraries = ["intel_dpdk", "dpdk_driver"]
libsFull = map fullN libraries
    where fullN l = "lib" ++ l ++ ".so"


main = defaultMainWithHooks simpleUserHooks {
    confHook  = myConfHook,
    buildHook = myBuildHook,
    preInst   = myPreInstHook,
    instHook  = myInstHook,
    preCopy   = myPreCopyHook,
    copyHook  = myCopyHook,
    preReg    = myPreRegHook }

exec :: FilePath -> [String] -> Maybe FilePath -> IO ()
exec prog args wdir = do
    h <- runProcess prog args wdir Nothing Nothing Nothing Nothing
    st <- waitForProcess h
    if st == ExitSuccess
        then return ()
        else error $ "Error running command (" ++ prog ++ ")" -- FIXME


-- This config hook will make sure that the directory where we install our
--   shared library is indeed on the search path.
--   Note: I'm not really sure why this is needed, since this is the default
--     location for the packet, but somehow cabal does not add it
myConfHook (pkg0, pbi) flags = do
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags

    let lpd        = localPkgDescr lbi
        lib        = fromJust (library lpd)
        libbi      = libBuildInfo lib
        custom_bi  = customFieldsBI libbi
        pkgd = packageDescription pkg0

        -- Directory where our library will end up in
        libD = libdir $ absoluteInstallDirs pkgd lbi NoCopyDest
        -- This is the actual change we're interested in
        libbi' = libbi { extraLibDirs = extraLibDirs libbi ++ [libD] }

        lib' = lib { libBuildInfo = libbi' }
        lpd' = lpd { library = Just lib' }
    return $ lbi { localPkgDescr = lpd' }


-- Build hook that will build the library and copy it into our build directory
myBuildHook pkg_descr local_bld_info user_hooks bld_flags = do
    let lib = fromJust (library pkg_descr)
        lib_bi = libBuildInfo lib
        inc_dirs = includeDirs lib_bi
        bld_dir = buildDir local_bld_info
        progs = withPrograms local_bld_info
        verb = fromFlag $ buildVerbosity bld_flags
    -- Conf
    exec "./doConfig.sh" [] $ Just dpdkDir
    exec "make" [] $ Just dpdkDir

    let bldLibDir = dpdkDir </> "build" </> "lib"
    forM_ libsFull $ \l -> do
        installOrdinaryFile verb (bldLibDir </> l) bld_dir


-- Hook to add the input files to installIncludes, so that they will be
-- installed
includeAddHook :: IO HookedBuildInfo
includeAddHook = do
    let incdir = dpdkDir </> "build" </> "include"
    rec_files <- getDirectoryContentsRecursive incdir
    let incs = filter ((".h" ==) . takeExtension) rec_files
    putStrLn $ "PreInstFiles: " ++ show incs
    return (Just $ emptyBuildInfo {
                installIncludes = incs,
                includeDirs = [incdir]
            }, [])

myPreInstHook :: Args -> InstallFlags -> IO HookedBuildInfo
myPreInstHook args in_flags = includeAddHook

myPreCopyHook :: Args -> CopyFlags -> IO HookedBuildInfo
myPreCopyHook args cp_flags = includeAddHook



-- Hook to install the shared library
libInstallHook :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
libInstallHook pkg_descr local_bld_info verb = do
    let idt = installDirTemplates local_bld_info
        libBD = fromPathTemplate $ libdir idt
        libSD = fromPathTemplate $ libsubdir idt
        libD = libBD </> libSD
        bldLibDir = dpdkDir </> "build" </> "lib"
    let ilibD = libdir $ absoluteInstallDirs pkg_descr local_bld_info NoCopyDest
    forM_ libsFull $ \l -> do
        installOrdinaryFile verb (bldLibDir </> l) (ilibD </> l)

-- Not exactly sure why cabal install does not call the instHook
myCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags ->
                IO ()
myCopyHook pkg_descr local_bld_info  user_hooks cp_flags = do
    copyHook simpleUserHooks pkg_descr local_bld_info user_hooks cp_flags
    libInstallHook pkg_descr local_bld_info $ fromFlag $ copyVerbosity cp_flags

myInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks ->
                InstallFlags -> IO ()
myInstHook pkg_descr local_bld_info  user_hooks in_flags = do
    instHook simpleUserHooks pkg_descr local_bld_info user_hooks in_flags
    libInstallHook pkg_descr local_bld_info $ fromFlag $
        installVerbosity in_flags


-- Add the extra library so that applications will be linked against it
--   Note we can't do this in a config hook because somewhere after config it
--   checks if it actually exists, which it doesn't at this point
extraLibAddHook :: IO HookedBuildInfo
extraLibAddHook =
    return (Just $ emptyBuildInfo {
        extraLibs = libraries }, [])

myPreRegHook :: Args -> RegisterFlags -> IO HookedBuildInfo
myPreRegHook _ _ = extraLibAddHook

