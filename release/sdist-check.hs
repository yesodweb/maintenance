{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (FilePath, getContents)
import System.Environment (getArgs)
import Network.HTTP.Conduit
import Network.HTTP.Types (status200, status404, status502)
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, forM_)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Archive.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Exception (try, SomeException (..))
import Control.Monad.IO.Class (liftIO)

debug :: String -> IO ()
#ifdef DEBUG
debug = putStrLn
#else
debug = const $ return ()
#endif

getUrlHackage :: Package -> IO (Request m)
getUrlHackage (Package a b) = do
    debug url
    parseUrl url
  where
    url = concat
        [ "http://hackage.haskell.org/packages/archive/"
        , a
        , "/"
        , b
        , "/"
        , a
        , "-"
        , b
        , ".tar.gz"
        ]

getUrlYackage :: Package -> IO (Request m)
getUrlYackage (Package a b) = do
    debug url
    parseUrl url
  where
    url = concat
        [ "http://yackage.yesodweb.com/package/"
        , a
        , "-"
        , b
        , ".tar.gz"
        ]

main :: IO ()
main = do
    manager <- newManager def
    (dir':args) <- getArgs
    let toPrune = args == ["--prune"]
    ss <- listDirectory (decodeString dir') >>= mapM (go manager)
    let m = Map.unionsWith Set.union ss
    let say = putStrLn . reverse . drop 7 . reverse . encodeString . filename

    case Map.lookup NoChanges m of
        Nothing -> return ()
        Just s -> do
            putStrLn "The following packages from Hackage have not changed:"
            mapM_ say $ Set.toList s
            when toPrune $ mapM_ removeFile $ Set.toList s

    case Map.lookup OnlyOnYackage m of
        Nothing -> return ()
        Just s -> do
            putStrLn "\nThe following packages from Yackage have not changed:"
            mapM_ say $ Set.toList s

    case Map.lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            putStrLn "\nThe following new packages exist locally:"
            mapM_ say $ Set.toList s
            forM_ (Set.toList s) $ \fp -> do
                copyFile fp $ "to-release" </> filename fp

    case Map.lookup NeedsVersionBump m of
        Nothing -> putStrLn "\nNo version bumps required, good to go!"
        Just s -> do
            putStrLn "\nThe following packages require a version bump:"
            mapM_ say $ Set.toList s

data Status = DoesNotExist | OnlyOnYackage | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

go :: Manager -> FilePath -> IO (Map.Map Status (Set.Set FilePath))
go m fp = do
    let base = T.reverse $ T.drop 7 $ T.reverse $ either id id $ toText $ filename fp
    let package = parsePackage $ T.unpack base
    localFileHackage <- liftIO $ getHackageFile package
    localFileYackage <- liftIO $ getYackageFile package
    fh <- liftIO $ isFile localFileHackage
    fy <- liftIO $ isFile localFileYackage
    let handleFile localFile noChanges = do
            debug $ "Comparing: " ++ show (fp, localFile)
            isDiff <- compareTGZ localFile fp
            return $ if isDiff then NeedsVersionBump else noChanges
    status <-
        case () of
            ()
                | fh -> handleFile localFileHackage NoChanges
                | fy -> handleFile localFileYackage OnlyOnYackage
                | otherwise -> do
                    reqH <- getUrlHackage package
                    resH <- C.runResourceT $ httpLbs reqH { rawBody = True, checkStatus = \_ _ -> Nothing } m
                    case () of
                        ()
                            | responseStatus resH == status404 || L.length (responseBody resH) == 0 -> do
                                liftIO $ debug $ "Not found on Hackage: " ++ show fp
                                reqY <- getUrlYackage package
                                resY <- C.runResourceT $ httpLbs reqY { checkStatus = \_ _ -> Nothing } m
                                case () of
                                    ()
                                        | responseStatus resY == status404 || L.length (responseBody resY) == 0 -> do
                                            liftIO $ debug $ "Not found on Yackage: " ++ show fp
                                            return DoesNotExist
                                        | responseStatus resY == status200 -> liftIO $ do
                                            createTree $ directory localFileYackage
                                            L.writeFile (encodeString localFileYackage) $ responseBody resY
                                            handleFile localFileYackage OnlyOnYackage
                                        | responseStatus resY == status502 -> do
                                            debug $ "Yackage isn't running"
                                            return DoesNotExist
                                        | otherwise -> error $ "Invalid status code: " ++ show (responseStatus resY)
                            | responseStatus resH == status200 -> do
                                createTree $ directory localFileHackage
                                L.writeFile (encodeString localFileHackage) $ responseBody resH
                                handleFile localFileHackage NoChanges
                            | otherwise -> error $ "Invalid status code: " ++ show (responseStatus resH)
    return $ Map.singleton status $ Set.singleton fp

data Package = Package String String

parsePackage :: String -> Package
parsePackage s =
    Package a b
  where
    s' = reverse s
    (b', a') = break (== '-') s'
    a = reverse $ drop 1 a'
    b = reverse b'

getHackageFile :: Package -> IO FilePath
getHackageFile (Package a b) = do
    cache <- getAppCacheDirectory "sdist-check"
    return $ cache </> "hackage" </> decodeString (concat [a, "-", b, ".tar.gz"])

getYackageFile :: Package -> IO FilePath
getYackageFile (Package a b) = do
    cache <- getAppCacheDirectory "sdist-check"
    return $ cache </> "yackage" </> decodeString (concat [a, "-", b, ".tar.gz"])

compareTGZ :: FilePath -> FilePath -> IO Bool
compareTGZ a b = {- FIXME catcher $ -} do
    a' <- getContents a
    b' <- getContents b
    return $ a' /= b'
  where
    -- catcher = handle (\SomeException{} -> debug (show ("compareTGZ" :: String, a, b)) >> return True)
    getContents fp = do
        lbs <- L.readFile (encodeString fp)
        ebss <- try $ C.runResourceT $ CL.sourceList (L.toChunks lbs) C.$$ ungzip C.=$ CL.consume
        case ebss of
            Left (e :: SomeException) -> do
                putStrLn $ concat
                    [ "Error opening tarball: "
                    , encodeString fp
                    , ", "
                    , show e
                    ]
                return Map.empty
            Right bss -> do
                l <- toList $ Tar.read $ L.fromChunks bss
                return $ Map.unions $ map go' l
    toList (Tar.Next e es) = do
        l <- toList es
        return $ e : l
    toList Tar.Done = return []
    toList (Tar.Fail s) = error $ show s
    go' e =
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> Map.singleton (Tar.entryPath e) lbs
            _ -> Map.empty
