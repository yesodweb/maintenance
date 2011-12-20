{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.HTTP.Enumerator
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)
import qualified Data.Text as T
import System.PosixCompat.Files
import Safe
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (unless, when, forM_)
import qualified Data.ByteString.Lazy as L
import Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar
import Codec.Zlib.Enum (ungzip)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Exception (try, Exception, SomeException (..), handle)

debug :: String -> IO ()
#ifdef DEBUG
debug = putStrLn
#else
debug = const $ return ()
#endif

getUrlHackage :: Package -> IO (Request IO)
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

getUrlYackage :: Package -> IO (Request IO)
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
main = withManager $ \m -> do
    (dir':args) <- getArgs
    let toPrune = args == ["--prune"]
    ss <- listDirectory (decodeString dir') >>= mapM (go m)
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
    localFileHackage <- getHackageFile package
    localFileYackage <- getYackageFile package
    fh <- isFile localFileHackage
    fy <- isFile localFileYackage
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
                    resH <- httpLbsRedirect reqH { rawBody = True } m
                    case () of
                        ()
                            | statusCode resH == 404 || L.length (responseBody resH) == 0 -> do
                                debug $ "Not found on Hackage: " ++ show fp
                                reqY <- getUrlYackage package
                                resY <- httpLbsRedirect reqY m
                                case () of
                                    ()
                                        | statusCode resY == 404 || L.length (responseBody resY) == 0 -> do
                                            debug $ "Not found on Yackage: " ++ show fp
                                            return DoesNotExist
                                        | statusCode resY == 200 -> do
                                            createTree $ directory localFileYackage
                                            L.writeFile (encodeString localFileYackage) $ responseBody resY
                                            handleFile localFileYackage OnlyOnYackage
                                        | statusCode resY == 502 -> do
                                            debug $ "Yackage isn't running"
                                            return DoesNotExist
                                        | otherwise -> error $ "Invalid status code: " ++ show (statusCode resY)
                            | statusCode resH == 200 -> do
                                createTree $ directory localFileHackage
                                L.writeFile (encodeString localFileHackage) $ responseBody resH
                                handleFile localFileHackage NoChanges
                            | otherwise -> error $ "Invalid status code: " ++ show (statusCode resH)
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
    catcher = handle (\SomeException{} -> debug (show ("compareTGZ" :: String, a, b)) >> return True)
    getContents fp = do
        lbs <- L.readFile (encodeString fp)
        ebss <- try' $ E.run $ E.enumList 8 (L.toChunks lbs) E.$$ ungzip E.=$ EL.consume
        case ebss of
            Left e -> do
                putStrLn $ concat
                    [ "Error opening tarball: "
                    , encodeString fp
                    , ", "
                    , show e
                    ]
                return Map.empty
            Right bss -> do
                l <- toList $ Tar.read $ L.fromChunks bss
                return $ Map.unions $ map go l
    toList (Tar.Next e es) = do
        l <- toList es
        return $ e : l
    toList Tar.Done = return []
    toList (Tar.Fail s) = error s
    go e =
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> Map.singleton (Tar.entryPath e) lbs
            _ -> Map.empty

try' :: Exception e => IO (Either e a) -> IO (Either e a)
try' f = do
    eex <- try f
    case eex of
        Left e -> return $ Left e
        Right (Left e) -> return $ Left e
        Right (Right a) -> return $ Right a
