{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar
import Codec.Zlib.Enum (ungzip)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Exception (try, Exception)

getUrl :: Package -> IO (Request IO)
getUrl (Package a b) =
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

main :: IO ()
main = withManager $ \m -> do
    [dir'] <- getArgs
    ss <- listDirectory (decodeString dir') >>= mapM (go m)
    let m = Map.unionsWith Set.union ss
    case Map.lookup NoChanges m of
        Nothing -> putStrLn "No pruning required"
        Just s -> do
            putStrLn "The following packages should be pruned:"
            mapM_ (putStrLn . T.unpack) $ Set.toList s

    putStrLn "\n"

    case Map.lookup NeedsVersionBump m of
        Nothing -> putStrLn "No version bumps required"
        Just s -> do
            putStrLn "The following packages require a version bump:"
            mapM_ (putStrLn . T.unpack) $ Set.toList s

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

go :: Manager -> FilePath -> IO (Map.Map Status (Set.Set T.Text))
go m fp = do
    let base = T.reverse $ T.drop 7 $ T.reverse $ either id id $ toText $ filename fp
    let package = parsePackage $ T.unpack base
    req <- getUrl package
    localFile <- getPackageFile package
    f <- isFile localFile
    let handleFile = do
            isDiff <- compareTGZ localFile fp
            return $ if isDiff then NeedsVersionBump else NoChanges
    status <-
        if f
            then handleFile
            else do
                res <- httpLbsRedirect req { rawBody = True } m
                case statusCode res of
                    404 -> return DoesNotExist
                    200 -> do
                        createTree $ directory localFile
                        L.writeFile (encodeString localFile) $ responseBody res
                        handleFile
                    _ -> error $ "Invalid status code: " ++ show (statusCode res)
    return $ Map.singleton status $ Set.singleton base

data Package = Package String String

parsePackage :: String -> Package
parsePackage s =
    Package a b
  where
    s' = reverse s
    (b', a') = break (== '-') s'
    a = reverse $ drop 1 a'
    b = reverse b'

getPackageFile :: Package -> IO FilePath
getPackageFile (Package a b) = do
    cache <- getAppCacheDirectory "sdist-check"
    return $ cache </> decodeString (concat [a, "-", b, ".tar.gz"])

compareTGZ :: FilePath -> FilePath -> IO Bool
compareTGZ a b = do
    a' <- getContents a
    b' <- getContents b
    return $ a' /= b'
  where
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
            Right bss -> return $ Map.unions $ map go $ toList $ Tar.read $ L.fromChunks bss
    toList (Tar.Next e es) = e : toList es
    toList Tar.Done = []
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
