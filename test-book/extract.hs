{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Filesystem
import Filesystem.Path.CurrentOS
import qualified Text.XML.Enumerator.Resolved as X
import System.Environment (getArgs)
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when, unless, filterM)
import qualified Data.Set as Set
import Data.List (sort)

type M = RWST FilePath (Set.Set FilePath) Int IO

main :: IO ()
main = do
    x <- getArgs
    case x of
        [a, b] -> do
            let infolder = decodeString a
                outfolder = decodeString b
            createTree outfolder
            ditas <- listDirectory infolder >>= filterM isDita
            ((), _, files) <- runRWST (mapM_ goFile $ sort ditas) outfolder 1
            listDirectory outfolder >>= mapM_ (\fp -> unless (fp `Set.member` files || not (hasExtension fp "hs")) $ do
                removeFile fp
                putStrLn $ "Pruning: " ++ encodeString fp)
        _ -> error $ "Usage: extract <infolder> <outfolder>"

isDita :: FilePath -> IO Bool
isDita fp = if hasExtension fp "dita" then isFile fp else return False

go :: FilePath -> M ()
go fp = do
    f <- liftIO $ isFile fp
    if f
        then goFile fp
        else do
            d <- liftIO $ isDirectory fp
            if d then liftIO (listDirectory fp) >>= mapM_ go else return ()

goFile :: FilePath -> M ()
goFile fp = do
    edoc <- liftIO $ X.readFile (encodeString fp) X.decodeEntities
    case edoc of
        Left{} -> return ()
        Right (X.Document _ (X.Element _ _ ns) _)->
            mapM_ (goNode $ basename fp) ns

goNode :: FilePath -> X.Node -> M ()
goNode base (X.NodeElement (X.Element "codeblock" as [X.NodeContent t]))
    | lookup "outputclass" as == Just "haskell" && hasMain t = do
        dir <- ask
        i <- get
        let i' = i + 1
        put i'
        let fn = T.concat [T.pack $ show i, "-", either id id $ toText base]
        let fp = dir </> fromText fn <.> "hs"
        f <- liftIO $ isFile fp
        toWrite <-
            if f
                then do
                    t' <- liftIO $ TIO.readFile (encodeString fp)
                    return $ t /= t'
                else return True
        when toWrite $ liftIO $ do
            putStrLn $ "Writing: " ++ encodeString fp
            TIO.writeFile (encodeString fp) t
        tell $ Set.singleton fp
    | otherwise =
        case saveFile t of
            Nothing -> return ()
            Just fp -> do
                dir <- ask
                liftIO $ TIO.writeFile (encodeString $ dir </> fp) t
goNode base (X.NodeElement (X.Element _ _ ns)) = mapM_ (goNode base) ns
goNode _ _ = return ()

saveFile :: T.Text -> Maybe FilePath
saveFile t =
    case T.lines t of
        a:_
            | "-- @" `T.isPrefixOf` a -> Just $ fromText $ T.drop 4 a
        _ -> Nothing

hasMain :: T.Text -> Bool
hasMain = any (T.isPrefixOf "main =") . T.lines
