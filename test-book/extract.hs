{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Filesystem
import Filesystem.Path.CurrentOS
import qualified Text.XML as X
import System.Environment (getArgs)
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when, unless, filterM)
import qualified Data.Set as Set
import Data.List (sort)
import Filesystem.Enumerator
import Data.Enumerator (($$), (=$), run_)
import qualified Data.Enumerator.List as EL
import Control.Exception (try, SomeException)

type M = RWST FilePath (Set.Set FilePath) Int IO

main :: IO ()
main = do
    x <- getArgs
    case x of
        [a, b] -> do
            let infolder = decodeString a
                outfolder = decodeString b
            createTree outfolder
            ditas <- run_ $ traverse False infolder $$ EL.filter (flip hasExtension "dita") =$ EL.consume
            ((), _, files) <- runRWST (mapM_ (goFile infolder) $ sort ditas) outfolder 1
            run_ $ traverse False outfolder $$ EL.mapM_ (\fp -> unless (fp `Set.member` files || not (hasExtension fp "hs")) $ do
                removeFile fp
                putStrLn $ "Pruning: " ++ encodeString fp)
        _ -> error $ "Usage: extract <infolder> <outfolder>"

goFile :: FilePath -> FilePath -> M ()
goFile infolder fp = do
    let mrelpath = stripPrefix infolder fp
    relpath <-
        case mrelpath of
            Just x -> return x
            Nothing -> error $ "Invalid result from stripPrefix on: " ++ show (infolder, fp)
    edoc <- liftIO $ try $ X.readFile X.def fp
    case edoc :: Either SomeException X.Document of
        Left{} -> return ()
        Right (X.Document _ (X.Element _ _ ns) _)->
            mapM_ (goNode relpath) ns

goNode :: FilePath -> X.Node -> M ()
goNode src (X.NodeElement (X.Element "codeblock" as [X.NodeContent t]))
    | lookup "outputclass" as == Just "haskell" && hasMain t = writeCodeblock src t "hs"
    | lookup "outputclass" as == Just "lhaskell" && hasMainLit t = writeCodeblock src t "lhs"
    | lookup "outputclass" as == Nothing = error $ "codeblock missing outputclass in: " ++ show src
    | otherwise =
        case saveFile t of
            Nothing -> return ()
            Just fp -> do
                dir <- ask
                liftIO $ createTree $ directory $ dir </> fp
                liftIO $ TIO.writeFile (encodeString $ dir </> fp) $ T.drop 1 $ T.dropWhile (/= '\n') t
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

hasMainLit :: T.Text -> Bool
hasMainLit = any (T.isPrefixOf "> main =") . T.lines

writeCodeblock src t ext = do
    dir <- ask
    i <- get
    let i' = i + 1
    put i'
    let fn = T.concat [T.pack $ show i, "-", either id id $ toText (basename src)]
    let fp = collapse $ dir </> directory src </> fromText fn <.> ext
    f <- liftIO $ isFile fp
    toWrite <-
        if f
            then do
                t' <- liftIO $ TIO.readFile (encodeString fp)
                return $ t /= t'
            else return True
    when toWrite $ liftIO $ do
        putStrLn $ "Writing: " ++ encodeString fp
        liftIO $ createTree $ directory fp
        TIO.writeFile (encodeString fp) t
    tell $ Set.singleton fp
