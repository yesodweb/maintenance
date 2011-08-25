{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Filesystem
import Filesystem.Path.CurrentOS
import qualified Text.XML.Enumerator.Resolved as X
import System.Environment (getArgs)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (unless)

main :: IO ()
main = do
    x <- getArgs
    case x of
        [a, b] -> do
            let infolder = decodeString a
                outfolder = decodeString b
            createTree outfolder
            evalStateT (go infolder) (outfolder, 1)
        _ -> error $ "Usage: extract <infolder> <outfolder>"

go :: FilePath -> StateT (FilePath, Int) IO ()
go fp = do
    f <- liftIO $ isFile fp
    if f
        then goFile fp
        else do
            d <- liftIO $ isDirectory fp
            if d then liftIO (listDirectory fp) >>= mapM_ go else return ()

goFile :: FilePath -> StateT (FilePath, Int) IO ()
goFile fp = do
    edoc <- liftIO $ X.readFile (encodeString fp) X.decodeEntities
    case edoc of
        Left{} -> return ()
        Right (X.Document _ (X.Element _ _ ns) _)->
            mapM_ (goNode $ basename fp) ns

goNode :: FilePath -> X.Node -> StateT (FilePath, Int) IO ()
goNode base (X.NodeElement (X.Element "codeblock" as [X.NodeContent t]))
    | lookup "outputclass" as == Just "haskell" = do
        (dir, i) <- get
        let i' = i + 1
        put (dir, i')
        let fn = T.concat [T.pack $ show i, "-", either id id $ toText base]
        let fp = dir </> fromText fn <.> "hs"
        t' <- liftIO $ TIO.readFile (encodeString fp)
        unless (t == t') $ liftIO $ TIO.writeFile (encodeString fp) t
goNode base (X.NodeElement (X.Element _ _ ns)) = mapM_ (goNode base) ns
goNode _ _ = return ()
