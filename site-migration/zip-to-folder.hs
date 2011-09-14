{-# LANGUAGE OverloadedStrings #-}
import Codec.Archive.Zip
import Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Prelude hiding (FilePath, writeFile)
import Filesystem.Path.CurrentOS
import Control.Monad.Trans.RWS
import qualified Data.Text as T
import Control.Monad
import Filesystem (createTree)

data Maps = Maps
    { mSrc2Dst :: Map.Map String FilePath
    , mDst2Src :: Map.Map FilePath (T.Text, Document) -- id, contains the unmodified document
    }
    deriving Show

main :: IO ()
main = do
    zipLBS <- L.readFile "map-1.zip"
    files <- fmap Map.unions $ mapM loadEntry $ zEntries $ toArchive zipLBS
    ((), Maps toDst toSrc, ()) <- runRWST (makeMaps id "map-1.ditamap") files (Maps Map.empty Map.empty)
    forM_ (Map.toList toSrc) $ \(fp, (id', Document a (Element root _ ns) b)) -> do
        createTree $ directory fp
        let as = [("id", id')]
        let ar = T.replicate (length $ filter (== '/') $ encodeString fp) "../"
        writeFile def (encodeString fp) $ Document a (fixHrefs ar toDst $ Element root as ns) b

fixHrefs ar toDst (Element e as ns) =
    Element e (addId $ map goA as) (map goN ns)
  where
    addId as' =
        case (e, lookup "href" as') of
            ("topicref", Just href) -> ("id", T.takeWhile (/= '.') $ snd $ T.breakOnEnd "/" href) : as'
            _ -> as'
    goA ("href", t) =
        case Map.lookup (T.unpack fp) toDst of
            Nothing -> ("href", t)
            Just fp' -> ("href", (ar `T.append` either id id (toText fp')) `T.append` suffix)
      where
        (fp, suffix) =
            if "yw://topic/" `T.isInfixOf` t
                then (T.concat ["topic-", snd $ T.breakOnEnd "/" t, ".dita"], "")
                else T.break (== '#') t
    goA a = a
    goN (NodeElement e) = NodeElement $ fixHrefs ar toDst e
    goN n = n

loadEntry e =
    case parseLBS def $ fromEntry e of
        Left a -> error $ show (eRelativePath e, a)
        Right doc -> return $ Map.singleton (eRelativePath e) doc

makeMaps prefix filename = do
    files <- ask
    maybe (error $ "File not found: " ++ filename) (makeMaps' prefix filename) $ Map.lookup filename files

makeMaps' prefix srcfile doc@(Document _ (Element "map" _ ns) _) = do
    title <-
        case ns of
            NodeElement (Element "title" [] [NodeContent t]):_ -> return t
            _ -> error $ "Couldn't get map title in: " ++ srcfile
    let id' = titleToId title
    let dstfile = prefix $ fromText $ T.append id' ".ditamap"
    let prefix' x = prefix $ fromText id' </> x
    addFile srcfile dstfile id' doc
    mapM_ (makeMaps prefix' . T.unpack) $ concatMap allHrefs ns
makeMaps' prefix srcfile doc@(Document _ (Element _ _ ns) _) = do
    title <-
        case ns of
            NodeElement (Element "title" [] [NodeContent t]):_ -> return t
            _ -> error $ "Couldn't get map title in: " ++ srcfile
    let id' = titleToId title
    let dstfile = prefix $ fromText $ T.append id' ".dita"
    addFile srcfile dstfile id' doc

allHrefs (NodeElement (Element e as ns)) = maybe id (:) (lookup "href" as) $ concatMap allHrefs ns
allHrefs _ = []

addFile src dst id' doc = do
    Maps a b <- get
    put $ Maps (Map.insert src dst a) (Map.insert dst (id', doc) b)

titleToId =
    T.concatMap go . T.toLower
  where
    go ' ' = "-"
    go '-' = "-"
    go c
        | 'a' <= c && c <= 'z' = T.singleton c
        | '0' <= c && c <= '9' = T.singleton c
    go _ = ""
