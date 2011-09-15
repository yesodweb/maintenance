{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import qualified Data.Text as T
import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment (getArgs)
import Filesystem (listDirectory, removeFile, removeDirectory, writeTextFile)
import Filesystem.Path.CurrentOS ((</>), decodeString, filename, hasExtension, encodeString, directory, fromText, (</>), (<.>))
import qualified Text.XML as X

data BI = BI
    { biUTC :: UTCTime
    , biMap :: T.Text
    , biSlug :: T.Text
    }
    deriving (Show, Read)

main = do
    bis <- fmap read $ readFile "blog-infos.txt"
    sql' <- mapM sql bis
    writeFile "blog-infos.sql" $ unlines $ "BEGIN TRANSACTION;" : "DELETE FROM \"Blog\";" : sql' ++ ["ROLLBACK;"]

sql bi = do
    [file] <- fmap (filter $ flip hasExtension "ditamap") $ listDirectory $ "blogs" </> decodeString folder
    X.Document _ (X.Element _ _ (X.NodeElement (X.Element _ _ [X.NodeContent title]):rest)) _ <- X.readFile_ X.def $ encodeString file
    file' <- case rest of
        [X.NodeElement (X.Element "topicref" [("id", _), ("href", href)] [])] -> do
            let ditafile = directory file </> fromText href
            X.Document _ root _ <- X.readFile_ X.def $ encodeString ditafile
            case root of
                X.Element _ _ [_, X.NodeElement (X.Element _ _ [X.NodeElement (X.Element "foreign" [("outputclass", oc)] [X.NodeContent content])])] -> do
                    ext <-
                        case oc of
                            "html" -> return "html"
                            "markdown" -> return "markdown"
                            _ -> error $ "Unknown outputclass: " ++ show oc
                    let newfile = directory file </> fromText (T.map noSlash title) <.> ext
                    writeTextFile newfile content
                    removeFile file
                    removeFile ditafile
                    removeDirectory $ directory ditafile
                    return newfile
                _ -> return file
        _ -> return file
    let s = concat
            [ "INSERT INTO \"Blog\" (posted, contents, slug, year, month, author, title) VALUES('"
            , show $ biUTC bi
            , "','home/1/"
            , concatMap escape $ drop 2 $ encodeString file'
            , "','"
            , T.unpack $ biSlug bi
            , "',"
            , show year
            , ","
            , show month
            , ",1,'"
            , concatMap escape $ T.unpack title
            , "');"
            ]
    return s
  where
    (year, month, _) = toGregorian $ utctDay $ biUTC bi
    pad [x] = ['0', x]
    pad s = s
    folder = concat
        [ show year
        , "/"
        , pad $ show month
        , "/"
        , T.unpack $ biSlug bi
        , "/"
        ]
    escape '\'' = "''"
    escape c = [c]
    noSlash '/' = '-'
    noSlash c = c
