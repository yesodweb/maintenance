{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import qualified Data.Text as T
import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment (getArgs)
import Filesystem (listDirectory)
import Filesystem.Path.CurrentOS ((</>), decodeString, filename, hasExtension, encodeString)
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
    writeFile "blog-infos.sql" $ unlines $ "BEGIN TRANSACTION;" : sql' ++ ["ROLLBACK;"]

sql bi = do
    [file] <- fmap (filter $ flip hasExtension "ditamap") $ listDirectory $ "blogs" </> decodeString folder
    X.Document _ (X.Element _ _ (X.NodeElement (X.Element _ _ [X.NodeContent title]):_)) _ <- X.readFile_ X.def $ encodeString file
    let s = concat
            [ "INSERT INTO \"Blog\" (posted, contents, slug, year, month, author, title) VALUES('"
            , show $ biUTC bi
            , "','home/1/blogs/"
            , folder -- FIXME
            , encodeString $ filename file
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
