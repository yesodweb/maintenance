{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS (fromText, (</>), (<.>), directory)
import Filesystem
import qualified Data.Text as T

data PI = PI
    { piName :: T.Text
    , piTopic :: T.Text
    , piWiki :: Bool
    }
    deriving (Show, Read)

data TopicFormat = TFMarkdown | TFHtml | TFText | TFDitaConcept | TFDitaTopic
    deriving (Read, Eq, Show)

data TI = TI
    { tiTitle :: T.Text
    , tiFormat :: TopicFormat
    , tiContent :: T.Text
    }
    deriving (Show, Read)

main = do
    [host] <- getArgs
    pis <- fmap (read . L8.unpack) $ simpleHttp $ host ++ "/extract/pages"
    mapM_ (go host) pis

go host pi = do
    ti <- fmap (read . L8.unpack) $ simpleHttp $ host ++ "/extract/topic/" ++ (T.unpack $ piTopic pi)
    let ext =
            case tiFormat ti of
                TFMarkdown -> "markdown"
                TFHtml -> "html"
                TFText -> "txt"
                TFDitaConcept -> error "dita concept"
                TFDitaTopic -> error "dita topic"
    let fp = (if piWiki pi then "wiki" else "page") </> fromText (piName pi) </> "index" <.> ext
    createTree (directory fp)
    writeTextFile fp $ tiContent ti
