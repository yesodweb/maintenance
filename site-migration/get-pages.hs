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
    let (ext, wrapper) =
            case tiFormat ti of
                TFMarkdown -> ("markdown", noWrapper)
                TFHtml -> ("html", noWrapper)
                TFText -> ("txt", noWrapper)
                TFDitaConcept -> ("dita", concept)
                TFDitaTopic -> ("dita", topic)
    let fp = (if piWiki pi then "wiki" else "page") </> fromText (piName pi) </> "index" <.> ext
    createTree (directory fp)
    writeTextFile fp $ wrapper (tiTitle ti) $ tiContent ti

noWrapper _ t = t
concept title t = T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<!DOCTYPE concept PUBLIC \"-//OASIS//DTD DITA Concept//EN\" \"concept.dtd\">"
    , "<concept id=\"ignored\"><title>"
    , T.concatMap escape title
    , "</title><conbody>"
    , t
    , "</conbody></concept>"
    ]
topic title t = T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    , "<!DOCTYPE topic PUBLIC \"-//OASIS//DTD DITA Concept//EN\" \"topic.dtd\">"
    , "<topic id=\"ignored\"><title>"
    , T.concatMap escape title
    , "</title><body>"
    , t
    , "</body></topic>"
    ]

escape '<' = "&lt;"
escape '>' = "&gt;"
escape '&' = "&amp;"
escape c = T.singleton c
