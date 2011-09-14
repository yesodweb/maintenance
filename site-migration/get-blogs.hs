import Data.Time
import qualified Data.Text as T
import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment (getArgs)

data BI = BI
    { biUTC :: UTCTime
    , biMap :: T.Text
    , biSlug :: T.Text
    }
    deriving (Show, Read)

main = do
    [host] <- getArgs
    bis <- fmap (read . L8.unpack) $ simpleHttp $ host ++ "/extract/blogs"
    writeFile "blog-infos.txt" $ show bis
    writeFile "get-blogs.sh" $ unlines $ "#!/bin/bash -ex" : map (go host) bis

go host bi = unlines
    [ "curl " ++ host ++ "/show/map/" ++ T.unpack (biMap bi) ++ "/download > map-" ++ T.unpack (biMap bi) ++ ".zip"
    , concat
        [ "../zip-to-folder map-"
        , T.unpack (biMap bi)
        , " "
        , show year
        , "/"
        , pad $ show month
        , "/"
        , T.unpack $ biSlug bi
        ]
    , "rm map-" ++ T.unpack (biMap bi) ++ ".zip"
    ]
  where
    (year, month, _) = toGregorian $ utctDay $ biUTC bi
    pad [x] = ['0', x]
    pad s = s
