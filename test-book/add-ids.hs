{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Stream.Parse
import Text.XML.Stream.Render
import Data.XML.Types
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS
import Filesystem.Enumerator
import Data.Enumerator (($$), run_, enumList, (=$))
import Data.Enumerator.Binary (iterHandle)
import qualified Data.Enumerator.List as EL
import qualified Data.Set as Set
import Filesystem
import Prelude hiding (FilePath)
import qualified Data.Text as T

main :: IO ()
main = do
    [dir'] <- getArgs
    let dir = decodeString dir'
    run_ $ traverse False dir $$ EL.filter isXML =$ EL.mapM_ fixIds
  where
    isXML fp = hasExtension fp "ditamap" || hasExtension fp "dita"

fixIds :: FilePath -> IO ()
fixIds fp = do
    events <- parseFile_ def (encodeString fp) EL.consume
    let (used, events') = noDups Set.empty events
    let tmp = fp <.> "tmp"
    let events'' = addIds 1 used events'
    withFile tmp WriteMode $ \h -> run_ $ enumList 8 events'' $$ renderBytes def =$ iterHandle h
    rename tmp fp

noDups used [] = (used, [])
noDups used (EventBeginElement name as:rest) =
    (used'', EventBeginElement name as' : rest')
  where
    (used', as') =
        case lookup "id" as of
            Nothing -> (used, as)
            Just [ContentText id']
                | id' `Set.member` used -> (used, filter (\(x, _) -> x /= "id") as)
                | otherwise -> (Set.insert id' used, as)
            Just _ -> error "Unresolved entities in id"
    (used'', rest') = noDups used' rest
noDups used (e:es) =
    (used', e:es')
  where
    (used', es') = noDups used es

addIds _ _ [] = []
addIds i used (EventBeginElement name as:rest) =
    EventBeginElement name as' : addIds i' used' rest
  where
    (i', used', as') =
        case lookup "id" as of
            Just{} -> (i, used, as)
            Nothing -> newId i
    newId i =
        if id' `Set.member` used
            then newId $ i + 1
            else (i, Set.insert id' used, ("id", [ContentText id']) : as)
      where
        id' = T.pack $ "x-" ++ show i
addIds i used (e:es) = e : addIds i used es
