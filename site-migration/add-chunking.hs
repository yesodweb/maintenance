{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (writeFile)
import Text.XML
import System.Environment

main = getArgs >>= mapM_ go

go fp = do
    Document a (Element r as1 (title:NodeElement (Element "topicref" as2 ns2):ns1)) b <- readFile_ def fp
    let as2' = ("chunk", "to-content") : as2
    writeFile def fp $ Document a (Element r as1 (title:NodeElement (Element "topicref" as2' ns2):ns1)) b
