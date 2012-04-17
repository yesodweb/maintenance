#!/bin/bash -ex

cabal build || cabal configure && cabal build
./dist/build/extract/extract $1 tmp

cd tmp
for f in $(find . -name \*.lhs) $(find . -name \*.hs)
do
    ghc -Werror $f --make -optl-lpthread
done
