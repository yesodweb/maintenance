#!/bin/bash -ex

ghc --make extract.hs
./extract $1 tmp

cd tmp
for f in $(find . -name \*.lhs) $(find . -name \*.hs)
do
    ghc -Werror $f --make -optl-lpthread
done
