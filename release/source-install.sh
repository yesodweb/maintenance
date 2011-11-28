#!/bin/bash -ex

DIR=$(pwd)

for f in $DIR/tarballs/*.tar.gz
do
    cd $DIR
    rm -rf tmp
    mkdir tmp
    cd tmp
    tar zxfv $f
    cd *
    cabal-src-install --src-only
done

cd $DIR
rm -rf tmp

cabal install $(find tarballs/ -type f -exec basename {} .tar.gz \;)
