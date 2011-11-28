#!/bin/bash -x

DIR=$(pwd)

# Add a missing package or two
cabal install hint || exit 1

# Inspired by http://neilmitchell.blogspot.com/2010/10/enhanced-cabal-sdist.html
for f in $DIR/tarballs/*.tar.gz
do
    cd $DIR
    rm -rf tmp
    mkdir tmp
    cd tmp
    tar zxfv $f && cd * && cabal configure --enable-tests && cabal build && cabal test && cabal check && cabal haddock --executables || exit 1
done
