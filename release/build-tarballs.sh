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
    # FIXME use cabal check and cabal haddock --executables
    tar zxfv $f && cd * && cabal configure && cabal build || exit 1
done
