#!/bin/bash -x

DIR=$(pwd)
PACKAGES="wai hamlet persistent yesod"

# Pull newest code, install and run test suites
function update-install {
    if [ -d $1 ]
    then
        cd $DIR/$1 && git pull || exit 1
    else
        git clone https://github.com/yesodweb/$1.git
    fi

    cd $DIR/$1 && ./install-all.sh || exit 1
}

for p in $PACKAGES
do
    update-install $p
done

# Special yesod scaffolding test
cd $DIR/yesod/yesod && ./test/run.sh || exit 1

# Generate tarballs
rm -rf $DIR/tarballs
mkdir $DIR/tarballs
for p in $PACKAGES
do
    for d in $DIR/$p/*
    do
        if [ -d $d ]
        then
            if [ -f $d/*.cabal ]
            then
                cd $d
                rm -f dist/*.gz
                # FIXME use cabal check
                cabal sdist || exit 1
                mv dist/*.gz $DIR/tarballs
            fi
        fi
    done
done

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

runghc $DIR/sdist-check.hs $DIR/tarballs
