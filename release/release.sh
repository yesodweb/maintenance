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
echo FOO
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
            cd $d
            rm -f dist/*.gz
            cabal sdist
            mv dist/*.gz $DIR/tarballs
        fi
    done
done

runghc $DIR/../sdist-check/sdist-check.hs $DIR/tarballs
