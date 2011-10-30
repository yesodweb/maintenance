#!/bin/bash -x

DIR=$(pwd)
. package-list.sh

# Pull newest code, install and run test suites
function update-install {
    if [ -d $1 ]
    then
        cd $DIR/$1 && git pull || exit 1
    else
        git clone https://github.com/yesodweb/$1.git
    fi

    cd $DIR/$1 && git submodule update --init && ./scripts/install || exit 1
}

for p in $PACKAGES
do
    update-install $p
done

# Special yesod scaffolding test
cd $DIR/yesod/yesod && ./test/run.sh || exit 1

$DIR/generate-tarballs.sh
$DIR/build-tarballs.sh
$DIR/check-tarballs.sh
