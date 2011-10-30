#!/bin/bash -x

DIR=$(pwd)
. package-list.sh

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
                cabal sdist || exit 1
                mv dist/*.gz $DIR/tarballs
            fi
        fi
    done
done

# Get rid of specific, unused packages
rm $DIR/tarballs/yesod-examples-*.tar.gz $DIR/tarballs/persistent-mongoDB-*.tar.gz $DIR/tarballs/wai-handler-webkit-*.tar.gz $DIR/tarballs/persistent-test-*.tar.gz
