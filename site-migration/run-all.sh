#!/bin/bash -ex
HOST="http://localhost:3000"

ghc --make zip-to-folder.hs

rm -rf book
curl $HOST/show/map/1/download > map-1.zip
./zip-to-folder map-1 book
rm map-1.zip

runghc get-blogs.hs $HOST
rm -rf blogs
mkdir blogs
cd blogs
bash -ex ../get-blogs.sh
cd ..
rm get-blogs.sh

rm zip-to-folder
