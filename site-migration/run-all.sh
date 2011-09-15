#!/bin/bash -ex
HOST="http://www.yesodweb.com"

ghc --make zip-to-folder.hs

rm -rf book
curl $HOST/show/map/1/download > map-1.zip
./zip-to-folder map-1 book
rm map-1.zip
runghc add-chunking.hs book/yesod-web-framework-book/*.ditamap

rm -rf wiki page
runghc get-pages.hs $HOST

runghc get-blogs.hs $HOST
rm -rf blogs
mkdir blogs
cd blogs
bash -ex ../get-blogs.sh
cd ..
rm get-blogs.sh

runghc blog-sql.hs
rm blog-infos.txt

rm zip-to-folder zip-to-folder.o zip-to-folder.hi

rm -rf home
mkdir -p home/1
mv blogs book home/1
