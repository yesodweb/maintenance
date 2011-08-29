rm -rf book.zip book
wget http://www.yesodweb.com/show/map/1/download -O book.zip
mkdir book
cd book
unzip ../book.zip
rm ../book.zip
rm *.ditamap
