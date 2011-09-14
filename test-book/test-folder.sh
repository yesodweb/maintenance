ghc --make extract.hs && ./extract $1 tmp || exit 1

cd tmp
for f in *.hs
do
    ghc -Werror $f -lpthread || exit 1
done