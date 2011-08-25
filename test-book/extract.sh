ghc --make extract.hs && ./extract $1 tmp || exit 1

for f in tmp/*.hs
do
    ghc -Werror $f -lpthread || exit 1
done
