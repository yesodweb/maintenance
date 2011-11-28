#!/bin/bash -x

rm -rf to-release && mkdir to-release
runghc sdist-check.hs tarballs || (cabal install tar safe zlib-enum unix-compat http-enumerator filesystem-enumerator && runghc sdist-check.hs tarballs)
