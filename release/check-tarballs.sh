#!/bin/bash -x

rm -rf to-release && mkdir to-release
runghc sdist-check.hs tarballs || (cabal install tar safe zlib-conduit unix-compat http-conduit filesystem-conduit && runghc sdist-check.hs tarballs)
