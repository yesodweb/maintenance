#!/bin/bash -x

rm -rf to-release && mkdir to-release
runghc sdist-check.hs tarballs || (cabal install tar safe filesystem-enumerator && runghc sdist-check.hs tarballs)
