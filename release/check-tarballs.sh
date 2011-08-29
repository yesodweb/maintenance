#!/bin/bash -x

runghc sdist-check.hs tarballs || (cabal install tar safe filesystem-enumerator && runghc sdist-check.hs tarballs)
