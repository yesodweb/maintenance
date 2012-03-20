#!/bin/bash -x

rm -rf to-release && mkdir to-release
cabal-dev install
./dist/build/sdist-check/sdist-check tarballs
