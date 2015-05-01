#!/bin/bash

cabal sandbox init
cabal install --only-dep --enable-tests
cabal build

