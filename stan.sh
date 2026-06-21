#!/usr/bin/env bash

# cabal update
# cabal install stan

export STAN_USE_DEFAULT_CONFIG=True
stan -s --hide-solution report
