#!/usr/bin/env bash

mkdir_and_cp() {
  mkdir -p $(dirname "$2") && cp -rf "$1" "$2"
}

mkdir_and_cp dist-newstyle/build/*/*/*/doc/html/helio/ docs/reports/helio/
mkdir_and_cp dist-newstyle/build/*/*/*/t/helio-test/hpc/vanilla/html/ docs/reports/helio-test/
