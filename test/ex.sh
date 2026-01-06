#!/bin/sh

# 引数チェック
if [ $# -ne 1 ]; then
  echo "Usage: $0 {atan|sin|cos}"
  exit 1
fi

FUNC=$1

# コンパイル
ocamlopt -O3 -I +unix unix.cmxa check.ml -o check

# 実行
./check "$FUNC" -domain-count 8

