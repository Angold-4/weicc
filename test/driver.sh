#!/bin/bash

tmp=`mktemp -d /tmp/weicc-test-XXXXXX`

# finially remove the $tmp dir (when Interrupt, Terminate, Exit, HUP)
trap 'rm -rf $tmp' INT TERM HUP EXIT

echo > $tmp/empty.c

check() {
  if [ $? -eq 0 ]; then
    echo "testing $1 ... passed"
  else
    echo "testing $1 ... failed"
    exit 1
  fi
}

# -o
rm -f $tmp/out
./weicc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
./weicc --help 2>&1 | grep -q weicc
check --help

echo OK
