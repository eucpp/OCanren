#!/bin/sh

TEST=$1
EXPECTED=$2
ACTUAL=$3
DIFF=$4

if ! diff -u ${EXPECTED} ${ACTUAL} > ${DIFF}; then
  echo "${TEST}: FAILED (see ${DIFF})"
  exit 1
else
  echo "${TEST}: PASSED"
  exit 0
fi