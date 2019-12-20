#!/bin/bash

# simple script for running test and diffing its result with the expected;
# please, run the script from the toplevel directory of the project

# predefined directory with tests
TESTDIR="regression"

# path to the file containing list of tests
TESTLIST="${TESTDIR}/tests.txt"

# directories to search for tests
SEARCH_DIRS="${TESTDIR} samples"

if test $# = 0; then
    echo "usage: $0 (<test name> | all)"
    exit 1
fi

if [[ $1 == "all" ]]; then
  # read test names from the file
  TESTS=`cat ${TESTLIST}`
else
  # try to find test/sample in predefined directories
  TEST=$1
  FOUND=0

  for DIR in ${SEARCH_DIRS}; do
    TESTDIR=${DIR}
    TESTSRC="${TESTDIR}/${TEST}.ml"
    if [[ -e ${TESTSRC} ]]; then
      FOUND=1
      break
    fi
  done

  if [[ ${FOUND} -eq "0" ]]; then
    echo "cannot find ${TEST}"
    echo "searched in ${SEARCH_DIRS}"
    exit
  fi

  TESTS=${TEST}

fi

for TEST in ${TESTS}; do

  TESTEXE="${TESTDIR}/${TEST}.exe"
  TESTLOG="${TESTDIR}/${TEST}.log"
  TESTDIFF="${TESTDIR}/${TEST}.diff"
  TESTORIG="${TESTDIR}/orig/${TEST}.orig"

  # run the test;
  # we use `dune exec` so that `dune`
  # can build the test and its dependencies
  # if it has not been done yet

  dune exec --no-print-directory ${TESTEXE} > ${TESTLOG}

  # diff test output with the expected and print `PASSED/FAILED` message

  if diff -u ${TESTORIG} ${TESTLOG} > ${TESTDIFF}; then
      echo "${TEST}: PASSED"
      rm -f ${TESTDIFF}
  else
      echo "${TEST}: FAILED (see ${TESTDIFF})"
  fi

done