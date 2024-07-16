#!/bin/sh

delete_symlink () {
     rm "$1" 2> /dev/null
}

main () {
    if [ -n "${ATTC_CRAM_TEST_PATH}" ]; then
	echo "Running cram tests in ./$ATTC_CRAM_TEST_PATH"
	ln -sf $ATTC_CRAM_TEST_PATH ./private-cram-tests
	dune runtest
	delete_symlink ./private-cram-tests
    else
	echo "Please set the ATTC_CRAM_TEST_PATH environment variable."
	exit 1
    fi
}

main
