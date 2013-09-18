#! /bin/bash

# tptp4X from TPTP 6.0.0 yields a failure exit code when it find an
# error.

tptp4X error.tptp

exitcode=$?

if [[ exitcode != 0 ]] ; then
    echo "Exit code from tptp4X after an error: $exitcode"
fi

# tptp4X from TPTP 6.0.0 *does not* yield a failure exit code when it
# finds a warning.

tptp4X -w warning.tptp

exitcode=$?

if [[ exitcode != 0 ]] ; then
    echo "Exit code from tptp4X after a warning: $exitcode"
fi
