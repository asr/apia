#! /bin/bash

# Tested with tptp4X from TPTP 6.2.0.

# tptp4X yields a failure exit code when it find an error.

tptp4X error.tptp

exitcode=$?

if [[ exitcode != 0 ]] ; then
    echo "Exit code from tptp4X after an error: $exitcode"
fi

# tptp4X *does not* yield a failure exit code when it finds a warning.

tptp4X -w warning.tptp

exitcode=$?

if [[ exitcode != 0 ]] ; then
    echo "Exit code from tptp4X after a warning: $exitcode"
fi
