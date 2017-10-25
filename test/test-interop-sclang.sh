#!/bin/sh

ML_PORT=12345
SC_PORT=57120

SCLANG=`which sclang`

if [ $? -eq 0 ]
then
    XVFB_RUN=`which xvfb-run`
    if [ $? -eq 0 ]
    then
        $XVFB_RUN ./test_interop_sclang.byte $ML_PORT $SCLANG $SC_PORT test/EchoServer.sc
    else
        ./test_interop_sclang.byte $ML_PORT $SCLANG $SC_PORT test/EchoServer.sc
    fi
else
    echo "couldn't find sclang"
    exit 1
fi
