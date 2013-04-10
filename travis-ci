#!/bin/sh -xe

export REBAR="`pwd`/rebar"

make

make test
make -C examples/addressbook deps all test
