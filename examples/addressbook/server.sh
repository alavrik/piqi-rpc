#!/bin/sh


export ERL_LIBS="../../deps:../../_build/default/lib"
export ERL_FLAGS="-pz ../../ebin -pz ebin -pz _build/default/lib/piqi_rpc_addressbook/ebin"


RUN='piqi_rpc:add_service({addressbook, addressbook_piqi, "addressbook"}).'


#rlwrap erl -oldshell $PA #-s piqi_rpc start -eval "$RUN"
erl -noshell -s piqi_rpc start -eval "$RUN"

