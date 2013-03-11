#!/bin/sh


PA="-pa ebin"
export ERL_LIBS="deps"


RUN='piqi_rpc:add_service({addressbook, addressbook_piqi_rpc, "addressbook"}).'


#rlwrap erl -oldshell $PA #-s piqi_rpc start -eval "$RUN"
erl $PA -noshell -s piqi_rpc start -eval "$RUN"

