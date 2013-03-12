#!/bin/sh


PA="-pa ebin"
export ERL_LIBS="deps"


RUN='piqi_rpc:add_service({process_info_example, process_info_piqi_rpc, "process-info"}).'


erl $PA -noshell -s piqi_rpc start -eval "$RUN"

