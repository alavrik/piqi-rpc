#!/bin/sh


export ERL_LIBS="../../deps:../../_build/default/lib"
export ERL_FLAGS="-pz ../../ebin -pz ebin -pz _build/default/lib/piqi_rpc_process_info/ebin"


RUN='piqi_rpc:add_service({process_info_example, process_info_piqi, "process-info"}).'


erl -noshell -s piqi_rpc start -eval "$RUN"

