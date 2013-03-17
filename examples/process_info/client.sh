#!/bin/sh

set -x # -e


if [ -z "$PIQI" ]
then
	PIQI=deps/piqi/priv/piqi-binary/"`uname -s`-`uname -m`"/piqi
fi


$PIQI call http://localhost:8888/process-info -p


$PIQI call http://localhost:8888/process-info -h


$PIQI call -t json http://localhost:8888/process-info/list-processes
curl -v -X POST -H 'Accept: application/json' 'http://localhost:8888/process-info/list-processes'


$PIQI call -t json http://localhost:8888/process-info/process-info -- "<0.12.0>"

curl -v -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '{"pid" : "<0.12.0>"}' 'http://localhost:8888/process-info/process-info'


$PIQI call -t json http://localhost:8888/process-info/list-process-info -- -a -d -m

curl -v -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '["all", "dictionary", "messages"]' 'http://localhost:8888/process-info/list-process-info'

