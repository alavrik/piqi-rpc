#!/bin/sh

set -x -e


if [ -z "$PIQI" ]
then
	PIQI=deps/piqi/priv/piqi-binary/"`uname -s`-`uname -m`"/piqi
fi


curl http://localhost:8888/addressbook


$PIQI call http://localhost:8888/addressbook -p


$PIQI call http://localhost:8888/addressbook -h


$PIQI call -t json http://localhost:8888/addressbook/get-person -- 0 || true
curl -v -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '{"id" : 0}' 'http://localhost:8888/addressbook/get-person'


$PIQI call -t json http://localhost:8888/addressbook/list-people
curl -v -X POST -H 'Accept: application/json' 'http://localhost:8888/addressbook/list-people'


$PIQI call -t json http://localhost:8888/addressbook/add-person -- \
    --name "J. Random Hacker" \
    --id 10 \
    --email "j.r.hacker@example.com" \
    --phone [ --number "(111) 123 45 67" ] \
    --phone [ \
        --number "(222) 123 45 67" \
        --mobile \
    ] || true

$PIQI call -t json http://localhost:8888/addressbook/get-person -- 10


$PIQI convert -t json person.piq

curl -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary @person.piq.json 'http://localhost:8888/addressbook/add-person'


$PIQI call -t json http://localhost:8888/addressbook/get-person -- 0
curl -X POST -H 'Accept: application/json' -H 'Content-Type: application/json' --data-binary '{"id" : 0}' 'http://localhost:8888/addressbook/get-person'


echo "ALL TESTS PASSED"

