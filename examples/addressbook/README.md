This directory contains a simple Piqi-RPC service implementation. The service
implements an addressbook that can be manipulated using JSON/XML/Prtotobuf over
plain HTTP.


Files
-----

`src/addressbook.piqi` and `src/person.piqi` -- Piqi-RPC service definitions

`src/addressbook.erl` -- addressbook service implementation

`client.sh` -- shell script making calls to the service started by `server.sh`

`server.sh` -- a script to start the service


Instructions
------------

Building:

    make deps (or rebar get-deps)

    make (or rebar compile)


Running the server:

    ./server.sh


Running some client commands against the started server (on a different
terminal):

    ./client.sh


Or, start the server, run the client commands and shutdown the server in one
command:

    ./test

