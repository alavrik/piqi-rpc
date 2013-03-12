This is a an example Piqi-RPC Erlang server that provides JSON/XML/Protobuf over
plain HTTP access to Erlang's `erlang:process_info/2` function. This function
exposes various information about processes running inside Erlang VM.


Files
-----

`src/process_info.piqi` -- Piqi-RPC service definition

`src/process_info_example.erl` -- service implementation

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

