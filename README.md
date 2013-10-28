[![Build Status](https://travis-ci.org/alavrik/piqi-rpc.png)](https://travis-ci.org/alavrik/piqi-rpc)


Piqi-RPC is an RPC-over-HTTP system for Erlang.


Piqi-RPC gives Erlang developers a convenient and reliable way to connect
services written in Erlang with the rest of the world using JSON, XML or Google
Protocol Buffers over plain HTTP.

Basically, Piqi-RPC can be viewed as an HTTP gateway for Erlang functions. Any
standard HTTP client can communicate with them not knowing anything about
Erlang or Piqi-RPC. And the opposite is also true: Erlang functions work with
idiomatic Erlang data structures and don’t have to be aware of HTTP or even
Piqi-RPC.

In other words, Piqi-RPC system automates several areas where Erlang developers
usually need to write (and maintain!) a lot of boring plumbing code:

**Parsing and validating input parameters** -- Piqi-RPC automatically parses
XML, JSON, Protocol Buffers and command-line arguments, validates them and
converts to idiomatic Erlang data structures.

**Generating output parameters** -- again, Piqi-RPC automatically generates XML,
JSON and Protocol Buffers from native Erlang representation.

**HTTP communication** -- Piqi-RPC provides fully-compliant HTTP communication
layer. It receives and validates HTTP requests, sends responses, handles
"Content-Type" and "Accept" headers and generates around 10 different meaningful
HTTP response types for various error conditions that can occur during request
execution.

Full description and documentation are available on the project homepage here:
http://piqi.org/doc/piqi-rpc/

The master copy of the documentation page is located in this repository:
[doc/piqi-rpc.md](doc/piqi-rpc.md)


Examples
--------

See [examples/addressbook](examples/addressbook/) and
[examples/process_info](examples/process_info/).


Bugs
----

Please report found problems using [GitHub
issues](http://github.com/alavrik/piqi-rpc/issues).


Mailing list
------------

http://groups.google.com/group/piqi


Contributing
------------

Your contributions are always welcome. Just open a pull request.

Some useful commands:

    make deps  # the same as "rebar get-deps"
    make       # the same as "rebar compile"

    make test
    make dialyzer
    make -C examples/addressbook deps all test


License
-------

[Apache License Version 2.0](LICENSE)

