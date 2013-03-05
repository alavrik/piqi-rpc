REBAR ?= rebar


all:
	$(REBAR) compile
	$(MAKE) -C piqic-erlang-rpc


deps:
	$(REBAR) get-deps


clean:
	$(REBAR) clean
	$(MAKE) -C piqic-erlang-rpc clean


distclean: clean
	rm -rf ebin deps


.PHONY: deps

