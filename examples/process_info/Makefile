REBAR ?= rebar


# only needed for running tests inside the piqi-rpc project
export ERL_LIBS = $(realpath ../../deps):$(realpath ../../_build/default/lib)


all:
	$(REBAR) compile


deps:
	$(REBAR) get-deps


test:
	./test


#dialyzer: all
#	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean skip_deps=true


distclean: clean
	rm -rf ebin _build rebar.lock


.PHONY: test deps

