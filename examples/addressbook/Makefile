REBAR ?= rebar

# workaround for a rebar bug
unexport ERL_LIBS


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
	rm -f addressbook person.piq.json


distclean: clean
	$(REBAR) delete-deps
	rm -rf ebin deps


.PHONY: test deps

