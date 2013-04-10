REBAR ?= rebar

# workaround for a rebar bug
unexport ERL_LIBS


all:
	$(REBAR) compile


deps:
	cd deps; for i in ../../../deps/*; do ln -sf $$i; done
	$(REBAR) get-deps


test:
	./test


#dialyzer: all
#	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean skip_deps=true
	rm -f addressbook person.piq.json


distclean: clean
	#$(REBAR) delete-deps
	cd deps; for i in ../../../deps/*; do rm -f `basename $$i`; done
	rm -rf ebin #deps


.PHONY: test deps

