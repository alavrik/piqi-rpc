REBAR ?= rebar


all:
	$(REBAR) compile
	$(MAKE) -C piqic-erlang-rpc


deps:
	$(REBAR) get-deps


dialyzer: all .dialyzer_deps_plt
	dialyzer --plt .dialyzer_deps_plt ./ebin


.dialyzer_deps_plt:
	cp $$DIALYZER_PLT $@
	dialyzer --add_to_plt --plt $@ \
		deps/piqi/ebin deps/mochiweb/ebin deps/webmachine/ebin


clean:
	$(REBAR) clean
	$(MAKE) -C piqic-erlang-rpc clean


distclean: clean
	rm -rf ebin deps .dialyzer_deps_plt


.PHONY: deps

