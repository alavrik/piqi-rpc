REBAR ?= rebar


all:
	$(REBAR) compile
	$(MAKE) -C piqic-erlang-rpc


piqic-erlang-rpc: priv/bin/piqic-erlang-rpc


# we need this so that stubs get rebuilt on compiler changes
priv/bin/piqic-erlang-rpc: $(REBAR_DEPS_DIR)/piqi/priv/bin/piqic-erlang ebin/piqic_erlang_rpc.beam
	touch $@


deps:
	$(REBAR) get-deps


test:
	$(REBAR) eunit skip_deps=true


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


.PHONY: deps piqic-erlang-rpc

