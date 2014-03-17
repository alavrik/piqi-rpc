
all:
	cd ..; rebar compile skip_deps=true


clean:
	cd ..; rebar clean skip_deps=true


PIQIC = $(REBAR_DEPS_DIR)/piqi/priv/bin/piqic-erlang
PIQIC_RPC = $(REBAR_DEPS_DIR)/piqi_rpc/priv/bin/piqic-erlang-rpc

PIQIC_FLAGS = #--trace


PIQI_FILES = person.piqi
PIQI_RPC_FILES = addressbook.piqi


# strip ".proto" suffix in ".proto.piqi" file extension
PIQI_NOPROTO_FILES = $(PIQI_FILES:%.proto.piqi=%.piqi)

PIQI_ERL_FILES = $(PIQI_NOPROTO_FILES:%.piqi=%_piqi.erl)
PIQI_HRL_FILES = $(PIQI_NOPROTO_FILES:%.piqi=%_piqi.hrl)

PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


PIQI_RPC_ERL_FILES = \
	$(PIQI_RPC_FILES:%.piqi=%_piqi.erl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_rpc.erl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_default_impl.erl)

PIQI_RPC_HRL_FILES = \
	$(PIQI_RPC_FILES:%.piqi=%_piqi.hrl) \
	$(PIQI_RPC_FILES:%.piqi=%_piqi_impl.hrl)

PIQI_RPC_ERLANG_FILES = $(PIQI_RPC_ERL_FILES) $(PIQI_RPC_HRL_FILES)


piqi: $(PIQI_ERLANG_FILES) $(PIQI_RPC_ERLANG_FILES)


$(PIQI_ERLANG_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


$(PIQI_RPC_ERLANG_FILES): $(PIQI_RPC_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC_RPC) $(PIQIC_FLAGS) $$i; \
	done


piqi-clean:
	rm -f $(PIQI_ERLANG_FILES)
	rm -f $(PIQI_RPC_ERLANG_FILES)
