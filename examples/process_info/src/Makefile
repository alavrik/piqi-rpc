
all:
	cd ..; rebar compile skip_deps=true


clean:
	cd ..; rebar clean skip_deps=true


PIQIC = $(REBAR_DEPS_DIR)/piqi/priv/bin/piqic-erlang
PIQIC_FLAGS = #--trace


# only needed for running tests inside the piqi-rpc project
ifeq ($(wildcard $(PIQIC)),)
PIQIC = ../../../deps/piqi/priv/bin/piqic-erlang
endif
ifeq ($(wildcard $(PIQIC)),)
PIQIC = ../../../_build/default/lib/piqi/priv/bin/piqic-erlang
endif


PIQI_FILES =
PIQI_RPC_FILES = process_info.piqi


ALL_PIQI_FILES = $(PIQI_FILES) $(PIQI_RPC_FILES)

# strip ".proto" suffix in ".proto.piqi" file extension
PIQI_NOPROTO_FILES = $(ALL_PIQI_FILES:%.proto.piqi=%.piqi)

PIQI_ERL_FILES = $(PIQI_NOPROTO_FILES:%.piqi=%_piqi.erl)
PIQI_HRL_FILES = $(PIQI_NOPROTO_FILES:%.piqi=%_piqi.hrl)

PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


piqi: $(PIQI_ERLANG_FILES)


$(PIQI_ERLANG_FILES): $(PIQI_FILES) $(PIQI_RPC_FILES) $(PIQIC)
	set -e; \
	for i in $(PIQI_FILES); do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done
	set -e; \
	for i in $(PIQI_RPC_FILES); do \
		$(PIQIC) $(PIQIC_FLAGS) --gen-rpc --gen-defaults $$i; \
	done


piqi-clean:
	rm -f $(PIQI_ERLANG_FILES)
