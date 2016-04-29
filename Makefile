REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: test

all: $(REBAR3)
	@$(REBAR3) do update, clean, compile, eunit, ct, dialyzer

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

distclean:
	@rm -rf ./_build

edoc:
	@$(REBAR3) edoc

compile:
	@$(REBAR3) compile

clean:
	@$(REBAR3) clean

rel: distclean
	@$(REBAR3) release

tar: distclean
	@$(REBAR3) as prod tar

test:
	@$(REBAR3) do eunit, ct
