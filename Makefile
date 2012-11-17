# Feel free to use, reuse and abuse the code in this file.

REBAR ?= ./rebar

.PHONY: all app dist-clean \
	get-deps clean ct eunit

all:	app

app:	get-deps	
	@${REBAR} compile

compile-app-only:
	@${REBAR} compile skip_deps=true

get-deps:
	@${REBAR} get-deps

clean:
	@${REBAR} clean
	rm -f erl_crash.dump

ct:
	@${REBAR} --config rebar-test.config ct skip_deps=true

eunit:
	@${REBAR} --config rebar-test.config eunit skip_deps=true

dist-clean: clean
	@${REBAR} delete-deps
