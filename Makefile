.PHONY: get-deps
get-deps:
	rebar get-deps

.PHONY: compile
compile:
	rebar compile

.PHONY: shell
shell:
	erl \
	-pa ebin deps/*/ebin \
	-eval "application:ensure_all_started(autocompile)."
