PROJECT_DIR:=$(strip $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))

all:
	@./rebar compile

clean:
	@./rebar clean
	@find $(PROJECT_DIR)/. -name "erl_crash\.dump" | xargs rm -f
	@find $(PROJECT_DIR)/. -name "*.beam" | xargs rm -f

get-deps: clean
	@./rebar get-deps
	@find $(PROJECT_DIR)/deps/ -name ".git" | xargs rm -rf

dialyze:
	@dialyzer -n -c $(PROJECT_DIR)/src/*.erl
