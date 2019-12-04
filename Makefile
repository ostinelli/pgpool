PROJECT_DIR:=$(strip $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))

all:
	@rebar3 compile

clean:
	@rebar3 clean
	@find $(PROJECT_DIR)/. -name "erl_crash\.dump" | xargs rm -f
	@find $(PROJECT_DIR)/. -name "*\.beam" | xargs rm -f
	@find $(PROJECT_DIR)/. -name "*\.so" | xargs rm -f

dialyzer:
	@rebar3 dialyzer

test: all
	ct_run -dir $(PROJECT_DIR)/test -logdir $(PROJECT_DIR)/test/results \
	-pa `rebar3 path`

travis:
	@$(PROJECT_DIR)/rebar3 compile
	ct_run -dir $(PROJECT_DIR)/test -logdir $(PROJECT_DIR)/test/results \
	-pa `$(PROJECT_DIR)/rebar3 path`
