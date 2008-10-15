VSN          := 0.1
ERL          ?= erl
EBIN_DIRS    := $(wildcard lib/*/ebin)
APP          := ejango

all: erl ebin/$(APP).app

erl: ebin lib lib/eunit
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv `find ebin -type f -iname \*.beam` ebin/$(APP).app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@

ebin:
	@mkdir ebin

lib:
	@mkdir lib

lib/eunit:
	@echo "You need to symlink eunit to your lib directory before we can compile."
	@echo "'ln -s Path/To/eunit lib/eunit' before we can compile."
	@false

dialyzer: erl
	@dialyzer -c ebin
