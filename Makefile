.PHONY: deps test

REBAR := ./rebar

all:
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) compile
	@$(REBAR) xref skip_deps=true
	@$(REBAR) eunit skip_deps=true
compile:
	@$(REBAR) compile skip_deps=true
xref:
	@$(REBAR) xref skip_deps=true
eunit:
	@$(REBAR) eunit skip_deps=true
typer:
	typer --plt $(PLT_FILE) -I include/ -r src/
doc: compile
	@$(REBAR) doc
clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean
