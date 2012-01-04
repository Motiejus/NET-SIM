test:
	ERL_FLAGS="-config silent_run.config" ./rebar eunit skip_deps=true
