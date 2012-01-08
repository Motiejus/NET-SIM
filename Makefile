.PHONY: all

all: deps ebin

deps:
	./rebar get-deps

ebin: src/* deps
	./rebar compile
