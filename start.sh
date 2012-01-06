#!/bin/sh

erl -config silent_run.config -pa ebin/ -pa deps/*/ebin/ -boot start_sasl -s netsim start_app
