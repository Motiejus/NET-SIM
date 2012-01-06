#!/bin/sh

#erl -pa ebin/* +Bd -sname net_sim -boot start_sasl -s netsim start_app
erl -config silent_run.config -pa ebin/ -pa deps/*/ebin/ -boot start_sasl -s netsim start_app
