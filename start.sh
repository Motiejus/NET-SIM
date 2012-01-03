#!/bin/sh

#erl -pa ebin/* +Bd -sname net_sim -boot start_sasl -s netsim start_app
erl -pa ebin/ -boot start_sasl -s netsim start_app
