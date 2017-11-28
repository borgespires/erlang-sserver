#!/bin/bash

rebar3 compile
erl -env ERL_LIBS _build/default/lib -eval 'application:ensure_all_started(sserver).' -noshell