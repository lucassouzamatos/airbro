#!/usr/bin/env bash

CURRENT=$(dirname $0)

(cd $CURRENT/../ && rebar3 shell logger_level debug)