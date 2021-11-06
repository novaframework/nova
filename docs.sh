#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang 24.0.2
#     asdf local erlang 24.0.2

rebar3 compile
rebar3 as docs edoc
version=0.8.1
ex_doc "nova" $version "_build/default/lib/nova/ebin" \
  --source-ref v${version} \
  --config docs.config $@