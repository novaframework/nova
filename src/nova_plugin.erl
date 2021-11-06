%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2020, Niclas Axelsson
%%% @doc
%%% Plugins can be run at two different times; either in the beginning or at
%% the end of a request. They can modify both the actual request or the nova-state.
%% A plugin is implemented with the <icode>nova_plugin</icode> behaviour
%%% and needs to implement three different functions: <icode>pre_request/2</icode>,
%% <icode>post_request/2</icode> and <icode>plugin_info/0</icode>.
%%%
%%%
%%%
%%%
%%% To register the plugin above you have to call
%%  <icode>nova_plugin:register_plugin(RequestType, http, example_plugin).</icode> in order
%%% to run it. <icode>RequestType</icode> can either be <icode>pre_request</icode> or
%%  <icode>post_request</icode>.
%%% @end
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_plugin).

-type request_type() :: pre_request | post_request.
-export_type([request_type/0]).

%% Define the callback functions for HTTP-plugins
-callback pre_request(State :: nova:state(), Options :: map()) ->
    {ok, State0 :: nova:state()} |
    {break, State0 :: nova:state()} |
    {stop, State0 :: nova:state()} |
    {error, Reason :: term()}.
-optional_callbacks([pre_request/2]).

-callback post_request(State :: nova:state(), Options :: map()) ->
    {ok, State0 :: nova:state()} |
    {break, State0 :: nova:state()} |
    {stop, State0 :: nova:state()} |
    {error, Reason :: term()}.
-optional_callbacks([post_request/2]).

-callback plugin_info() -> {Title :: binary(),
                            Version :: binary(),
                            Author :: binary(),
                            Description :: binary(),
                            Options :: [{Key :: atom(), OptionDescription :: binary()}]}.
