%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2020, Niclas Axelsson
%%% @doc
%%% Plugins can be run at two different times; either in the beginning or at
%%% the end of a request. They can modify both the actual request or the nova-state.
%%% A plugin is implemented with the nova_plugin behaviour
%%% and needs to implement three different functions: pre_request/2,
%%% post_request/2 and plugin_info/0.
%%%
%%% A plugin can return either {ok, NewState}, {break, NewState},
%%% {stop, NewState}, {error, Reason}.
%%%
%%% {ok, NewState} will continue the normal execution with the NewState.
%%%
%%% {break, NewState} breaks the execution of the current plugin-chain. This means that
%%% if a pre_request-plugin returns the break-statement the rest of the plugins in that chain will be skipped.
%%%
%%% {stop, NewState} stops the execution. The plugin is responsible in this case for returning
%%% a proper response to the client.
%%%
%%% {error, Reason} will stop the execution and call the nova_error plugin, resulting in a 500 response back
%%% to the user. If debug mode is enabled the reason will be returned in the 500-response.
%%%
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
