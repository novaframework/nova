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

%% @doc
%% Start function for the plugin. This function is called when the plugin is started
%% and will return a state that will be passed to the other functions during
%% the life cycle of the plugin. The state can be any term.
%% @end
-callback init() -> State :: nova:state().
-optional_callbacks([init/0]).

%% @doc
%% Stop function for the plugin. This function is called when the application is stopped.
%% It takes a state as argument and should return ok.
%% @end
-callback stop(State :: nova:state()) -> ok.
-optional_callbacks([stop/1]).

%% @doc
%% This function is called before the request is processed. It can modify the request
%% and the nova-state. It takes a state and a map of options as arguments and should return
%% either {ok, NewState}, {break, NewState}, {stop, NewState} or {error, Reason}.
%% @end
-callback pre_request(State :: nova:state(), Options :: map()) ->
    {ok, State0 :: nova:state()} |
    {break, State0 :: nova:state()} |
    {stop, State0 :: nova:state()} |
    {error, Reason :: term()}.
-optional_callbacks([pre_request/2]).

%% @doc
%% This function is called after the request is processed. It can modify the request.
%% It takes a state and a map of options as arguments and should return
%% either {ok, NewState}, {break, NewState}, {stop, NewState} or {error, Reason}.
%% The state is only used if there's another plugin invoked after this one.
%% @end
-callback post_request(State :: nova:state(), Options :: map()) ->
    {ok, State0 :: nova:state()} |
    {break, State0 :: nova:state()} |
    {stop, State0 :: nova:state()} |
    {error, Reason :: term()}.
-optional_callbacks([post_request/2]).

%% @doc
%% This function should return information about the plugin. The information is used
%% in the documentation and in the plugin-listing.
%% @end
-callback plugin_info() -> {Title :: binary(),
                            Version :: binary(),
                            Url :: binary(),
                            Author :: binary(),
                            Description :: binary(),
                            Requires :: [PluginName :: binary() |
                                                       {PluginName :: binary(), Version :: binary()}],
                            Options :: [{Key :: atom(), OptionDescription :: binary()}]}.
