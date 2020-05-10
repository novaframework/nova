%%%-------------------------------------------------------------------
%% @doc
%% Nova application behaviour callback (Not used)
%% @end
%%%-------------------------------------------------------------------

-module(nova_app).

-behaviour(application).

-include_lib("nova/include/nova.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
    case maps:get(main_app, StartArgs, undefined) of
        undefined ->
            ?ERROR("No main_app defined in mod-section of app.src-file!~nCheck the installation-section in the manual."),
            throw({nova_error, no_main_app_defined});
        MainApp ->
            Configuration = application:get_env(configuration, MainApp),
            nova_sup:start_link(Configuration)
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
