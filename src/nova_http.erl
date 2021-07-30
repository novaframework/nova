-module(nova_http).
-export([
         %% Functions for modifying headers
         set_headers/2,
         add_header/3,
         remove_header/2,

         %% Functions for modifying body
         set_body/2,
         clear_body/1,

         %% Functions for setting http-status
         set_status/2,

         %% Cookies
         set_cookie/3,
         set_cookie/4
        ]).

-spec set_headers(Headers :: map() | [{Key :: binary(), Value :: binary()}], NovaHttpState) ->
                         NovaHttpState when NovaHttpState :: nova:state().
set_headers(Map, NovaHttpState) when is_map(Map) ->
    set_headers(maps:to_list(Map), NovaHttpState);
set_headers([], NovaHttpState) -> NovaHttpState;
set_headers([{Key, Value}|Tl], NovaHttpState = #{req := Req}) ->
    Req0 = cowboy_req:set_resp_header(Key, Value, Req),
    set_headers(Tl, NovaHttpState#{req => Req0}).

-spec add_header(Key :: binary(), Value :: binary(), NovaHttpState) ->
                        NovaHttpState when NovaHttpState :: nova:state().
add_header(Key, Value, NovaHttpState) when is_binary(Key) andalso is_binary(Value)->
    set_headers([{Key, Value}], NovaHttpState).


-spec remove_header(Key :: binary(), NovaHttpState) -> NovaHttpState when
      NovaHttpState :: nova:state().
remove_header(Key, NovaHttpState = #{req := Req}) ->
    Req0 = cowboy_req:delete_resp_header(Key, Req),
    NovaHttpState#{req => Req0}.


-spec set_body(Body :: cowboy_req:resp_body(), NovaHttpState) ->
                      NovaHttpState when NovaHttpState :: nova:state().
set_body(Body, NovaHttpState = #{req := Req}) ->
    Req0 = cowboy_req:set_resp_body(Body, Req),
    NovaHttpState#{req => Req0}.


-spec clear_body(NovaHttpState) -> NovaHttpState when
      NovaHttpState :: nova:state().
clear_body(NovaHttpState) ->
    set_body(<<"">>, NovaHttpState).


-spec set_status(StatusCode :: integer(), NovaHttpState) -> NovaHttpState when
      NovaHttpState :: nova:state().
set_status(StatusCode, NovaHttpState) ->
    NovaHttpState#{resp_status => StatusCode}.

-spec set_cookie(Key :: binary(), Value :: iodata(), NovaHttpState) ->
                        NovaHttpState when NovaHttpState :: nova:state().
set_cookie(Key, Value, NovaHttpState) ->
    set_cookie(Key, Value, #{}, NovaHttpState).

-spec set_cookie(Key :: binary(), Value :: iodata(), Options :: cow_cookie:cookie_opts(), NovaHttpState) ->
                        NovaHttpState when NovaHttpState :: nova:state().
set_cookie(Key, Value, Options, NovaHttpState = #{req := Req}) ->
    Req0 = cowboy_req:set_resp_cookie(Key, Value, Req, Options),
    NovaHttpState#{req => Req0}.
