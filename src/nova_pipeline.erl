-module(nova_pipeline).
-moduledoc """
What Nova does with a request.

A server runs `execute/2` on every request, in the process that serves it,
with the environment its listener was started with. The request goes through
the steps of the listener in order: the router, the plugins that run before
the request, the security callbacks, the controller and the plugins that run
after it. A step that answers the request stops the others.

A request that went through every step without an answer is answered `204`.
""".

-export([
         execute/2,
         steps/0
        ]).

-doc "A step: it hands the request on with `{ok, Req, Env}`, or ends it with `{stop, Req}`.".
-callback execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
                                   when Req :: nova_req:req(), Env :: nova_server:env().

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
              when Req :: nova_req:req(), Env :: nova_server:env().
execute(Req0, Env) ->
    ok = nova_req:forget_response(),
    Req = nova_session:start_request(on_listener(Req0, Env)),
    try
        answered(run(maps:get(nova_steps, Env, steps()), Req, Env))
    after
        ok = nova_session:end_request(),
        ok = nova_req:forget_response()
    end.

%% What the listener says of a request: its name, and the server that
%% answers it.
on_listener(Req, Env) ->
    maps:merge(Req, maps:with([listener, nova_server], Env)).

-doc "The steps of a listener that names none.".
-spec steps() -> [module()].
steps() ->
    [
     nova_router,
     nova_plugin_handler,
     nova_security_handler,
     nova_handler,
     nova_plugin_handler
    ].

run([], Req, Env) ->
    {ok, Req, Env};
run([Step | Rest], Req, Env) ->
    case Step:execute(Req, Env) of
        {ok, Req0, Env0} -> run(Rest, Req0, Env0);
        {stop, Req0} -> {stop, Req0}
    end.

answered({ok, Req, Env}) ->
    {ok, with_answer(Req), Env};
answered({stop, Req}) ->
    {stop, with_answer(Req)}.

with_answer(Req) ->
    case nova_req:response_begun(Req) of
        false -> nova_req:reply(204, Req);
        _Sent -> Req
    end.
