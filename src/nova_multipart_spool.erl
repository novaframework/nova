-module(nova_multipart_spool).
-moduledoc """
Tracks the files a request spools to disk and deletes them when the request is over.

A `nova_multipart_handler` that writes a part to disk calls `track/1` from the process serving the request. When that process ends, every tracked file that nobody kept is deleted. That covers a request that was answered, one a plugin or the security callback refused, one whose controller crashed and one whose client went away in the middle of a part.

## Keeping a file

A controller that wants an upload to outlive the request takes it over with `keep/1`:

```erlang
upload(#{files := [#{result := #{path := Path}}]}) ->
    ok = nova_multipart_spool:keep(Path),
    {json, 201, #{}, #{stored => Path}}.
```

Moving the file out with `file:rename/2` keeps it as well, since the spool deletes by path.

## Limits

The spool is bounded across all requests that are being served. Set the limits under the `multipart_spool` key of the `nova` environment:

```erlang
{nova, [{multipart_spool, #{max_files => 1024, max_bytes => 8000000000}}]}
```

- `max_files` - files tracked at one time. Default `infinity`: a request spools at most `max_parts` files and the listener bounds the requests, while a count across the node is one that a few slow uploads can use up.
- `max_bytes` - bytes tracked at one time. Default `8000000000`.

Either takes `infinity`. A part that would pass a limit is refused with `{error, spool_full}`, which `nova_multipart_plugin` answers with a `503`. A kept file no longer counts.
""".
-behaviour(gen_server).

-export([start_link/0,
         track/1,
         grow/2,
         rename/2,
         renamed/2,
         discard/1,
         keep/1,
         usage/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_FILES, infinity).
-define(DEFAULT_MAX_BYTES, 8000000000).

-doc "The name of a spooled file. A string and a binary of the same name are the same file.".
-type path() :: file:filename_all().

-record(state, {files = #{} :: #{binary() => {Owner :: pid(), Bytes :: non_neg_integer()}},
                owners = #{} :: #{pid() => {reference(), #{binary() => []}}},
                bytes = 0 :: non_neg_integer()}).

-doc false.
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc "Ties `Path` to the calling process: the file is deleted when that process ends, unless it was kept. Call it before creating the file.".
-spec track(Path :: path()) -> ok | {error, spool_full | spool_not_running}.
track(Path) ->
    call({track, name(Path), self()}).

-doc "Counts `Bytes` more for a tracked file. Call it before writing them.".
-spec grow(Path :: path(), Bytes :: non_neg_integer()) ->
          ok | {error, spool_full | not_tracked | spool_not_running}.
grow(Path, Bytes) ->
    call({grow, name(Path), Bytes}).

-doc "Renames a tracked file to `NewPath` and goes on tracking it there. The spool does the renaming, so a request that ends in the middle of it leaves no file behind, which `file:rename/2` followed by `renamed/2` can.".
-spec rename(Path :: path(), NewPath :: path()) -> ok | {error, not_tracked | spool_not_running | file:posix()}.
rename(Path, NewPath) ->
    call({rename, name(Path), name(NewPath)}).

-doc "Tells the spool that a tracked file now lives at `NewPath`. A file renamed by a process that ends before it says so is left on disk; `rename/2` has no such moment.".
-spec renamed(Path :: path(), NewPath :: path()) -> ok | {error, not_tracked | spool_not_running}.
renamed(Path, NewPath) ->
    call({renamed, name(Path), name(NewPath)}).

-doc "Deletes a tracked file now and forgets it.".
-spec discard(Path :: path()) -> ok | {error, not_tracked | spool_not_running}.
discard(Path) ->
    call({discard, name(Path)}).

-doc "Takes a tracked file over from the spool, which then neither deletes it nor counts it. The caller owns the file from here on.".
-spec keep(Path :: path()) -> ok | {error, not_tracked | spool_not_running}.
keep(Path) ->
    call({keep, name(Path)}).

-doc "The number of files the spool tracks and the bytes counted for them.".
-spec usage() -> #{files := non_neg_integer(), bytes := non_neg_integer()}.
usage() ->
    case call(usage) of
        #{files := _, bytes := _} = Usage -> Usage;
        {error, spool_not_running} -> #{files => 0, bytes => 0}
    end.

call(Request) ->
    try
        gen_server:call(?SERVER, Request)
    catch
        exit:{noproc, _} -> {error, spool_not_running}
    end.

-spec name(path()) -> binary().
name(Path) when is_binary(Path) ->
    Path;
name(Path) ->
    case unicode:characters_to_binary(Path, unicode, file:native_name_encoding()) of
        Name when is_binary(Name) -> Name;
        _ -> error(badarg, [Path])
    end.

-doc false.
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

-doc false.
-spec handle_call(term(), gen_server:from(), #state{}) -> {reply, term(), #state{}}.
handle_call({track, Path, Owner}, _From, State = #state{files = Files})
  when is_binary(Path), is_pid(Owner) ->
    case within(max_files, map_size(Files) + 1) of
        true ->
            {reply, ok, own(Path, Owner, 0, State)};
        false ->
            {reply, {error, spool_full}, State}
    end;
handle_call({grow, Path, Bytes}, _From, State = #state{files = Files, bytes = Total})
  when is_binary(Path), is_integer(Bytes), Bytes >= 0 ->
    case Files of
        #{Path := {Owner, Size}} ->
            case within(max_bytes, Total + Bytes) of
                true ->
                    {reply, ok, State#state{files = Files#{Path => {Owner, Size + Bytes}}, bytes = Total + Bytes}};
                false ->
                    {reply, {error, spool_full}, State}
            end;
        _ ->
            {reply, {error, not_tracked}, State}
    end;
handle_call({rename, Path, NewPath}, _From, State) when is_binary(Path), is_binary(NewPath) ->
    case State#state.files of
        #{Path := {Owner, Bytes}} ->
            case file:rename(Path, NewPath) of
                ok ->
                    {ok, State1} = forget(Path, State),
                    {reply, ok, own(NewPath, Owner, Bytes, State1)};
                {error, _Reason} = Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {error, not_tracked}, State}
    end;
handle_call({renamed, Path, NewPath}, _From, State) when is_binary(Path), is_binary(NewPath) ->
    case State#state.files of
        #{Path := {Owner, Bytes}} ->
            {ok, State1} = forget(Path, State),
            {reply, ok, own(NewPath, Owner, Bytes, State1)};
        _ ->
            {reply, {error, not_tracked}, State}
    end;
handle_call({discard, Path}, _From, State) when is_binary(Path) ->
    case forget(Path, State) of
        {ok, State1} ->
            _ = file:delete(Path),
            {reply, ok, State1};
        error ->
            {reply, {error, not_tracked}, State}
    end;
handle_call({keep, Path}, _From, State) when is_binary(Path) ->
    case forget(Path, State) of
        {ok, State1} -> {reply, ok, State1};
        error -> {reply, {error, not_tracked}, State}
    end;
handle_call(usage, _From, State = #state{files = Files, bytes = Bytes}) ->
    {reply, #{files => map_size(Files), bytes => Bytes}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

-doc false.
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-doc false.
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Monitor, process, Owner, _Reason}, State = #state{files = Files, owners = Owners}) ->
    case Owners of
        #{Owner := {Monitor, Paths}} ->
            Left = maps:keys(Paths),
            [file:delete(Path) || Path <- Left],
            Bytes = lists:sum([Size || Path <- Left, {_Owner, Size} <- [maps:get(Path, Files)]]),
            {noreply, State#state{files = maps:without(Left, Files),
                                  owners = maps:remove(Owner, Owners),
                                  bytes = State#state.bytes - Bytes}};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{files = Files}) ->
    [file:delete(Path) || Path <- maps:keys(Files)],
    ok.

-spec own(binary(), pid(), non_neg_integer(), #state{}) -> #state{}.
own(Path, Owner, Bytes, State) ->
    #state{files = Files, owners = Owners, bytes = Total} = case forget(Path, State) of
                                                                {ok, Forgotten} -> Forgotten;
                                                                error -> State
                                                            end,
    Owned = case Owners of
                #{Owner := {Monitor, Paths}} -> {Monitor, Paths#{Path => []}};
                _ -> {erlang:monitor(process, Owner), #{Path => []}}
            end,
    #state{files = Files#{Path => {Owner, Bytes}}, owners = Owners#{Owner => Owned}, bytes = Total + Bytes}.

-spec forget(binary(), #state{}) -> {ok, #state{}} | error.
forget(Path, State = #state{files = Files, owners = Owners, bytes = Total}) ->
    case Files of
        #{Path := {Owner, Bytes}} ->
            #{Owner := {Monitor, Paths}} = Owners,
            Paths1 = maps:remove(Path, Paths),
            Owners1 = case map_size(Paths1) of
                          0 ->
                              erlang:demonitor(Monitor, [flush]),
                              maps:remove(Owner, Owners);
                          _ ->
                              Owners#{Owner => {Monitor, Paths1}}
                      end,
            {ok, State#state{files = maps:remove(Path, Files), owners = Owners1, bytes = Total - Bytes}};
        _ ->
            error
    end.

within(Limit, Amount) ->
    case limit(Limit) of
        infinity -> true;
        Max -> Amount =< Max
    end.

limit(Limit) ->
    Default = case Limit of
                  max_files -> ?DEFAULT_MAX_FILES;
                  max_bytes -> ?DEFAULT_MAX_BYTES
              end,
    case application:get_env(nova, multipart_spool, #{}) of
        #{Limit := infinity} -> infinity;
        #{Limit := Max} when is_integer(Max), Max >= 0 -> Max;
        _ -> Default
    end.
