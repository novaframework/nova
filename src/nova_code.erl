-module(nova_code).
-moduledoc false.

-export([module_named/1]).

-doc """
The module called `Name`, for a name that is put together at run time, such
as the view of a controller or the router of an application. The name only
becomes an atom when it is one already or a module of that name is found in
the code path, so a name taken from a request or from configuration cannot
fill the atom table.
""".
-spec module_named(Name :: string()) -> {ok, module()} | error.
module_named(Name) ->
    try
        {ok, list_to_existing_atom(Name)}
    catch
        error:badarg -> load_named(Name)
    end.

load_named(Name) ->
    case filename:basename(Name) =:= Name andalso code:where_is_file(Name ++ ".beam") of
        false ->
            error;
        non_existing ->
            error;
        File ->
            case code:load_abs(filename:rootname(File, ".beam")) of
                {module, Module} -> {ok, Module};
                {error, _Reason} -> error
            end
    end.
