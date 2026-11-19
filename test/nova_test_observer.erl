%%% Lets the controllers and plugins of a suite tell its test case what ran.
-module(nova_test_observer).

-export([observed/2]).

%% Sends What to the process registered as Observer, if there is one.
-spec observed(atom(), term()) -> ok.
observed(Observer, What) ->
    case whereis(Observer) of
        undefined -> ok;
        Pid -> Pid ! {observed, What}
    end,
    ok.
