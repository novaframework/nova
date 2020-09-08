%%%-------------------------------------------------------------------
%% @doc nova_admin public API
%% @end
%%%-------------------------------------------------------------------

(module {{name}}_app
 (behaviour application)
 (export (start 2) (stop 1)))

(defun start (_starttype, _startargs)
 ({{name}}_sup:start_link))

(defun stop (_state),
 'ok)
