(defmodule {{name}}.app
  (behaviour gen_server)
  (export
    ;; app implementation
    (start 2)
    (stop 0)))

(include-lib "logjam/include/logjam.hrl")

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (log-info "Starting {{name}} application ...")
  (ets:new ({{name}}.config:table-name)
           ({{name}}.config:table-opts))
  ({{name}}.sup:start_link))

(defun stop ()
  ({{name}}.sup:stop)
  'ok)