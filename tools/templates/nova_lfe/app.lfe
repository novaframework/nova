(defmodule {{name}}_app
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
  ({{name}}.sup:start_link))

(defun stop ()
  ({{name}}.sup:stop)
  'ok)