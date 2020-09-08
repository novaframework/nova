(module {{name}}_sup
 (behaviour supervisor)
 (export (start_link 0) (init 1))
 (define THIS NEED TO BE SET))

(defun start_link ()
 (supervisor:start_link ({'local, ?SERVER}, ?MODULE, [])))

(defun init ([])
  {'ok, {{'one_for_all, 0, 1}, []}})
