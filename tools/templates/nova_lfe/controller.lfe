(module {{name}}_main_controller
        (export (index 1)))

(defun index (#{req := #{method := <<"GET">>}})
  {'ok, [{'message, "Nova is running!"}]})
