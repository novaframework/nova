#{
  routes => [
             {404, { nova_error_controller, not_found }, #{}},
             {500, { nova_error_controller, server_error }, #{}}
            ]
 }.
