#{prefix => "",
  security => false,
  routes => [
            {"/", {{name}}_main_controller, index}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
