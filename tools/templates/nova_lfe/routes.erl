#{prefix => "",
  security => false,
  routes => [
            {"/", { {{name}}_main_controller, index}, #{methods => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
