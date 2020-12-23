#m(prefix ""
   security false
   routes (#("/"
             #({{name}}_main_controller index)
             #M(methods (get)))))