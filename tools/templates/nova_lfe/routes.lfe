#m(prefix ""
   security false
   routes (#("/"
             #({{name}}.controllers.main index)
             #M(methods (get)))))