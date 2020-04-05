(for (def-var x 1) (< x 10) (set x (+ x 1))
  (for (def-var y 1) (< y 10) (set y (+ y 1))
    (println x " * " y " => " (* x y))))
