(def-fun mandelbrot (c_x c_y n)
         (begin
           (def-var x_n 0)
           (def-var y_n 0)
           (def-var x_n_ 0)
           (def-var y_n_ 0)
           (def-var i 0)
           (def-var ret 0)

           (while (< i n)
                  (begin
                    (set x_n_ (+ (- (* x_n x_n) (* y_n y_n)) c_x))
                    (set y_n_ (+ (* (* 2 x_n) y_n) c_y))
                    (def-var t (+ (* x_n_ x_n_) (* y_n_ y_n_)))
                    (if (> t 4.0)
                      (begin
                        (set ret t)
                        (set i n))
                      (begin
                        (set x_n x_n_)
                        (set y_n y_n_)))
                    (set i (+ i 1))))
           ret))

(def-fun calc_mandel ()
         (begin
           (def-var x_max 2)
           (def-var x_min -2)
           (def-var y_max 1)
           (def-var y_min -1)
           (def-var dx 0.03)
           (def-var dy 0.045)

           (def-var y y_max)
           (while (> y y_min)
                  (begin
                    (def-var x x_min)
                    (while (< x x_max)
                           (begin
                             (def-var t (mandelbrot x y 300))
                             (if (> t 8)
                               (print "#")
                               (if (> t 6)
                                 (print "*")
                                 (if (> t 4)
                                   (print ".")
                                   (print "@"))))
                             (set x (+ x dx))))
                    (println "")
                    (set y (- y dy))))))
(calc_mandel)
