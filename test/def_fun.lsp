(def-fun f (a b)
  (begin
    (def-var i 0)
    (def-var j (+ a b))
    (while (< i j)
      (begin
        (print "i ")
        (println i)
        (set i (+ i 1))))))

(f 2 3)
