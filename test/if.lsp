(if (< 3 1)
  (println "true")
  (println "false"))

(if (true)
  (begin
    (println "true yeah")
    (println "second line"))
  (println "false"))


(if (false)
  (println "true")
  (begin
    (println "false yeah")
    (println "second line")))
