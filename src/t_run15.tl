(import stdlib.io)
(import day15)

(define (main) : i64
  (begin
    (io.print-string "small: ")
    (day15.solve-day15 "/tmp/d15_example_small.txt")
    (io.print-string "large: ")
    (day15.solve-day15 "/tmp/d15_example_large.txt")
    0))
