(import stdlib.io)
(define (f [a : bool] [b : bool] [c : i64]) : bool
  (if (or a b)
    false
    (begin
      (set! c (+ c 1))
      true)))
(define (main) : i64 0)
