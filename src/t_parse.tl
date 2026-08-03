(import stdlib.io)
(import util as u)
(define (solve-day10 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    (begin
      (io.print-string text)
      unit)))
(define (main) : i64 0)
