(import stdlib.io)
(import stdlib.vector)
(import (vector.vector i64) as ivec)
(define (f [q : (&mut q ivec.Vec)]) : bool
  (let
    [moves : ivec.Vec (ivec.new)]
    [ok? : bool true]
    (begin
      (while (and ok? (> (ivec.len (& q)) 0))
        (let
          [bx : i64 (ivec.get (& q) (- (ivec.len (& q)) 1) 0)]
          (begin
            (ivec.pop (&mut q))
            (let [ny2 : i64 bx]
              (begin
                (if (> ny2 0)
                  (set! ok? false)
                  unit)))
            unit)))
      true)))
(define (main) : i64 0)
