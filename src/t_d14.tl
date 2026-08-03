(import stdlib.io)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; step a robot one second with wrap-around
(define (step! [xs : (&mut xs ivec.Vec)] [ys : (&mut ys ivec.Vec)]
               [vxs : (& vxs ivec.Vec)] [vys : (& vys ivec.Vec)]
               [i : i64] [w : i64] [h : i64]) : unit
  (let
    [nx : i64 (+ (ivec.get (& xs) i 0) (ivec.get vxs i 0))]
    [ny : i64 (+ (ivec.get (& ys) i 0) (ivec.get vys i 0))]
    (begin
      (if (< nx 0)
        (set! nx (+ nx w))
        (if (>= nx w)
          (set! nx (- nx w))
          unit))
      (if (< ny 0)
        (set! ny (+ ny h))
        (if (>= ny h)
          (set! ny (- ny h))
          unit))
      (ivec.set xs i nx)
      (ivec.set ys i ny))))

;; do all robots have unique positions? (last-seen grid, tag = t)
(define (all-unique? [xs : (& xs ivec.Vec)] [ys : (& ys ivec.Vec)]
                     [seen : (&mut seen ivec.Vec)] [w : i64] [h : i64] [t : i64]) : bool
  (let
    [n : i64 (ivec.len xs)]
    [ok? : bool true]
    [i : i64 0]
    (begin
      (while (and ok? (< i n))
        (let [idx : i64 (+ (* (ivec.get ys i 0) w) (ivec.get xs i 0))]
          (begin
            (if (= (ivec.get (& seen) idx 0) t)
              (set! ok? false)
              (ivec.set seen idx t))
            (set! i (+ i 1))
            unit)))
      ok?)))

(define (main) : i64 0)
