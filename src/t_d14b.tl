;; Day 14: Restroom Redoubt
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

(define (solve-day14 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [xs : ivec.Vec (ivec.new)]
    [ys : ivec.Vec (ivec.new)]
    [vxs : ivec.Vec (ivec.new)]
    [vys : ivec.Vec (ivec.new)]
    [w : i64 101]
    [h : i64 103]
    [i : i64 0]
    (begin
      (while (< i nlines)
        (let [nums : ivec.Vec (u.parse-ints (strvec.get (& lines) i ""))]
          (begin
            (ivec.push (&mut xs) (ivec.get (& nums) 0 0))
            (ivec.push (&mut ys) (ivec.get (& nums) 1 0))
            (ivec.push (&mut vxs) (ivec.get (& nums) 2 0))
            (ivec.push (&mut vys) (ivec.get (& nums) 3 0)))
          (set! i (+ i 1)))))
      ;; part 1: 100 seconds
      (let [t : i64 0]
        (begin
          (while (< t 100)
            (begin
              (let [j : i64 0]
                (begin
                  (while (< j (ivec.len (& xs)))
                    (begin
                      (step! (&mut xs) (&mut ys) (& vxs) (& vys) j w h)
                      (set! j (+ j 1)))))
              (set! t (+ t 1))))
          (let
            [q1 : i64 0]
            [q2 : i64 0]
            [q3 : i64 0]
            [q4 : i64 0]
            [j : i64 0]
            (begin
              (while (< j (ivec.len (& xs)))
                (begin
                  (let
                    [x : i64 (ivec.get (& xs) j 0)]
                    [y : i64 (ivec.get (& ys) j 0)]
                    (begin
                      (if (and (< x 50) (< y 51)) (set! q1 (+ q1 1)) unit)
                      (if (and (> x 50) (< y 51)) (set! q2 (+ q2 1)) unit)
                      (if (and (< x 50) (> y 51)) (set! q3 (+ q3 1)) unit)
                      (if (and (> x 50) (> y 51)) (set! q4 (+ q4 1)) unit)))
                  (set! j (+ j 1))))))
              (io.print-string "day14 part1: ")
              (io.print (* (* (* q1 q2) q3) q4))
              (io.print-newline))))
      (io.print-string "part2-stub")
      (io.print-newline)
      unit)
(define (main) : i64
  (begin
    (solve-day14 "inputs/day14.txt")
    0))
