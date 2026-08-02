;; Day 2: Red-Nosed Reports
(import stdlib.io)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; Is the pair ok for direction want-inc? (diff 1..3 in that direction)
(define (dir-ok? [prev : i64] [cur : i64] [want-inc? : bool]) : bool
  (if want-inc?
    (and (> cur prev) (<= (- cur prev) 3))
    (and (< cur prev) (<= (- prev cur) 3))))

;; Monotone check with one optional skip index (skip = -1 for none).
(define (monotone? [levels : (& levels ivec.Vec)] [skip : i64] [want-inc? : bool]) : bool
  (let
    [n : i64 (ivec.len levels)]
    [prev : i64 0]
    [have-prev? : bool false]
    [ok? : bool true]
    [i : i64 0]
    (begin
      (while (and ok? (< i n))
        (begin
          (if (= i skip)
            unit
            (let [cur : i64 (ivec.get levels i 0)]
              (begin
                (if have-prev?
                  (if (dir-ok? prev cur want-inc?)
                    unit
                    (set! ok? false))
                  unit)
                (set! prev cur)
                (set! have-prev? true))))
          (set! i (+ i 1))))
      ok?)))

;; Safe without removal, or safe after removing exactly one level.
(define (report-safe? [levels : (& levels ivec.Vec)]) : bool
  (let
    [n : i64 (ivec.len levels)]
    [found? : bool false]
    [skip : i64 -1]
    (begin
      (while (and (not found?) (< skip n))
        (begin
          (if (or (monotone? levels skip true) (monotone? levels skip false))
            (set! found? true)
            unit)
          (set! skip (+ skip 1))))
      found?)))

(define (report-safe-strict? [levels : (& levels ivec.Vec)]) : bool
  (or (monotone? levels -1 true) (monotone? levels -1 false)))

(define (solve-day02 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [lines : strvec.Vec (u.split-lines text)]
    [safe1 : i64 0]
    [safe2 : i64 0]
    [i : i64 0]
    (begin
      (while (< i (strvec.len (& lines)))
        (begin
          (let [levels : ivec.Vec (u.parse-ints (strvec.get (& lines) i ""))]
            (begin
              (if (report-safe-strict? (& levels))
                (set! safe1 (+ safe1 1))
                unit)
              (if (report-safe? (& levels))
                (set! safe2 (+ safe2 1))
                unit)))
          (set! i (+ i 1))))
      (io.print-string "day02 part1: ")
      (io.print safe1)
      (io.print-newline)
      (io.print-string "day02 part2: ")
      (io.print safe2)
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day02 "inputs/day02.txt")
    0))
