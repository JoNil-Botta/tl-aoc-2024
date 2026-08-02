;; Day 1: Historian Hysteria
(import stdlib.io)
(import stdlib.hashmap)
(import stdlib.sort)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (sort.vec i64) as isort)
(import (hashmap.hashmap i64 i64) as i64map)

(define (abs-diff [a : i64] [b : i64]) : i64
  (if (< a b) (- b a) (- a b)))

(define (solve-day01 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [nums : ivec.Vec (u.parse-ints text)]
    [n : i64 (/ (ivec.len (& nums)) 2)]
    [left : ivec.Vec (ivec.new)]
    [right : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i (* n 2))
        (begin
          (ivec.push (&mut left) (ivec.get (& nums) i 0))
          (ivec.push (&mut right) (ivec.get (& nums) (+ i 1) 0))
          (set! i (+ i 2))))
      (isort.sort! (&mut left))
      (isort.sort! (&mut right))
      ;; part 1: sum of absolute differences of sorted pairs
      (let
        [total : i64 0]
        [j : i64 0]
        (begin
          (while (< j n)
            (begin
              (set! total (+ total (abs-diff (ivec.get (& left) j 0) (ivec.get (& right) j 0))))
              (set! j (+ j 1))))
          (io.print-string "day01 part1: ")
          (io.print total)
          (io.print-newline)))
      ;; part 2: similarity score = sum of x * (count of x in right)
      (let
        [counts : i64map.Map (i64map.new)]
        [score : i64 0]
        [j : i64 0]
        (begin
          (while (< j n)
            (begin
              (i64map.insert! counts (ivec.get (& right) j 0)
                (+ (i64map.lookup-value-or (i64map.get (& counts) (ivec.get (& right) j 0)) 0) 1))
              (set! j (+ j 1))))
          (set! j 0)
          (while (< j n)
            (begin
              (set! score (+ score
                (* (ivec.get (& left) j 0)
                   (i64map.lookup-value-or (i64map.get (& counts) (ivec.get (& left) j 0)) 0))))
              (set! j (+ j 1))))
          (io.print-string "day01 part2: ")
          (io.print score)
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day01 "inputs/day01.txt")
    0))
