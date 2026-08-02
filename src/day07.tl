;; Day 7: Bridge Repair
(import stdlib.io)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

(define (digits10 [x : i64]) : i64
  (let [d : i64 10]
    (begin
      (while (<= d x)
        (set! d (* d 10)))
      d)))

(define (concat-num [a : i64] [b : i64]) : i64
  (+ (* a (digits10 b)) b))

;; can the numbers from idx form `target` with +, * and optionally ||?
(define (can-make? [nums : (& nums ivec.Vec)] [idx : i64] [cur : i64] [target : i64] [use-concat? : bool]) : bool
  (if (= idx (ivec.len nums))
    (= cur target)
    (if (> cur target)
      false
      (let [next : i64 (ivec.get nums idx 0)]
        (or
          (can-make? nums (+ idx 1) (+ cur next) target use-concat?)
          (can-make? nums (+ idx 1) (* cur next) target use-concat?)
          (if use-concat?
            (can-make? nums (+ idx 1) (concat-num cur next) target use-concat?)
            false))))))

(define (solve-day07 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [sum1 : i64 0]
    [sum2 : i64 0]
    [i : i64 0]
    (begin
      (while (< i nlines)
        (let
          [nums : ivec.Vec (u.parse-ints (strvec.get (& lines) i ""))]
          [target : i64 (ivec.get (& nums) 0 0)]
          (begin
            (if (can-make? (& nums) 2 (ivec.get (& nums) 1 0) target false)
              (begin
                (set! sum1 (+ sum1 target))
                (set! sum2 (+ sum2 target)))
              (if (can-make? (& nums) 2 (ivec.get (& nums) 1 0) target true)
                (set! sum2 (+ sum2 target))
                unit)))
          (set! i (+ i 1))))
      (io.print-string "day07 part1: ")
      (io.print sum1)
      (io.print-newline)
      (io.print-string "day07 part2: ")
      (io.print sum2)
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day07 "inputs/day07.txt")
    0))
