;; Day 22: Monkey Market
;; Part 1: sum of the 2000th secret per buyer. Part 2: best 4-consecutive-diff
;; window; per buyer the first occurrence of a window wins; key = 4 diffs in
;; 0..18 encoded base 19 (19^4 = 130321).
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(define (next-secret [s : i64]) : i64
  (let
    [s1 : i64 (bit-and (bit-xor s (shl s 6)) 16777215)]
    [s2 : i64 (bit-xor s1 (shr s1 5))]
    [s3 : i64 (bit-and (bit-xor s2 (shl s2 11)) 16777215)]
    s3))

(define (solve-day22 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [nums : ivec.Vec (u.parse-ints text)]
    [n : i64 (ivec.len (& nums))]
    [totals : ivec.Vec (ivec.new)]
    [seen : ivec.Vec (ivec.new)]
    [i : i64 0]
    [p1 : i64 0]
    (begin
      (while (< i 130321)
        (begin
          (ivec.push (&mut totals) 0)
          (ivec.push (&mut seen) 0)
          (set! i (+ i 1))))
      (set! i 0)
      (while (< i n)
        (let
          [s : i64 (ivec.get (& nums) i 0)]
          [gen : i64 (+ i 1)]
          [prev : i64 (% s 10)]
          [d1 : i64 0]
          [d2 : i64 0]
          [d3 : i64 0]
          [d4 : i64 0]
          [k : i64 0]
          (begin
            (while (< k 2000)
              (let
                [s2 : i64 (next-secret s)]
                [cur : i64 (% s2 10)]
                (begin
                  (set! d1 d2)
                  (set! d2 d3)
                  (set! d3 d4)
                  (set! d4 (- cur prev))
                  (if (>= k 3)
                    (let
                      [key : i64
                        (+ (* (+ (* (+ (* (+ d1 9) 19) (+ d2 9)) 19) (+ d3 9)) 19) (+ d4 9))]
                      (if (not (= (ivec.get (& seen) key 0) gen))
                        (begin
                          (ivec.set (&mut seen) key gen)
                          (ivec.set (&mut totals) key
                            (+ (ivec.get (& totals) key 0) cur)))
                        unit))
                    unit)
                  (set! s s2)
                  (set! prev cur)
                  (set! k (+ k 1)))))
            (set! p1 (+ p1 s))
            (set! i (+ i 1)))))
      (let
        [best : i64 0]
        [j : i64 0]
        (begin
          (while (< j 130321)
            (begin
              (if (> (ivec.get (& totals) j 0) best)
                (set! best (ivec.get (& totals) j 0))
                unit)
              (set! j (+ j 1))))
          (io.print-string "day22 part1: ")
          (io.print-string (string.int->string p1))
          (io.print-newline)
          (io.print-string "day22 part2: ")
          (io.print-string (string.int->string best))
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day22 "inputs/day22.txt")
    0))
