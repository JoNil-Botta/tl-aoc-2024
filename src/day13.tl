;; Day 13: Claw Contraption
(import stdlib.io)
(import stdlib.vector)
(import stdlib.string)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; solve 2x2 system; returns cost (3A+B) or -1 if no non-negative integer solution
(define (solve-machine [ax : i64] [ay : i64] [bx : i64] [by : i64] [px : i64] [py : i64]
                       [limit : i64]) : i64
  (let
    [d : i64 (- (* ax by) (* ay bx))]
    (begin
      (if (= d 0)
        -1
        (let
          [an : i64 (- (* px by) (* py bx))]
          [bn : i64 (- (* ax py) (* ay px))]
          [a : i64 (/ an d)]
          [b : i64 (/ bn d)]
          (begin
            (if (and (>= a 0) (>= b 0)
                     (= (* a d) an)
                     (= (* b d) bn)
                     (or (< limit 0) (and (<= a limit) (<= b limit))))
              (+ (* 3 a) b)
              -1)))))))

(define (solve-day13 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [total1 : i64 0]
    [total2 : i64 0]
    [i : i64 0]
    (begin
      (while (< i nlines)
        (let
          [cur : String (strvec.get (& lines) i "")]
          (begin
            (if (= (string.string-length (& cur)) 0)
              (set! i (+ i 1))
              (let
                [la : ivec.Vec (u.parse-ints cur)]
                [lb : ivec.Vec (u.parse-ints (strvec.get (& lines) (+ i 1) ""))]
                [lp : ivec.Vec (u.parse-ints (strvec.get (& lines) (+ i 2) ""))]
                (begin
            (let
              [ax : i64 (ivec.get (& la) 0 0)]
              [ay : i64 (ivec.get (& la) 1 0)]
              [bx : i64 (ivec.get (& lb) 0 0)]
              [by : i64 (ivec.get (& lb) 1 0)]
              [px : i64 (ivec.get (& lp) 0 0)]
              [py : i64 (ivec.get (& lp) 1 0)]
              (begin
                (let [c1 : i64 (solve-machine ax ay bx by px py 100)]
                  (if (>= c1 0)
                    (set! total1 (+ total1 c1))
                    unit))
                (let [c2 : i64 (solve-machine ax ay bx by (+ px 10000000000000) (+ py 10000000000000) -1)]
                  (if (>= c2 0)
                    (set! total2 (+ total2 c2))
                    unit))))
                  (set! i (+ i 3))))))))
      (io.print-string "day13 part1: ")
      (io.print total1)
      (io.print-newline)
      (io.print-string "day13 part2: ")
      (io.print total2)
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day13 "inputs/day13.txt")
    0))
