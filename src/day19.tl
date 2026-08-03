;; Day 19: Linen Layout
;; DP per design: ways[i] = number of ways to tile design[i..]; sum over
;; patterns that match at i of ways[i + len(pattern)].
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; does pattern p match design d starting at position di?
(define (match-at? [d : String] [di : i64] [p : String]) : bool
  (let
    [pl : i64 (string.string-length (& p))]
    [ok? : bool true]
    [k : i64 0]
    (begin
      (while (and ok? (< k pl))
        (begin
          (if (not (= (string.string-ref (& d) (+ di k)) (string.string-ref (& p) k)))
            (set! ok? false)
            unit)
          (set! k (+ k 1))))
      ok?)))

;; number of ways to assemble `design` from the patterns
(define (count-ways [design : String] [patterns : (& patterns strvec.Vec)]) : i64
  (let
    [n : i64 (string.string-length (& design))]
    [ways : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (ivec.push (&mut ways) 0)
          (set! i (+ i 1))))
      (ivec.push (&mut ways) 1)
      (set! i (- n 1))
      (while (>= i 0)
        (let
          [p : i64 0]
          [acc : i64 0]
          (begin
            (while (< p (strvec.len (& patterns)))
              (begin
                (let
                  [pat : String (strvec.get (& patterns) p "")]
                  [pl : i64 (string.string-length (& pat))]
                  (if (and (<= (+ i pl) n) (match-at? design i pat))
                    (set! acc (+ acc (ivec.get (& ways) (+ i pl) 0)))
                    unit))
                (set! p (+ p 1))))
            (ivec.set (&mut ways) i acc)))
        (set! i (- i 1)))
      (ivec.get (& ways) 0 0))))

;; split the first line on ", "
(define (split-patterns [line : String]) : strvec.Vec
  (let
    [n : i64 (string.string-length (& line))]
    [v : strvec.Vec (strvec.new)]
    [start : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (if (= (string.string-ref (& line) i) ',')
            (begin
              (strvec.push (&mut v) (string.substring (& line) start (- i start)))
              (set! start (+ i 2)))
            unit)
          (set! i (+ i 1))))
      (if (< start n)
        (strvec.push (&mut v) (string.substring (& line) start (- n start)))
        unit)
      v)))

(define (solve-day19 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [first : String (strvec.get (& lines) 0 "")]
    [patterns : strvec.Vec (split-patterns first)]
    [nlines : i64 (strvec.len (& lines))]
    [i : i64 2]
    [p1 : i64 0]
    [p2 : i64 0]
    (begin
      (while (< i nlines)
        (let
          [design : String (strvec.get (& lines) i "")]
          [w : i64 (count-ways design (& patterns))]
          (begin
            (if (> w 0)
              (set! p1 (+ p1 1))
              unit)
            (set! p2 (+ p2 w)))
          (set! i (+ i 1))))
      (io.print-string "day19 part1: ")
      (io.print-string (string.int->string p1))
      (io.print-newline)
      (io.print-string "day19 part2: ")
      (io.print-string (string.int->string p2))
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day19 "inputs/day19.txt")
    0))
