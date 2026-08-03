;; Day 21: Keypad Conundrum
;; Chain: me -> dir pad N -> ... -> dir pad 1 -> numeric pad.
;; dir-cost(l, a, b): my presses so the pad at depth l (a directional pad)
;; presses b after a. l=0 is the top pad (manhattan + 1).
;; num-cost(a, b): my presses so the numeric pad presses b after a, using
;; dir-cost(l, ..) for the l directional pads above it. The arm of the pad
;; above the numeric starts each move at 'A'.
;; Candidate paths: horizontal-first and vertical-first (the two shortest
;; orders that avoid the missing corner; grouping moves is optimal).
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

(define (abs-i [v : i64]) : i64
  (if (< v 0) (- 0 v) v))

;; directional keys: '<'=0, 'v'=1, '>'=2, '^'=3, 'A'=4
(define (dir-pos [k : i64] [axis : i64]) : i64
  (if (= axis 0)
    (if (= k 0) 0 (if (= k 1) 1 (if (= k 2) 2 (if (= k 3) 1 2))))
    (if (= k 3) 0 (if (= k 4) 0 1))))

;; numeric keys: '0'..'9' (48..57), 'A' (65)
(define (num-pos [k : i64] [axis : i64]) : i64
  ;; 7 8 9 / 4 5 6 / 1 2 3 / 0 A
  (if (= k 65)
    (if (= axis 0) 2 3)
    (let [d : i64 (- k 48)]
      (if (= d 0)
        (if (= axis 0) 1 3)
        (let [n : i64 (- d 1)]
          (if (= axis 0) (% n 3) (- 2 (/ n 3))))))))

(define (memo-new [n : i64]) : ivec.Vec
  (let
    [v : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (ivec.push (&mut v) -1)
          (set! i (+ i 1))))
      v)))

;; cost of one candidate path on a directional pad at level l (l >= 1)
(define (dir-cand [l : i64] [a : i64] [b : i64] [order : i64]
                  [memo : (&mut memo ivec.Vec)]) : i64
  (let
    [seq : ivec.Vec (ivec.new)]
    [xa : i64 (dir-pos a 0)]
    [ya : i64 (dir-pos a 1)]
    [xb : i64 (dir-pos b 0)]
    [yb : i64 (dir-pos b 1)]
    [dx : i64 (- xb xa)]
    [dy : i64 (- yb ya)]
    [nh : i64 (abs-i dx)]
    [nv : i64 (abs-i dy)]
    (begin
      ;; the controller's arm is at 'A' after every press
      (ivec.push (&mut seq) 4)
      (if (= order 0)
        (begin
          (let [k : i64 0]
            (begin
              (while (< k nh)
                (begin
                  (ivec.push (&mut seq) (if (> dx 0) 2 0))
                  (set! k (+ k 1))))
              (set! k 0)
              (while (< k nv)
                (begin
                  (ivec.push (&mut seq) (if (> dy 0) 1 3))
                  (set! k (+ k 1))))))
          (ivec.push (&mut seq) 4))
        (begin
          (let [k : i64 0]
            (begin
              (while (< k nv)
                (begin
                  (ivec.push (&mut seq) (if (> dy 0) 1 3))
                  (set! k (+ k 1))))
              (set! k 0)
              (while (< k nh)
                (begin
                  (ivec.push (&mut seq) (if (> dx 0) 2 0))
                  (set! k (+ k 1))))))
          (ivec.push (&mut seq) 4)))
      ;; validate: no intermediate step on the gap (0,0)
      (let
        [ok? : bool true]
        [px : i64 xa]
        [py : i64 ya]
        [k : i64 1]
        (begin
          (while (and ok? (< k (- (ivec.len (& seq)) 1)))
            (let [key : i64 (ivec.get (& seq) k 0)]
              (begin
                (if (= key 0) (set! px (- px 1)) unit)
                (if (= key 2) (set! px (+ px 1)) unit)
                (if (= key 3) (set! py (- py 1)) unit)
                (if (= key 1) (set! py (+ py 1)) unit)
                (if (and (= px 0) (= py 0))
                  (set! ok? false)
                  unit)
                (set! k (+ k 1)))))
          (if (not ok?)
            1000000000000000000
            (let
              [total : i64 0]
              [k2 : i64 1]
              (begin
                (while (< k2 (ivec.len (& seq)))
                  (begin
                    (set! total
                      (+ total (dir-cost (- l 1)
                                         (ivec.get (& seq) (- k2 1) 0)
                                         (ivec.get (& seq) k2 0)
                                         memo)))
                    (set! k2 (+ k2 1))))
                total))))))))

;; my presses so the directional pad at depth l presses b after a
(define (dir-cost [l : i64] [a : i64] [b : i64] [memo : (&mut memo ivec.Vec)]) : i64
  (if (= l 0)
    (+ (abs-i (- (dir-pos b 0) (dir-pos a 0)))
       (abs-i (- (dir-pos b 1) (dir-pos a 1)))
       1)
    (let
      [idx : i64 (+ (* l 25) (+ (* a 5) b))]
      [cached : i64 (ivec.get (& memo) idx 0)]
      (if (>= cached 0)
        cached
        (let
          [c0 : i64 (dir-cand l a b 0 memo)]
          [c1 : i64 (dir-cand l a b 1 memo)]
          [best : i64 (if (< c0 c1) c0 c1)]
          (begin
            (ivec.set memo idx best)
            best))))))

;; cost of one candidate path on the numeric pad (bottom), pressing b after a;
;; l = number of directional pads above the numeric one
(define (num-cand [l : i64] [a : i64] [b : i64] [order : i64]
                  [memo : (&mut memo ivec.Vec)]) : i64
  (let
    [seq : ivec.Vec (ivec.new)]
    [xa : i64 (num-pos a 0)]
    [ya : i64 (num-pos a 1)]
    [xb : i64 (num-pos b 0)]
    [yb : i64 (num-pos b 1)]
    [dx : i64 (- xb xa)]
    [dy : i64 (- yb ya)]
    [nh : i64 (abs-i dx)]
    [nv : i64 (abs-i dy)]
    (begin
      (ivec.push (&mut seq) 4)
      (if (= order 0)
        (begin
          (let [k : i64 0]
            (begin
              (while (< k nh)
                (begin
                  (ivec.push (&mut seq) (if (> dx 0) 2 0))
                  (set! k (+ k 1))))
              (set! k 0)
              (while (< k nv)
                (begin
                  (ivec.push (&mut seq) (if (> dy 0) 1 3))
                  (set! k (+ k 1))))))
          (ivec.push (&mut seq) 4))
        (begin
          (let [k : i64 0]
            (begin
              (while (< k nv)
                (begin
                  (ivec.push (&mut seq) (if (> dy 0) 1 3))
                  (set! k (+ k 1))))
              (set! k 0)
              (while (< k nh)
                (begin
                  (ivec.push (&mut seq) (if (> dx 0) 2 0))
                  (set! k (+ k 1))))))
          (ivec.push (&mut seq) 4)))
      ;; validate on numeric layout: no intermediate step on the gap (0,3)
      (let
        [ok? : bool true]
        [px : i64 xa]
        [py : i64 ya]
        [k : i64 1]
        (begin
          (while (and ok? (< k (- (ivec.len (& seq)) 1)))
            (let [key : i64 (ivec.get (& seq) k 0)]
              (begin
                (if (= key 0) (set! px (- px 1)) unit)
                (if (= key 2) (set! px (+ px 1)) unit)
                (if (= key 3) (set! py (- py 1)) unit)
                (if (= key 1) (set! py (+ py 1)) unit)
                (if (and (= px 0) (= py 3))
                  (set! ok? false)
                  unit)
                (set! k (+ k 1)))))
          (if (not ok?)
            1000000000000000000
            (let
              [total : i64 0]
              [k2 : i64 1]
              (begin
                (while (< k2 (ivec.len (& seq)))
                  (begin
                    (set! total
                      (+ total (dir-cost l
                                         (ivec.get (& seq) (- k2 1) 0)
                                         (ivec.get (& seq) k2 0)
                                         memo)))
                    (set! k2 (+ k2 1))))
                total))))))))

;; my presses so the numeric pad presses b after a (l directional pads above)
(define (num-cost [l : i64] [a : i64] [b : i64] [memo : (&mut memo ivec.Vec)]) : i64
  (let
    [c0 : i64 (num-cand l a b 0 memo)]
    [c1 : i64 (num-cand l a b 1 memo)]
    (if (< c0 c1) c0 c1)))

;; total presses for one code
(define (solve-code [code : String] [l : i64] [memo : (&mut memo ivec.Vec)]) : i64
  (let
    [n : i64 (string.string-length (& code))]
    [prev : i64 65]
    [total : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (let
          [ch : char (string.string-ref (& code) i)]
          [k : i64 (cast ch : i64)]
          (begin
            (set! total (+ total (num-cost l prev k memo)))
            (set! prev k))
          (set! i (+ i 1))))
      total)))

;; leading digits of a code like "029A" -> 29
(define (code-num [code : String]) : i64
  (let
    [n : i64 (string.string-length (& code))]
    [acc : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (let [ch : i64 (cast (string.string-ref (& code) i) : i64)]
            (if (and (>= ch 48) (<= ch 57))
              (set! acc (+ (* acc 10) (- ch 48)))
              unit))
          (set! i (+ i 1))))
      acc)))

(define (solve-day21 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [memo : ivec.Vec (memo-new 625)]
    [i : i64 0]
    [p1 : i64 0]
    [p2 : i64 0]
    (begin
      (while (< i nlines)
        (let
          [code : String (strvec.get (& lines) i "")]
          (begin
            (set! p1 (+ p1 (* (code-num code) (solve-code code 1 (&mut memo)))))
            (set! p2 (+ p2 (* (code-num code) (solve-code code 24 (&mut memo)))))
            (set! i (+ i 1)))))
      (io.print-string "day21 part1: ")
      (io.print-string (string.int->string p1))
      (io.print-newline)
      (io.print-string "day21 part2: ")
      (io.print-string (string.int->string p2))
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day21 "inputs/day21.txt")
    0))
