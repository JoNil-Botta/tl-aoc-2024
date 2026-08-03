;; Day 17: Chronospatial Computer
;; Part 2: build A from the most significant octal digits; the output of the
;; tail iterations is run(candidate) with candidate = A >> 3*idx, so match
;; run(candidate) against the program suffix starting at idx.
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

;; run the program with registers A, B, C; returns the output vector
(define (run-vm [a : i64] [b : i64] [c : i64] [prog : (& prog ivec.Vec)]) : ivec.Vec
  (let
    [out : ivec.Vec (ivec.new)]
    [ip : i64 0]
    (begin
      (while (< ip (ivec.len (& prog)))
        (let
          [op : i64 (ivec.get (& prog) ip 0)]
          [oper : i64 (ivec.get (& prog) (+ ip 1) 0)]
          [combo : i64
            (if (< oper 4)
              oper
              (if (= oper 4)
                a
                (if (= oper 5) b (if (= oper 6) c 0))))]
          (begin
            (if (= op 3)
              (if (not (= a 0))
                (set! ip oper)
                (set! ip (+ ip 2)))
              (begin
                (if (= op 0)
                  (set! a (shr a combo))
                  (if (= op 1)
                    (set! b (bit-xor b oper))
                    (if (= op 2)
                      (set! b (bit-and combo 7))
                      (if (= op 4)
                        (set! b (bit-xor b c))
                        (if (= op 5)
                          (ivec.push (&mut out) (bit-and combo 7))
                          (if (= op 6)
                            (set! b (shr a combo))
                            (if (= op 7)
                              (set! c (shr a combo))
                              unit)))))))
                (set! ip (+ ip 2)))))))
      out)))

;; out == prog[idx:]
(define (prog-suffix? [out : (& out ivec.Vec)] [prog : (& prog ivec.Vec)] [idx : i64]) : bool
  (if (not (= (ivec.len (& out)) (- (ivec.len (& prog)) idx)))
    false
    (let
      [ok? : bool true]
      [k : i64 0]
      (begin
        (while (and ok? (< k (ivec.len (& out))))
          (begin
            (if (not (= (ivec.get (& out) k 0) (ivec.get (& prog) (+ idx k) 0)))
              (set! ok? false)
              unit)
            (set! k (+ k 1))))
        ok?))))

;; smallest A whose full output equals prog; builds A from high octal digits
(define (find-a [prog : (& prog ivec.Vec)] [a : i64] [idx : i64]) : i64
  (if (< idx 0)
    a
    (let
      [t : i64 0]
      [res : i64 -1]
      (begin
        (while (and (< t 8) (= res -1))
          (begin
            (let
              [cand : i64 (+ (shl a 3) t)]
              [outv : ivec.Vec (run-vm cand 0 0 prog)]
              (if (prog-suffix? (& outv) prog idx)
                (set! res (find-a prog cand (- idx 1)))
                unit))
            (set! t (+ t 1))))
        res))))

(define (solve-day17 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [nums : ivec.Vec (u.parse-ints text)]
    [a0 : i64 (ivec.get (& nums) 0 0)]
    [b0 : i64 (ivec.get (& nums) 1 0)]
    [c0 : i64 (ivec.get (& nums) 2 0)]
    [prog : ivec.Vec (ivec.new)]
    [i : i64 3]
    (begin
      (while (< i (ivec.len (& nums)))
        (begin
          (ivec.push (&mut prog) (ivec.get (& nums) i 0))
          (set! i (+ i 1))))
      (let [outv : ivec.Vec (run-vm a0 b0 c0 (& prog))]
        (begin
          (io.print-string "day17 part1: ")
          (let [o : i64 0]
            (begin
              (while (< o (ivec.len (& outv)))
                (begin
                  (if (> o 0)
                    (io.print-string ",")
                    unit)
                  (io.print-string (string.int->string (ivec.get (& outv) o 0)))
                  (set! o (+ o 1))))
              (io.print-newline)))))
      (let [ans : i64 (find-a (& prog) 0 (- (ivec.len (& prog)) 1))]
        (begin
          (io.print-string "day17 part2: ")
          (io.print-string (string.int->string ans))
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day17 "inputs/day17.txt")
    0))
