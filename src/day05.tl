;; Day 5: Print Queue
(import stdlib.io)
(import stdlib.hashmap)
(import stdlib.vector)
(import stdlib.string)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)
(import (hashmap.hashmap i64 i64) as rulesmap)

;; key for rule "a before b" (pages are 1-99)
(define (rule-key [a : i64] [b : i64]) : i64
  (+ (* a 100) b))

(define (rule-exists? [rules : (& rules rulesmap.Map)] [a : i64] [b : i64]) : bool
  (>= (rulesmap.lookup-value-or (rulesmap.get rules (rule-key a b)) 0) 1))

;; correctly ordered: no pair (i<j) violates a rule (u[j] before u[i])
(define (correct? [upd : (& upd ivec.Vec)] [rules : (& rules rulesmap.Map)]) : bool
  (let
    [n : i64 (ivec.len upd)]
    [ok? : bool true]
    [i : i64 0]
    (begin
      (while (and ok? (< i n))
        (let [j : i64 (+ i 1)]
          (begin
            (while (and ok? (< j n))
              (begin
                (if (rule-exists? rules (ivec.get upd j 0) (ivec.get upd i 0))
                  (set! ok? false)
                  unit)
                (set! j (+ j 1))))
            (set! i (+ i 1)))))
      ok?)))

;; insertion sort with the rule comparator
(define (sort-update! [upd : (&mut upd ivec.Vec)] [rules : (& rules rulesmap.Map)]) : unit
  (let
    [n : i64 (ivec.len (& upd))]
    [i : i64 1]
    (begin
      (while (< i n)
        (let [j : i64 i]
          (begin
            (while (and (> j 0) (rule-exists? rules (ivec.get (& upd) j 0) (ivec.get (& upd) (- j 1) 0)))
              (begin
                (let [t : i64 (ivec.get (& upd) j 0)]
                  (begin
                    (ivec.set upd j (ivec.get (& upd) (- j 1) 0))
                    (ivec.set upd (- j 1) t)))
                (set! j (- j 1))))
            (set! i (+ i 1))))))))

(define (solve-day05 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [lines : strvec.Vec (u.split-lines text)]
    [rules : rulesmap.Map (rulesmap.new)]
    [nlines : i64 (strvec.len (& lines))]
    [in-rules? : bool true]
    [sum1 : i64 0]
    [sum2 : i64 0]
    [i : i64 0]
    (begin
      (while (< i nlines)
        (let
          [line : String (strvec.get (& lines) i "")]
          [len : i64 (string.string-length (& line))]
          (begin
            (if (= len 0)
              (set! in-rules? false)
              (if in-rules?
                (let [nums : ivec.Vec (u.parse-ints line)]
                  (rulesmap.insert! rules (rule-key (ivec.get (& nums) 0 0) (ivec.get (& nums) 1 0)) 1))
                (let [upd : ivec.Vec (u.parse-ints line)]
                  (if (correct? (& upd) (& rules))
                    (set! sum1 (+ sum1 (ivec.get (& upd) (/ (ivec.len (& upd)) 2) 0)))
                    (begin
                      (sort-update! (&mut upd) (& rules))
                      (set! sum2 (+ sum2 (ivec.get (& upd) (/ (ivec.len (& upd)) 2) 0))))))))
            (set! i (+ i 1)))))
      (io.print-string "day05 part1: ")
      (io.print sum1)
      (io.print-newline)
      (io.print-string "day05 part2: ")
      (io.print sum2)
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day05 "inputs/day05.txt")
    0))
