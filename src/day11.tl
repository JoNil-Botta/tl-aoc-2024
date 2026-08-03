;; Day 11: Plutonian Pebbles
(import stdlib.io)
(import stdlib.hashmap)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (hashmap.hashmap i64 i64) as i64map)

(define (ndigits [x : i64]) : i64
  (let [d : i64 1]
    (begin
      (while (>= x 10)
        (begin
          (set! x (/ x 10))
          (set! d (+ d 1))))
      d)))

(define (pow10 [n : i64]) : i64
  (let [p : i64 1]
    (begin
      (while (> n 0)
        (begin
          (set! p (* p 10))
          (set! n (- n 1))))
      p)))

(define (add-child [ncounts : (&mut ncounts i64map.Map)] [nstones : (&mut nstones ivec.Vec)]
                   [child : i64] [cnt : i64]) : unit
  (begin
    (if (not (i64map.contains? (& ncounts) child))
      (ivec.push (&mut nstones) child)
      unit)
    (i64map.insert-or-update! ncounts child
      (+ (i64map.lookup-value-or (i64map.get (& ncounts) child) 0) cnt))))

;; clear a buffer pair before it is used as the write target
(define (reset! [counts : (&mut counts i64map.Map)] [stones : (&mut stones ivec.Vec)]) : unit
  (let
    [n : i64 (ivec.len (& stones))]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (i64map.remove-ref! counts (ivec.get (& stones) i 0))
          (set! i (+ i 1))))
      (while (> (ivec.len (& stones)) 0)
        (begin
          (ivec.pop stones)
          unit)))))

(define (blink! [counts-a : (&mut counts-a i64map.Map)] [stones-a : (&mut stones-a ivec.Vec)]
                [counts-b : (&mut counts-b i64map.Map)] [stones-b : (&mut stones-b ivec.Vec)]) : unit
  (let
    [n : i64 (ivec.len (& stones-a))]
    [i : i64 0]
    (begin
      (while (< i n)
        (let
          [s : i64 (ivec.get (& stones-a) i 0)]
          [cnt : i64 (i64map.lookup-value-or (i64map.get (& counts-a) s) 0)]
          (begin
            (if (= s 0)
              (add-child counts-b stones-b 1 cnt)
              (if (= (bit-and (ndigits s) 1) 0)
                (let
                  [p10 : i64 (pow10 (/ (ndigits s) 2))]
                  [hi : i64 (/ s p10)]
                  [lo : i64 (- s (* hi p10))]
                  (begin
                    (add-child counts-b stones-b hi cnt)
                    (add-child counts-b stones-b lo cnt)))
                (add-child counts-b stones-b (* s 2024) cnt)))
            (set! i (+ i 1))))))))

(define (total-count [counts : (& counts i64map.Map)] [stones : (& stones ivec.Vec)]) : i64
  (let
    [n : i64 (ivec.len stones)]
    [total : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (set! total (+ total (i64map.lookup-value-or (i64map.get counts (ivec.get stones i 0)) 0)))
          (set! i (+ i 1))))
      total)))

(define (solve-day11 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [nums : ivec.Vec (u.parse-ints text)]
    [counts0 : i64map.Map (i64map.new)]
    [stones0 : ivec.Vec (ivec.new)]
    [counts1 : i64map.Map (i64map.new)]
    [stones1 : ivec.Vec (ivec.new)]
    [ni : i64 (ivec.len (& nums))]
    [i : i64 0]
    (begin
      (while (< i ni)
        (begin
          (add-child (&mut counts0) (&mut stones0) (ivec.get (& nums) i 0) 1)
          (set! i (+ i 1))))
      (let
        [blink : i64 0]
        (begin
          (while (< blink 75)
            (begin
              (if (= (bit-and blink 1) 0)
                (begin
                  (reset! (&mut counts1) (&mut stones1))
                  (blink! (&mut counts0) (&mut stones0) (&mut counts1) (&mut stones1)))
                (begin
                  (reset! (&mut counts0) (&mut stones0))
                  (blink! (&mut counts1) (&mut stones1) (&mut counts0) (&mut stones0))))
              (set! blink (+ blink 1))
              (if (= blink 25)
                (begin
                  (io.print-string "day11 part1: ")
                  (io.print (total-count (& counts1) (& stones1)))
                  (io.print-newline))
                unit))))
          (io.print-string "day11 part2: ")
          (io.print (total-count (& counts1) (& stones1)))
          (io.print-newline)))))

(define (main) : i64
  (begin
    (solve-day11 "inputs/day11.txt")
    0))
