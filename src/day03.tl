;; Day 3: Mull It Over
(import stdlib.io)
(import stdlib.byte_buf)
(import util as u)

(defstruct MulResult
  (value i64) ;; -1 when no valid mul starts at i
  (end i64))  ;; index just past the closing ')'

;; Try to parse mul(a,b) starting at bytes[i]; MulResult.value = -1 on failure.
(define (try-mul [bytes : (& bytes)] [n : i64] [i : i64]) : MulResult
  (let
    [j : i64 (+ i 4)]
    [a : i64 0]
    [b : i64 0]
    [da : i64 0]
    [db : i64 0]
    [ok? : bool true]
    (begin
      ;; parse first number (1-3 digits)
      (while (and ok? (< j n) (u.is-digit-byte (byte_buf.bytes-get bytes j 0)))
        (begin
          (set! a (+ (* a 10) (- (cast (byte_buf.bytes-get bytes j 0) : i64) 48)))
          (set! da (+ da 1))
          (set! j (+ j 1))))
      (if (= da 0)
        (set! ok? false)
        unit)
      ;; require comma
      (if (and ok? (< j n) (= (byte_buf.bytes-get bytes j 0) (cast ',' : u8)))
        (set! j (+ j 1))
        (set! ok? false))
      ;; parse second number
      (while (and ok? (< j n) (u.is-digit-byte (byte_buf.bytes-get bytes j 0)))
        (begin
          (set! b (+ (* b 10) (- (cast (byte_buf.bytes-get bytes j 0) : i64) 48)))
          (set! db (+ db 1))
          (set! j (+ j 1))))
      (if (= db 0)
        (set! ok? false)
        unit)
      ;; require ')'
      (if (and ok? (< j n) (= (byte_buf.bytes-get bytes j 0) (cast ')' : u8)))
        (set! j (+ j 1))
        (set! ok? false))
      (if ok?
        (MulResult (* a b) j)
        (MulResult -1 (+ i 1))))))

(define (bytes-eq-at [bytes : (& bytes)] [i : i64] [lit : String]) : bool
  (let
    [lb : (& lit bytes) (byte_buf.str-as-bytes (& lit))]
    [m : i64 (byte_buf.bytes-length lb)]
    [k : i64 0]
    [match? : bool true]
    (begin
      (while (and match? (< k m))
        (begin
          (if (= (byte_buf.bytes-get bytes (+ i k) 0) (byte_buf.bytes-get lb k 0))
            unit
            (set! match? false))
          (set! k (+ k 1))))
      match?)))

(define (solve-day03 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [total1 : i64 0]
    [total2 : i64 0]
    [enabled? : bool true]
    [i : i64 0]
    (begin
      (while (< i n)
        (let
          [is-dont? : bool (and (<= (+ i 6) n) (bytes-eq-at bytes i "don't()"))]
          [is-do? : bool (and (<= (+ i 4) n) (bytes-eq-at bytes i "do()"))]
          [is-mul? : bool (and (<= (+ i 4) n) (bytes-eq-at bytes i "mul("))]
          [r : MulResult (if is-mul? (try-mul bytes n i) (MulResult -1 0))]
          (begin
            (if is-dont?
              (begin
                (set! enabled? false)
                (set! i (+ i 7)))
              (if is-do?
                (begin
                  (set! enabled? true)
                  (set! i (+ i 4)))
                (if (>= r.value 0)
                  (begin
                    (set! total1 (+ total1 r.value))
                    (if enabled?
                      (set! total2 (+ total2 r.value))
                      unit)
                    (set! i r.end))
                  (set! i (+ i 1))))))))
      (io.print-string "day03 part1: ")
      (io.print total1)
      (io.print-newline)
      (io.print-string "day03 part2: ")
      (io.print total2)
      (io.print-newline))))

(define (main) : i64
  (begin
    (solve-day03 "inputs/day03.txt")
    0))
