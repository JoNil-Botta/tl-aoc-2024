;; util.tl — shared helpers for AoC 2024 solutions.
(import stdlib.io)
(import stdlib.string)
(import stdlib.byte_buf)
(import stdlib.vector)

(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; Read a whole file into an owned String.
(define (read-input [path : String]) : String
  (io.read-file path))

(define (is-digit-byte [b : u8]) : bool
  (and (>= b 48) (<= b 57)))

;; Parse every whitespace-separated integer in `text` into a vec.
(define (parse-ints [text : String]) : ivec.Vec
  (let
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [v : ivec.Vec (ivec.new)]
    [i : i64 0]
    [val : i64 0]
    [sign : i64 1]
    [in-num? : bool false]
    (begin
      (while (< i n)
        (let [b : u8 (byte_buf.bytes-get bytes i 0)]
          (if (is-digit-byte b)
            (begin
              (set! val (+ (* val 10) (- (cast b : i64) 48)))
              (set! in-num? true))
            (begin
              (if in-num?
                (begin
                  (ivec.push (&mut v) (* sign val))
                  (set! val 0)
                  (set! sign 1)
                  (set! in-num? false))
                unit)
              (if (= b 45)
                (set! sign -1)
                unit))))
        (set! i (+ i 1)))
      (if in-num?
        (ivec.push (&mut v) (* sign val))
        unit)
      v)))

;; Split on newlines, dropping the trailing empty piece after the final '\n'.
(define (split-lines [text : String]) : strvec.Vec
  (let
    [n : i64 (string.string-length (& text))]
    [v : strvec.Vec (strvec.new)]
    [start : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (if (= (string.string-ref (& text) i) '\n')
            (begin
              (strvec.push (&mut v) (string.substring (& text) start (- i start)))
              (set! start (+ i 1)))
            unit)
          (set! i (+ i 1))))
      (if (< start n)
        (strvec.push (&mut v) (string.substring (& text) start (- n start)))
        unit)
      v)))
