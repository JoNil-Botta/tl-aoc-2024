;; Day 4: Ceres Search
(import stdlib.io)
(import stdlib.byte_buf)
(import util as u)

;; Flat grid access: lines are separated by '\n', width w, height h.
(define (grid-get [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]) : u8
  (if (and (>= r 0) (< r h) (>= c 0) (< c w))
    (byte_buf.bytes-get bytes (+ (* r (+ w 1)) c) 0)
    (cast 0 : u8)))

(define (match-xmas? [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64] [dr : i64] [dc : i64]) : bool
  (and
    (= (grid-get bytes w h r c) (cast 'X' : u8))
    (= (grid-get bytes w h (+ r dr) (+ c dc)) (cast 'M' : u8))
    (= (grid-get bytes w h (+ r (* 2 dr)) (+ c (* 2 dc))) (cast 'A' : u8))
    (= (grid-get bytes w h (+ r (* 3 dr)) (+ c (* 3 dc))) (cast 'S' : u8))))

(define (match-xmas-char [b : u8] [ch : char]) : bool
  (= b (cast ch : u8)))

;; X-MAS: 'A' center with M/S on both diagonals in either order.
(define (match-x-mas? [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]) : bool
  (let
    [tl : u8 (grid-get bytes w h (- r 1) (- c 1))]
    [br : u8 (grid-get bytes w h (+ r 1) (+ c 1))]
    [tr : u8 (grid-get bytes w h (- r 1) (+ c 1))]
    [bl : u8 (grid-get bytes w h (+ r 1) (- c 1))]
    (and
      (match-xmas-char (grid-get bytes w h r c) 'A')
      (or (and (match-xmas-char tl 'M') (match-xmas-char br 'S'))
          (and (match-xmas-char tl 'S') (match-xmas-char br 'M')))
      (or (and (match-xmas-char tr 'M') (match-xmas-char bl 'S'))
          (and (match-xmas-char tr 'S') (match-xmas-char bl 'M'))))))

(define (solve-day04 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [w : i64 0]
    (begin
      ;; find width = index of first '\n'
      (let [found? : bool false]
        (while (and (not found?) (< w n))
          (if (= (byte_buf.bytes-get bytes w 0) (cast '\n' : u8))
            (set! found? true)
            (set! w (+ w 1)))))
      (let
        [h : i64 (/ n (+ w 1))]
        [count1 : i64 0]
        [count2 : i64 0]
        [r : i64 0]
        (begin
          (while (< r h)
            (let [c : i64 0]
              (begin
                (while (< c w)
                  (begin
                    ;; part 1: all 8 directions
                    (if (match-xmas? bytes w h r c 1 0) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c (- 0 1) 0) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c 0 1) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c 0 (- 0 1)) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c 1 1) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c 1 (- 0 1)) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c (- 0 1) 1) (set! count1 (+ count1 1)) unit)
                    (if (match-xmas? bytes w h r c (- 0 1) (- 0 1)) (set! count1 (+ count1 1)) unit)
                    ;; part 2
                    (if (match-x-mas? bytes w h r c) (set! count2 (+ count2 1)) unit)
                    (set! c (+ c 1))))
                (set! r (+ r 1)))))
          (io.print-string "day04 part1: ")
          (io.print count1)
          (io.print-newline)
          (io.print-string "day04 part2: ")
          (io.print count2)
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day04 "inputs/day04.txt")
    0))
