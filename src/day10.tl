;; Day 10: Hoof It
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(define (grid-get [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]) : u8
  (if (and (>= r 0) (< r h) (>= c 0) (< c w))
    (byte_buf.bytes-get bytes (+ (* r (+ w 1)) c) 0)
    (cast 0 : u8)))

(define (drd [dir : i64]) : i64
  (if (= dir 0) -1 (if (= dir 2) 1 0)))

(define (dcd [dir : i64]) : i64
  (if (= dir 1) 1 (if (= dir 3) -1 0)))

;; number of paths from (r,c) to any 9; marks reachable 9s per generation
(define (paths-from [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]
                    [nine-marks : (&mut nine-marks ivec.Vec)] [gen : i64]) : i64
  (let
    [cur : i64 (- (cast (grid-get bytes w h r c) : i64) 48)]
    (begin
      (if (= cur 9)
        (begin
          (let [idx : i64 (+ (* r w) c)]
            (if (>= (ivec.get (& nine-marks) idx 0) gen)
              unit
              (ivec.set nine-marks idx gen)))
          1)
        (let
          [total : i64 0]
          [dir : i64 0]
          (begin
            (while (< dir 4)
              (begin
                (let
                  [nr : i64 (+ r (drd dir))]
                  [nc : i64 (+ c (dcd dir))]
                  (begin
                    (if (and (>= nr 0) (< nr h) (>= nc 0) (< nc w)
                             (= (- (cast (grid-get bytes w h nr nc) : i64) 48) (+ cur 1)))
                      (set! total (+ total (paths-from bytes w h nr nc nine-marks gen)))
                      unit)))
                (set! dir (+ dir 1))))
            total))))))

(define (solve-day10 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [w : i64 0]
    [wfound? : bool false]
    (begin
      (while (and (not wfound?) (< w n))
        (if (= (byte_buf.bytes-get bytes w 0) (cast '\n' : u8))
          (set! wfound? true)
          (set! w (+ w 1))))
      (let
        [h : i64 (/ n (+ w 1))]
        [nine-marks : ivec.Vec (ivec.with-capacity (* w h))]
        [k : i64 0]
        (begin
          (while (< k (* w h))
            (begin
              (ivec.push (&mut nine-marks) 0)
              (set! k (+ k 1))))
          (let
            [score : i64 0]
            [rating : i64 0]
            [gen : i64 1]
            [r : i64 0]
            (begin
              (while (< r h)
                (let [c : i64 0]
                  (begin
                    (while (< c w)
                      (begin
                        (if (= (grid-get bytes w h r c) (cast '0' : u8))
                          (begin
                            (set! rating (+ rating (paths-from bytes w h r c (&mut nine-marks) gen)))
                            (let [k2 : i64 0]
                              (begin
                                (while (< k2 (* w h))
                                  (begin
                                    (if (>= (ivec.get (& nine-marks) k2 0) gen)
                                      (set! score (+ score 1))
                                      unit)
                                    (set! k2 (+ k2 1))))
                                (set! gen (+ gen 1)))))
                          unit)
                        (set! c (+ c 1))))
                    (set! r (+ r 1)))))
              (io.print-string "day10 part1: ")
              (io.print score)
              (io.print-newline)
              (io.print-string "day10 part2: ")
              (io.print rating)
              (io.print-newline))))))))

(define (main) : i64
  (begin
    (solve-day10 "inputs/day10.txt")
    0))
