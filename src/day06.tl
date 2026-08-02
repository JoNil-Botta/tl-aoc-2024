;; Day 6: Guard Gallivant
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

;; mark (idx, dir) for generation gen; true if already marked this gen+dir
(define (mark-visited! [visited : (&mut visited ivec.Vec)] [idx : i64] [dir : i64] [gen : i64]) : bool
  (let
    [v : i64 (ivec.get (& visited) idx 0)]
    [mask : i64 (shl 1 dir)]
    (begin
      (if (and (= (shr v 4) gen) (>= (bit-and v mask) 1))
        true
        (begin
          (ivec.set visited idx (+ (* gen 16) (bit-or (if (= (shr v 4) gen) (bit-and v 15) 0) mask)))
          false)))))

;; simulate; obstacle at (obr, obc); -1,-1 for none. Returns 0 exited, 1 looped.
(define (simulate [bytes : (& bytes)] [w : i64] [h : i64] [sr : i64] [sc : i64]
                  [obr : i64] [obc : i64] [visited : (&mut visited ivec.Vec)] [gen : i64]) : i64
  (let
    [r : i64 sr]
    [c : i64 sc]
    [dir : i64 0]
    [looped? : bool false]
    [done? : bool false]
    (begin
      (while (and (not done?) (not looped?))
        (begin
          (let [idx : i64 (+ (* r w) c)]
            (if (mark-visited! visited idx dir gen)
              (set! looped? true)
              unit))
          (if (not looped?)
            (let
              [nr : i64 (+ r (drd dir))]
              [nc : i64 (+ c (dcd dir))]
              (begin
                (if (or (< nr 0) (>= nr h) (< nc 0) (>= nc w))
                  (set! done? true)
                  (if (or (= (grid-get bytes w h nr nc) (cast '#' : u8))
                          (and (= nr obr) (= nc obc)))
                    (set! dir (if (= dir 3) 0 (+ dir 1)))
                    (begin
                      (set! r nr)
                      (set! c nc))))))
            unit)))
      (if looped? 1 0))))

(define (solve-day06 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
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
        [sr : i64 0]
        [sc : i64 0]
        (begin
          ;; find guard start
          (let
            [r : i64 0]
            [done? : bool false]
            (begin
              (while (and (not done?) (< r h))
                (let [c : i64 0]
                  (begin
                    (while (and (not done?) (< c w))
                      (begin
                        (if (= (grid-get bytes w h r c) (cast '^' : u8))
                          (begin
                            (set! sr r)
                            (set! sc c)
                            (set! done? true))
                          unit)
                        (set! c (+ c 1))))
                    (set! r (+ r 1)))))))
          ;; part 1
          (let
            [visited : ivec.Vec (ivec.with-capacity (* w h))]
            [k : i64 0]
            (begin
              (while (< k (* w h))
                (begin
                  (ivec.push (&mut visited) 0)
                  (set! k (+ k 1))))
              (simulate bytes w h sr sc (- 0 1) (- 0 1) (&mut visited) 0)
              (let
                [cnt : i64 0]
                [path : ivec.Vec (ivec.new)]
                [k2 : i64 0]
                (begin
                  (while (< k2 (* w h))
                    (begin
                      (if (>= (ivec.get (& visited) k2 0) 1)
                        (begin
                          (set! cnt (+ cnt 1))
                          (ivec.push (&mut path) k2))
                        unit)
                      (set! k2 (+ k2 1))))
                  (io.print-string "day06 part1: ")
                  (io.print cnt)
                  (io.print-newline))
                  ;; part 2
                  (let
                    [loops : i64 0]
                    [gen : i64 1]
                    [pi : i64 0]
                    (begin
                      (while (< pi (ivec.len (& path)))
                        (begin
                          (let
                            [idx : i64 (ivec.get (& path) pi 0)]
                            [r2 : i64 (/ idx w)]
                            [c2 : i64 (- idx (* r2 w))]
                            (begin
                              (if (and (not (and (= r2 sr) (= c2 sc)))
                                       (not (= (grid-get bytes w h r2 c2) (cast '#' : u8))))
                                (begin
                                  (if (= (simulate bytes w h sr sc r2 c2 (&mut visited) gen) 1)
                                    (set! loops (+ loops 1))
                                    unit)
                                  (set! gen (+ gen 1)))
                                unit)))
                          (set! pi (+ pi 1))))
                      (io.print-string "day06 part2: ")
                      (io.print loops)
                      (io.print-newline)))))))))))

(define (main) : i64
  (begin
    (solve-day06 "inputs/day06.txt")
    0))
