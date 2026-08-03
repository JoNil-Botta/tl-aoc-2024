;; Day 12: Garden Groups
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(define (grid-get [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]) : u8
  (if (and (>= r 0) (< r h) (>= c 0) (< c w))
    (byte_buf.bytes-get bytes (+ (* r (+ w 1)) c) 0)
    (cast 0 : u8)))

(define (in-region? [bytes : (& bytes)] [w : i64] [h : i64] [p : u8] [r : i64] [c : i64]) : bool
  (and (>= r 0) (< r h) (>= c 0) (< c w)
       (= (grid-get bytes w h r c) p)))

;; flood fill from (r,c); collects cell indices into cells, marks seen; returns area
(define (flood-fill [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]
                    [seen : (&mut seen ivec.Vec)] [cells : (&mut cells ivec.Vec)]) : i64
  (let
    [plant : u8 (grid-get bytes w h r c)]
    [queue : ivec.Vec (ivec.new)]
    [start : i64 (+ (* r w) c)]
    [area : i64 0]
    (begin
      (ivec.push (&mut queue) start)
      (ivec.set seen start 1)
      (while (> (ivec.len (& queue)) 0)
        (let
          [ci : i64 (ivec.get (& queue) (- (ivec.len (& queue)) 1) 0)]
          (begin
            (ivec.pop (&mut queue))
            (let
              [rr : i64 (/ ci w)]
              [cc : i64 (- ci (* rr w))]
              (begin
                (ivec.push (&mut cells) ci)
                (set! area (+ area 1))
                (if (and (in-region? bytes w h plant (- rr 1) cc)
                         (< (ivec.get (& seen) (+ (* (- rr 1) w) cc) 0) 1))
                  (begin
                    (ivec.set seen (+ (* (- rr 1) w) cc) 1)
                    (ivec.push (&mut queue) (+ (* (- rr 1) w) cc))
                    unit)
                  unit)
                (if (and (in-region? bytes w h plant (+ rr 1) cc)
                         (< (ivec.get (& seen) (+ (* (+ rr 1) w) cc) 0) 1))
                  (begin
                    (ivec.set seen (+ (* (+ rr 1) w) cc) 1)
                    (ivec.push (&mut queue) (+ (* (+ rr 1) w) cc))
                    unit)
                  unit)
                (if (and (in-region? bytes w h plant rr (- cc 1))
                         (< (ivec.get (& seen) (+ (* rr w) (- cc 1)) 0) 1))
                  (begin
                    (ivec.set seen (+ (* rr w) (- cc 1)) 1)
                    (ivec.push (&mut queue) (+ (* rr w) (- cc 1)))
                    unit)
                  unit)
                (if (and (in-region? bytes w h plant rr (+ cc 1))
                         (< (ivec.get (& seen) (+ (* rr w) (+ cc 1)) 0) 1))
                  (begin
                    (ivec.set seen (+ (* rr w) (+ cc 1)) 1)
                    (ivec.push (&mut queue) (+ (* rr w) (+ cc 1)))
                    unit)
                  unit)
                unit)))))
      area)))

;; perimeter of a region
(define (region-perimeter [bytes : (& bytes)] [w : i64] [h : i64] [plant : u8]
                          [cells : (& cells ivec.Vec)]) : i64
  (let
    [total : i64 0]
    [i : i64 0]
    (begin
      (while (< i (ivec.len (& cells)))
        (let
          [ci : i64 (ivec.get (& cells) i 0)]
          [rr : i64 (/ ci w)]
          [cc : i64 (- ci (* rr w))]
          (begin
            (if (in-region? bytes w h plant (- rr 1) cc)
              unit
              (set! total (+ total 1)))
            (if (in-region? bytes w h plant (+ rr 1) cc)
              unit
              (set! total (+ total 1)))
            (if (in-region? bytes w h plant rr (- cc 1))
              unit
              (set! total (+ total 1)))
            (if (in-region? bytes w h plant rr (+ cc 1))
              unit
              (set! total (+ total 1)))
            (set! i (+ i 1)))))
      total)))
(define (region-sides [bytes : (& bytes)] [w : i64] [h : i64] [plant : u8]
                      [cells : (& cells ivec.Vec)]) : i64
  (let
    [total : i64 0]
    [i : i64 0]
    (begin
      (while (< i (ivec.len (& cells)))
        (let
          [ci : i64 (ivec.get (& cells) i 0)]
          [rr : i64 (/ ci w)]
          [cc : i64 (- ci (* rr w))]
          (begin
            (if (and (not (in-region? bytes w h plant (- rr 1) cc))
                     (not (in-region? bytes w h plant rr (- cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (not (in-region? bytes w h plant (- rr 1) cc))
                     (not (in-region? bytes w h plant rr (+ cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (not (in-region? bytes w h plant (+ rr 1) cc))
                     (not (in-region? bytes w h plant rr (- cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (not (in-region? bytes w h plant (+ rr 1) cc))
                     (not (in-region? bytes w h plant rr (+ cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (in-region? bytes w h plant (- rr 1) cc)
                     (in-region? bytes w h plant rr (- cc 1))
                     (not (in-region? bytes w h plant (- rr 1) (- cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (in-region? bytes w h plant (- rr 1) cc)
                     (in-region? bytes w h plant rr (+ cc 1))
                     (not (in-region? bytes w h plant (- rr 1) (+ cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (in-region? bytes w h plant (+ rr 1) cc)
                     (in-region? bytes w h plant rr (- cc 1))
                     (not (in-region? bytes w h plant (+ rr 1) (- cc 1))))
              (set! total (+ total 1))
              unit)
            (if (and (in-region? bytes w h plant (+ rr 1) cc)
                     (in-region? bytes w h plant rr (+ cc 1))
                     (not (in-region? bytes w h plant (+ rr 1) (+ cc 1))))
              (set! total (+ total 1))
              unit)
            (set! i (+ i 1)))))
      total)))

(define (solve-day12 [fpath : String]) : unit
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
        [seen : ivec.Vec (ivec.with-capacity (* w h))]
        [k : i64 0]
        (begin
          (while (< k (* w h))
            (begin
              (ivec.push (&mut seen) 0)
              (set! k (+ k 1))))
          (let
            [total1 : i64 0]
            [total2 : i64 0]
            [r : i64 0]
            (begin
              (while (< r h)
                (let [c : i64 0]
                  (begin
                    (while (< c w)
                      (begin
                        (let [idx : i64 (+ (* r w) c)]
                          (if (>= (ivec.get (& seen) idx 0) 1)
                            unit
                            (let
                              [plant : u8 (grid-get bytes w h r c)]
                              [cells : ivec.Vec (ivec.new)]
                              [area : i64 (flood-fill bytes w h r c (&mut seen) (&mut cells))]
                              (begin
                                (set! total1 (+ total1 (* area (region-perimeter bytes w h plant (& cells)))))
                                (set! total2 (+ total2 (* area (region-sides bytes w h plant (& cells)))))))))
                        (set! c (+ c 1))))
                    (set! r (+ r 1)))))
              (io.print-string "day12 part1: ")
              (io.print total1)
              (io.print-newline)
              (io.print-string "day12 part2: ")
              (io.print total2)
              (io.print-newline))))))))

(define (main) : i64
  (begin
    (solve-day12 "inputs/day12.txt")
    0))
