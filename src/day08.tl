;; Day 8: Resonant Collinearity
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(define (grid-get [bytes : (& bytes)] [w : i64] [h : i64] [r : i64] [c : i64]) : u8
  (if (and (>= r 0) (< r h) (>= c 0) (< c w))
    (byte_buf.bytes-get bytes (+ (* r (+ w 1)) c) 0)
    (cast 0 : u8)))

(define (in-bounds? [w : i64] [h : i64] [r : i64] [c : i64]) : bool
  (and (>= r 0) (< r h) (>= c 0) (< c w)))

(define (solve-day08 [path : String]) : unit
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
        [pos : ivec.Vec (ivec.new)]
        [ant1 : ivec.Vec (ivec.with-capacity (* w h))]
        [ant2 : ivec.Vec (ivec.with-capacity (* w h))]
        [k : i64 0]
        (begin
          ;; collect antenna positions; zero the antinode grids
          (while (< k (* w h))
            (begin
              (ivec.push (&mut ant1) 0)
              (ivec.push (&mut ant2) 0)
              (set! k (+ k 1))))
          (let [r : i64 0]
            (begin
              (while (< r h)
                (let [c : i64 0]
                  (begin
                    (while (< c w)
                      (begin
                        (if (not (= (grid-get bytes w h r c) (cast '.' : u8)))
                          (ivec.push (&mut pos) (+ (* r w) c))
                          unit)
                        (set! c (+ c 1))))
                    (set! r (+ r 1)))))))
          (let
            [npos : i64 (ivec.len (& pos))]
            [i : i64 0]
            (begin
              (while (< i npos)
                (let [j : i64 (+ i 1)]
                  (begin
                    (while (< j npos)
                      (begin
                        (let
                          [pi : i64 (ivec.get (& pos) i 0)]
                          [pj : i64 (ivec.get (& pos) j 0)]
                          [ri : i64 (/ pi w)]
                          [ci : i64 (- pi (* ri w))]
                          [rj : i64 (/ pj w)]
                          [cj : i64 (- pj (* rj w))]
                          (begin
                            (if (= (grid-get bytes w h ri ci) (grid-get bytes w h rj cj))
                              (let
                                [dr : i64 (- rj ri)]
                                [dc : i64 (- cj ci)]
                                (begin
                                  ;; part 1: one step beyond each antenna
                                  (if (in-bounds? w h (- ri dr) (- ci dc))
                                    (ivec.set (&mut ant1) (+ (* (- ri dr) w) (- ci dc)) 1)
                                    unit)
                                  (if (in-bounds? w h (+ rj dr) (+ cj dc))
                                    (ivec.set (&mut ant1) (+ (* (+ rj dr) w) (+ cj dc)) 1)
                                    unit)
                                  ;; part 2: whole line, both directions
                                  (let
                                    [rr : i64 ri]
                                    [cc : i64 ci]
                                    (begin
                                      (while (in-bounds? w h rr cc)
                                        (begin
                                          (ivec.set (&mut ant2) (+ (* rr w) cc) 1)
                                          (set! rr (+ rr (- 0 dr)))
                                          (set! cc (+ cc (- 0 dc)))))))
                                  (let
                                    [rr : i64 rj]
                                    [cc : i64 cj]
                                    (begin
                                      (while (in-bounds? w h rr cc)
                                        (begin
                                          (ivec.set (&mut ant2) (+ (* rr w) cc) 1)
                                          (set! rr (+ rr dr))
                                          (set! cc (+ cc dc))))))))
                              unit)))
                        (set! j (+ j 1))))
                    (set! i (+ i 1)))))))
          ;; count
          (let
            [cnt1 : i64 0]
            [cnt2 : i64 0]
            [k2 : i64 0]
            (begin
              (while (< k2 (* w h))
                (begin
                  (if (>= (ivec.get (& ant1) k2 0) 1)
                    (set! cnt1 (+ cnt1 1))
                    unit)
                  (if (>= (ivec.get (& ant2) k2 0) 1)
                    (set! cnt2 (+ cnt2 1))
                    unit)
                  (set! k2 (+ k2 1))))
              (io.print-string "day08 part1: ")
              (io.print cnt1)
              (io.print-newline)
              (io.print-string "day08 part2: ")
              (io.print cnt2)
              (io.print-newline))))))))

(define (main) : i64
  (begin
    (solve-day08 "inputs/day08.txt")
    0))
