;; Day 20: Race Condition
;; BFS from the start over track cells; every cheat is a pair of track cells
;; with Manhattan distance <= limit; savings = path-dist difference - Manhattan.
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

(define (abs-i [v : i64]) : i64
  (if (< v 0) (- 0 v) v))

;; BFS from (sx, sy); returns dist per cell (y*w+x), -1 if unreachable
(define (bfs-dist [bytes : (& bytes)] [w : i64] [h : i64]
                  [sx : i64] [sy : i64]) : ivec.Vec
  (let
    [total : i64 (* w h)]
    [dist : ivec.Vec (ivec.new)]
    [q : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i total)
        (begin
          (ivec.push (&mut dist) -1)
          (set! i (+ i 1))))
      (ivec.push (&mut q) (+ (* sy w) sx))
      (ivec.set (&mut dist) (+ (* sy w) sx) 0)
      (let [head : i64 0]
        (begin
          (while (< head (ivec.len (& q)))
            (let
              [cur : i64 (ivec.get (& q) head 0)]
              [cx : i64 (% cur w)]
              [cy : i64 (/ cur w)]
              (begin
                (set! head (+ head 1))
                (let [dd : i64 0]
                  (begin
                    (while (< dd 4)
                      (begin
                        (let
                          [nx : i64 (+ cx (if (= dd 0) 1 (if (= dd 1) (- 0 1) 0)))]
                          [ny : i64 (+ cy (if (= dd 2) 1 (if (= dd 3) (- 0 1) 0)))]
                          [ni : i64 (+ (* ny w) nx)]
                          (if (and (>= nx 0) (< nx w) (>= ny 0) (< ny h)
                                   (= (ivec.get (& dist) ni 0) -1)
                                   (not (= (byte_buf.bytes-get bytes (+ (* ny (+ w 1)) nx) 0)
                                           (cast '#' : u8))))
                            (begin
                              (ivec.set (&mut dist) ni (+ (ivec.get (& dist) cur 0) 1))
                              (ivec.push (&mut q) ni))
                            unit))
                        (set! dd (+ dd 1))))))
              unit))))
      dist))))

;; count cheats with manhattan <= limit and savings >= need
(define (count-cheats [bytes : (& bytes)] [w : i64] [h : i64]
                      [dist : (& dist ivec.Vec)] [limit : i64] [need : i64]) : i64
  (let
    [cnt : i64 0]
    [cy : i64 0]
    (begin
      (while (< cy h)
        (let [cx : i64 0]
          (begin
            (while (< cx w)
              (begin
                (let [ca : i64 (+ (* cy w) cx)]
                  (if (>= (ivec.get (& dist) ca 0) 0)
                    (let [dx : i64 (- 0 limit)]
                      (begin
                        (while (<= dx limit)
                          (let [dy : i64 (- 0 limit)]
                            (begin
                              (while (<= dy limit)
                                (begin
                                  (let
                                    [md : i64 (+ (abs-i dx) (abs-i dy))]
                                    (if (and (>= md 1) (<= md limit)
                                             (>= (+ cx dx) 0) (< (+ cx dx) w)
                                             (>= (+ cy dy) 0) (< (+ cy dy) h))
                                      (let
                                        [cb : i64 (+ (* (+ cy dy) w) (+ cx dx))]
                                        [db : i64 (ivec.get (& dist) cb 0)]
                                        (if (and (>= db 0)
                                                 (>= (- (- db (ivec.get (& dist) ca 0)) md) need))
                                          (set! cnt (+ cnt 1))
                                          unit))
                                      unit))
                                  (set! dy (+ dy 1))))
                              (set! dx (+ dx 1)))))))
                      unit))
                (set! cx (+ cx 1))))
            (set! cy (+ cy 1)))))
      cnt)))

(define (solve-day20 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [first : String (strvec.get (& lines) 0 "")]
    [w : i64 (string.string-length (& first))]
    [h : i64 (strvec.len (& lines))]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [sx : i64 0]
    [sy : i64 0]
    (begin
      (let [y : i64 0]
        (begin
          (while (< y h)
            (let [x : i64 0]
              (begin
                (while (< x w)
                  (begin
                    (if (= (byte_buf.bytes-get bytes (+ (* y (+ w 1)) x) 0) (cast 'S' : u8))
                      (begin
                        (set! sx x)
                        (set! sy y))
                      unit)
                    (set! x (+ x 1))))
                (set! y (+ y 1)))))
      (let [dist : ivec.Vec (bfs-dist bytes w h sx sy)]
        (begin
          (io.print-string "day20 part1: ")
          (io.print-string (string.int->string (count-cheats bytes w h (& dist) 2 100)))
          (io.print-newline)
          (io.print-string "day20 part2: ")
          (io.print-string (string.int->string (count-cheats bytes w h (& dist) 20 100)))
          (io.print-newline))))))))

(define (main) : i64
  (begin
    (solve-day20 "inputs/day20.txt")
    0))
