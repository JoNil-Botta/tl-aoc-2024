;; Day 18: RAM Run
;; Part 1: BFS after the first 1024 bytes (12 in the example).
;; Part 2: binary search the first byte count that disconnects (0,0) from the exit.
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(define (dir-dx [d : i64]) : i64
  (if (= d 0) 1 (if (= d 1) (- 0 1) 0)))

(define (dir-dy [d : i64]) : i64
  (if (= d 2) 1 (if (= d 3) (- 0 1) 0)))

;; is there a path from (0,0) to (size-1, size-1) with the first n bytes fallen?
(define (path-exists? [xs : (& xs ivec.Vec)] [ys : (& ys ivec.Vec)]
                      [n : i64] [size : i64]) : bool
  (let
    [total : i64 (* size size)]
    [blocked : ivec.Vec (ivec.new)]
    [dist : ivec.Vec (ivec.new)]
    [q : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i total)
        (begin
          (ivec.push (&mut blocked) 0)
          (ivec.push (&mut dist) -1)
          (set! i (+ i 1))))
      (set! i 0)
      (while (< i n)
        (begin
          (ivec.set (&mut blocked)
            (+ (* (ivec.get (& ys) i 0) size) (ivec.get (& xs) i 0))
            1)
          (set! i (+ i 1))))
      (ivec.push (&mut q) 0)
      (ivec.set (&mut dist) 0 0)
      (let [head : i64 0]
        (begin
          (while (and (< head (ivec.len (& q)))
                      (= (ivec.get (& dist) (- total 1) 0) -1))
            (let
              [cur : i64 (ivec.get (& q) head 0)]
              [cx : i64 (% cur size)]
              [cy : i64 (/ cur size)]
              (begin
                (set! head (+ head 1))
                (let [dd : i64 0]
                  (begin
                    (while (< dd 4)
                      (begin
                        (let
                          [nx : i64 (+ cx (dir-dx dd))]
                          [ny : i64 (+ cy (dir-dy dd))]
                          [ni : i64 (+ (* ny size) nx)]
                          (if (and (>= nx 0) (< nx size) (>= ny 0) (< ny size)
                                   (= (ivec.get (& blocked) ni 0) 0)
                                   (= (ivec.get (& dist) ni 0) -1))
                            (begin
                              (ivec.set (&mut dist) ni (+ (ivec.get (& dist) cur 0) 1))
                              (ivec.push (&mut q) ni))
                            unit))
                        (set! dd (+ dd 1))))))
              unit))))
      (>= (ivec.get (& dist) (- total 1) 0) 0)))))

(define (solve-day18 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [nums : ivec.Vec (u.parse-ints text)]
    [xs : ivec.Vec (ivec.new)]
    [ys : ivec.Vec (ivec.new)]
    [maxc : i64 0]
    [i : i64 0]
    (begin
      (while (< i (ivec.len (& nums)))
        (begin
          (let [x : i64 (ivec.get (& nums) i 0)]
            (begin
              (ivec.push (&mut xs) x)
              (if (> x maxc) (set! maxc x) unit)))
          (let [y : i64 (ivec.get (& nums) (+ i 1) 0)]
            (begin
              (ivec.push (&mut ys) y)
              (if (> y maxc) (set! maxc y) unit)))
          (set! i (+ i 2))))
      (let
        [size : i64 (+ maxc 1)]
        [fall1 : i64 (if (> size 20) 1024 12)]
        (begin
          ;; part 1: shortest path after fall1 bytes
          (let
            [total : i64 (* size size)]
            [blocked : ivec.Vec (ivec.new)]
            [dist : ivec.Vec (ivec.new)]
            [q : ivec.Vec (ivec.new)]
            [j : i64 0]
            (begin
              (while (< j total)
                (begin
                  (ivec.push (&mut blocked) 0)
                  (ivec.push (&mut dist) -1)
                  (set! j (+ j 1))))
              (set! j 0)
              (while (< j fall1)
                (begin
                  (ivec.set (&mut blocked)
                    (+ (* (ivec.get (& ys) j 0) size) (ivec.get (& xs) j 0))
                    1)
                  (set! j (+ j 1))))
              (ivec.push (&mut q) 0)
              (ivec.set (&mut dist) 0 0)
              (let [head : i64 0]
                (begin
                  (while (and (< head (ivec.len (& q)))
                              (= (ivec.get (& dist) (- total 1) 0) -1))
                    (let
                      [cur : i64 (ivec.get (& q) head 0)]
                      [cx : i64 (% cur size)]
                      [cy : i64 (/ cur size)]
                      (begin
                        (set! head (+ head 1))
                        (let [dd : i64 0]
                          (begin
                            (while (< dd 4)
                              (begin
                                (let
                                  [nx : i64 (+ cx (dir-dx dd))]
                                  [ny : i64 (+ cy (dir-dy dd))]
                                  [ni : i64 (+ (* ny size) nx)]
                                  (if (and (>= nx 0) (< nx size) (>= ny 0) (< ny size)
                                           (= (ivec.get (& blocked) ni 0) 0)
                                           (= (ivec.get (& dist) ni 0) -1))
                                    (begin
                                      (ivec.set (&mut dist) ni (+ (ivec.get (& dist) cur 0) 1))
                                      (ivec.push (&mut q) ni))
                                    unit))
                                (set! dd (+ dd 1))))))
                      unit))))
              (io.print-string "day18 part1: ")
              (io.print-string (string.int->string (ivec.get (& dist) (- total 1) 0)))
              (io.print-newline))
            ;; part 2: binary search the first count with no path
            (let
              [lo : i64 fall1]
              [hi : i64 (/ (ivec.len (& nums)) 2)]
              (begin
                (while (< lo hi)
                  (let [mid : i64 (/ (+ lo hi) 2)]
                    (if (path-exists? (& xs) (& ys) mid size)
                      (set! lo (+ mid 1))
                      (set! hi mid))))
                (io.print-string "day18 part2: ")
                (io.print-string (string.int->string (ivec.get (& xs) (- lo 1) 0)))
                (io.print-string ",")
                (io.print-string (string.int->string (ivec.get (& ys) (- lo 1) 0)))
                (io.print-newline))))))))))

(define (main) : i64
  (begin
    (solve-day18 "inputs/day18.txt")
    0))
