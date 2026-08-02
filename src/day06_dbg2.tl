(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector u8) as u8vec)
(import day06 as d6)
(define (main) : i64
  (let
    [text : String (u.read-input "/tmp/day06_ex.txt")]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [w : i64 0]
    [wfound? : bool false]
    (begin
      (while (and (not wfound?) (< w n))
        (if (= (byte_buf.bytes-get bytes w 0) (cast '\n' : u8))
          (set! wfound? true)
          (set! w (+ w 1))))
      (let [h : i64 (/ n (+ w 1))]
        (begin
          ;; part1 path
          (let
            [visited : u8vec.Vec (u8vec.with-capacity (* w h))]
            [k : i64 0]
            [cnt : i64 0]
            (begin
              (while (< k (* w h))
                (begin
                  (u8vec.push (&mut visited) 0)
                  (set! k (+ k 1))))
              (d6.simulate bytes w h 6 4 (- 0 1) (- 0 1) (&mut visited))
              ;; candidate loop count
              (let
                [loops : i64 0]
                [r2 : i64 0]
                (begin
                  (while (< r2 h)
                    (let [c2 : i64 0]
                      (begin
                        (while (< c2 w)
                          (begin
                            (if (>= (cast (u8vec.get (& visited) (+ (* r2 w) c2) 0) : i64) 1)
                              (let [k3 : i64 0]
                                (begin
                                  (while (< k3 (* w h))
                                    (begin
                                      (u8vec.set (&mut visited) k3 0)
                                      (set! k3 (+ k3 1))))
                                  (if (= (d6.simulate bytes w h 6 4 r2 c2 (&mut visited)) 1)
                                    (begin
                                      (set! loops (+ loops 1))
                                      (io.print r2)
                                      (io.print-string ",")
                                      (io.print c2)
                                      (io.print-string " "))
                                    unit)))
                              unit)
                            (set! c2 (+ c2 1))))
                        (set! r2 (+ r2 1)))))
                  (io.print-string "LOOPS=")
                  (io.print loops)
                  (io.print-newline)
                  (io.print-string "done")
                  (io.print-newline)
                  0)))))))))
