;; Day 15: Warehouse Woes
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.hashmap)
(import stdlib.vector)
(import stdlib.string)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)
(import (hashmap.hashmap i64 i64) as boxmap)

(define (wall? [bytes : (& bytes)] [w : i64] [h : i64] [x : i64] [y : i64]) : bool
  (let [gx : i64 (/ x 2)]
    (if (and (>= y 0) (< y h) (>= gx 0) (< gx w))
      (= (byte_buf.bytes-get bytes (+ (* y (+ w 1)) gx) 0) (cast '#' : u8))
      true)))

(define (box-at? [boxes : (& boxes boxmap.Map)] [x : i64] [y : i64] [w2 : i64]) : bool
  (>= (boxmap.lookup-value-or (boxmap.get (& boxes) (+ (* y w2) x)) 0) 1))

(define (box-left-at [boxes : (& boxes boxmap.Map)] [x : i64] [y : i64] [w2 : i64]) : i64
  (if (box-at? boxes x y w2)
    x
    (if (and (>= x 1) (box-at? boxes (- x 1) y w2))
      (- x 1)
      -1)))

;; horizontal push; returns new robot x, or -1 if blocked
(define (push-h [bytes : (& bytes)] [w : i64] [h : i64] [boxes : (&mut boxes boxmap.Map)]
                [w2 : i64] [rx : i64] [ry : i64] [dx : i64]) : i64
  (let
    [nx : i64 (+ rx dx)]
    (begin
      (if (wall? bytes w h nx ry)
        rx
        (let [b : i64 (box-left-at (& boxes) nx ry w2)]
          (if (< b 0)
            nx
            (let
              [end-x : i64 nx]
              [blocked? : bool false]
              [done? : bool false]
              (begin
                (while (not done?)
                  (begin
                    (set! end-x (+ end-x dx))
                    (if (wall? bytes w h end-x ry)
                      (begin
                        (set! blocked? true)
                        (set! done? true))
                      (if (>= (box-left-at (& boxes) end-x ry w2) 0)
                        unit
                        (set! done? true))))))
                (if blocked?
                  rx
                  (begin
                    (boxmap.remove-ref! boxes (+ (* ry w2) nx))
                    (boxmap.insert! boxes (+ (* ry w2) end-x) 1)
                    nx)))))))))

;; vertical push; returns true if the robot moved
;; vertical push; returns true if the robot moved
(define (push-v [bytes : (& bytes)] [w : i64] [h : i64] [boxes : (&mut boxes boxmap.Map)]
                [w2 : i64] [rx : i64] [ry : i64] [dy : i64]) : bool
  (let
    [ny : i64 (+ ry dy)]
    (begin
      (if (wall? bytes w h rx ny)
        false
        (let
          [b : i64 (box-left-at (& boxes) rx ny w2)]
          (begin
            (if (< b 0)
              true
              (let
                [queue : ivec.Vec (ivec.new)]
                [moves : ivec.Vec (ivec.new)]
                [seen : boxmap.Map (boxmap.new)]
                [ok? : bool true]
                (begin
                  (ivec.push (&mut queue) b)
                  (boxmap.insert! seen (+ (* ny w2) b) 1)
                  (while (and ok? (> (ivec.len (& queue)) 0))
                    (let
                      [bx : i64 (ivec.get (& queue) (- (ivec.len (& queue)) 1) 0)]
                      (begin
                        (ivec.pop (&mut queue))
                        (ivec.push (&mut moves) bx)
                        (let [ny2 : i64 (+ ny dy)]
                          (begin
                            (cond
                              [(wall? bytes w h bx ny2) (set! ok? false)]
                              [(wall? bytes w h (+ bx 1) ny2) (set! ok? false)]
                              [else
                                (let
                                  [o1 : i64 (box-left-at (& boxes) bx ny2 w2)]
                                  [o2 : i64 (box-left-at (& boxes) (+ bx 1) ny2 w2)]
                                  (begin
                                    (if (and (>= o1 0)
                                             (< (boxmap.lookup-value-or (boxmap.get (& seen) (+ (* ny2 w2) o1)) 0) 1))
                                      (begin
                                        (boxmap.insert! seen (+ (* ny2 w2) o1) 1)
                                        (ivec.push (&mut queue) o1))
                                      unit)
                                    (if (and (>= o2 0)
                                             (< (boxmap.lookup-value-or (boxmap.get (& seen) (+ (* ny2 w2) o2)) 0) 1))
                                      (begin
                                        (boxmap.insert! seen (+ (* ny2 w2) o2) 1)
                                        (ivec.push (&mut queue) o2))
                                      unit)
                                    unit)
                                unit)])
                            unit)))))
                      (if ok?
                        (begin
                          (let [k : i64 0]
                            (begin
                              (while (< k (ivec.len (& moves)))
                                (begin
                                  (boxmap.remove-ref! boxes (+ (* ny w2) (ivec.get (& moves) k 0)))
                                  (set! k (+ k 1))))
                              (set! k 0)
                              (while (< k (ivec.len (& moves)))
                                (begin
                                  (boxmap.insert! boxes (+ (* (+ ny dy) w2) (ivec.get (& moves) k 0)) 1)
                                  (set! k (+ k 1)))))
                            true))
                        false))))))))))

(define (solve-day15 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [w : i64 0]
    [h : i64 0]
    [i : i64 0]
    (begin
      (while (and (= h 0) (< i nlines))
        (if (= (string.string-length (& (strvec.get (& lines) i ""))) 0)
          (set! h i)
          (set! i (+ i 1))))
      (set! w (string.string-length (& (strvec.get (& lines) 0 ""))))
      (let
        [grid-text : String ""]
        (begin
          (let [k : i64 0]
            (begin
              (while (< k h)
                (begin
                  (set! grid-text (string.append grid-text (strvec.get (& lines) k "")))
                  (set! grid-text (string.append grid-text "\n"))
                  (set! k (+ k 1)))))
          (let
            [gbytes : (& grid-text bytes) (byte_buf.str-as-bytes (& grid-text))]
            (begin
              (io.print-string "day15 part1: ")
              (io.print (simulate gbytes w h (& lines) nlines 1))
              (io.print-newline)
              (io.print-string "day15 part2: ")
              (io.print (simulate gbytes w h (& lines) nlines 2))
              (io.print-newline))))))))))))

(define (main) : i64
  (begin
    (solve-day15 "inputs/day15.txt")
    0))
