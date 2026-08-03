;; Day 15: Warehouse Woes
;; Scale 1: normal grid, 1-wide boxes. Scale 2: doubled grid, 2-wide boxes.
;; Boxes are stored in a hashmap keyed by (y*w2 + left_edge_x) -> 1.
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.hashmap)
(import stdlib.vector)
(import stdlib.string)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)
(import (hashmap.hashmap i64 i64) as boxmap)

;; is the cell a wall? x/y are in scaled coordinates; the grid bytes are the
;; original (unscaled) text, so map back via x/2. Out of bounds counts as wall.
(define (wall? [bytes : (& bytes)] [w : i64] [h : i64] [x : i64] [y : i64] [scale : i64]) : bool
  (let [gx : i64 (/ x scale)]
    (if (and (>= y 0) (< y h) (>= gx 0) (< gx w))
      (= (byte_buf.bytes-get bytes (+ (* y (+ w 1)) gx) 0) (cast '#' : u8))
      true)))

;; is there a box whose left edge is exactly at (x, y)?
(define (box-at? [boxes : (& boxes boxmap.Map)] [x : i64] [y : i64] [w2 : i64]) : bool
  (>= (boxmap.lookup-value-or (boxmap.get (& boxes) (+ (* y w2) x)) 0) 1))

;; left edge of the box covering cell (x, y), or -1 if none
(define (box-left-at [boxes : (& boxes boxmap.Map)] [x : i64] [y : i64] [w2 : i64]
                     [scale : i64]) : i64
  (if (box-at? boxes x y w2)
    x
    (if (and (>= scale 2) (>= x 1) (box-at? boxes (- x 1) y w2))
      (- x 1)
      -1)))

;; move every box in the chain (list of left edges) by dx along row ry
(define (shift-chain-h [boxes : (&mut boxes boxmap.Map)] [chain : (& chain ivec.Vec)]
                       [ry : i64] [w2 : i64] [dx : i64]) : unit
  (let [k : i64 0]
    (begin
      (while (< k (ivec.len (& chain)))
        (begin
          (boxmap.remove-ref! boxes (+ (* ry w2) (ivec.get (& chain) k 0)))
          (set! k (+ k 1))))
      (set! k 0)
      (while (< k (ivec.len (& chain)))
        (begin
          (boxmap.insert! boxes (+ (* ry w2) (+ (ivec.get (& chain) k 0) dx)) 1)
          (set! k (+ k 1))))
      unit)))

;; horizontal push; returns the robot's new x, or -1 if blocked
(define (push-h [bytes : (& bytes)] [w : i64] [h : i64] [boxes : (&mut boxes boxmap.Map)]
                [w2 : i64] [scale : i64] [rx : i64] [ry : i64] [dx : i64]) : i64
  (let
    [nx : i64 (+ rx dx)]
    [b : i64 -1]
    (begin
      (if (wall? bytes w h nx ry scale)
        -1
        (begin
          (set! b (box-left-at (& boxes) nx ry w2 scale))
          (if (< b 0)
            nx
            (let
              [chain : ivec.Vec (ivec.new)]
              [cur : i64 b]
              [next? : bool true]
              (begin
                (ivec.push (&mut chain) b)
                (while next?
                  (let [probe : i64 (+ cur (* scale dx))]
                    (if (box-at? (& boxes) probe ry w2)
                      (begin
                        (ivec.push (&mut chain) probe)
                        (set! cur probe))
                      (set! next? false))))
                (let [end-probe : i64 (if (> dx 0) (+ cur scale) (- cur 1))]
                  (if (wall? bytes w h end-probe ry scale)
                    -1
                    (begin
                      (shift-chain-h boxes (& chain) ry w2 dx)
                      nx)))))))))))

;; move every box in the vertical chain (list of box keys) by dy rows
(define (shift-chain-v [boxes : (&mut boxes boxmap.Map)] [chain : (& chain ivec.Vec)]
                       [w2 : i64] [dy : i64]) : unit
  (let [k : i64 0]
    (begin
      (while (< k (ivec.len (& chain)))
        (begin
          (boxmap.remove-ref! boxes (ivec.get (& chain) k 0))
          (set! k (+ k 1))))
      (set! k 0)
      (while (< k (ivec.len (& chain)))
        (begin
          (boxmap.insert! boxes (+ (ivec.get (& chain) k 0) (* w2 dy)) 1)
          (set! k (+ k 1))))
      unit)))

(defstruct PushResult
  (ok bool)
  (moves ivec.Vec))

;; BFS over boxes: from the box whose left edge is (b, ny), collect every box
;; that would move when pushed by dy; ok? goes false if any front cell is a wall
(define (collect-v [bytes : (& bytes)] [w : i64] [h : i64] [boxes : (& boxes boxmap.Map)]
                   [w2 : i64] [scale : i64] [b : i64] [ny : i64] [dy : i64]) : PushResult
  (let
    [queue : ivec.Vec (ivec.new)]
    [moves : ivec.Vec (ivec.new)]
    [seen : boxmap.Map (boxmap.new)]
    [ok? : bool true]
    (begin
      (ivec.push (&mut queue) (+ (* ny w2) b))
      (boxmap.insert! seen (+ (* ny w2) b) 1)
      (while (and ok? (> (ivec.len (& queue)) 0))
        (let
          [key : i64 (ivec.get (& queue) (- (ivec.len (& queue)) 1) 0)]
          (begin
            (ivec.pop (&mut queue))
            (ivec.push (&mut moves) key)
            (let
              [brow : i64 (/ key w2)]
              [bx : i64 (- key (* brow w2))]
              [ny2 : i64 (+ brow dy)]
              (begin
                (let [ci : i64 0]
                  (begin
                    (while (and ok? (< ci scale))
                      (begin
                        (let [cx : i64 (+ bx ci)]
                          (begin
                            (if (wall? bytes w h cx ny2 scale)
                              (set! ok? false)
                              (let [o : i64 (box-left-at (& boxes) cx ny2 w2 scale)]
                                (if (and (>= o 0)
                                         (< (boxmap.lookup-value-or (boxmap.get (& seen) (+ (* ny2 w2) o)) 0) 1))
                                  (begin
                                    (boxmap.insert! seen (+ (* ny2 w2) o) 1)
                                    (ivec.push (&mut queue) (+ (* ny2 w2) o)))
                                  unit))))
                        (set! ci (+ ci 1))))
                    unit))
                unit))))))
      (PushResult ok? moves))))

;; vertical push; returns true if the robot moved
(define (push-v [bytes : (& bytes)] [w : i64] [h : i64] [boxes : (&mut boxes boxmap.Map)]
                [w2 : i64] [scale : i64] [rx : i64] [ry : i64] [dy : i64]) : bool
  (let [ny : i64 (+ ry dy)]
    (if (wall? bytes w h rx ny scale)
      false
      (if (< (box-left-at (& boxes) rx ny w2 scale) 0)
        true
        (let
          [res : PushResult (collect-v bytes w h (& boxes) w2 scale (box-left-at (& boxes) rx ny w2 scale) ny dy)]
          (if res.ok
            (begin
              (shift-chain-v boxes (& res.moves) w2 dy)
              true)
            false))))))

(defstruct RPos
  (x i64)
  (y i64))

;; apply one move to the robot; returns the new robot position
(define (apply-move [gbytes : (& gbytes bytes)] [w : i64] [h : i64]
                    [boxes : (&mut boxes boxmap.Map)] [w2 : i64] [scale : i64]
                    [rx : i64] [ry : i64] [mv : char]) : RPos
  (if (= mv '<')
    (let [nx : i64 (push-h gbytes w h boxes w2 scale rx ry (- 0 1))]
      (if (>= nx 0)
        (RPos nx ry)
        (RPos rx ry)))
    (if (= mv '>')
      (let [nx : i64 (push-h gbytes w h boxes w2 scale rx ry 1)]
        (if (>= nx 0)
          (RPos nx ry)
          (RPos rx ry)))
      (if (= mv '^')
        (if (push-v gbytes w h boxes w2 scale rx ry (- 0 1))
          (RPos rx (- ry 1))
          (RPos rx ry))
        (if (= mv 'v')
          (if (push-v gbytes w h boxes w2 scale rx ry 1)
            (RPos rx (+ ry 1))
            (RPos rx ry))
          (RPos rx ry))))))

;; run the full simulation for a given scale (1 or 2); returns the GPS sum
(define (simulate [gbytes : (& gbytes bytes)] [w : i64] [h : i64]
                  [lines : (& lines strvec.Vec)] [nlines : i64] [scale : i64]) : i64
  (let
    [w2 : i64 (* w scale)]
    [boxes : boxmap.Map (boxmap.new)]
    [rx : i64 0]
    [ry : i64 0]
    (begin
      (let [y : i64 0]
        (begin
          (while (< y h)
            (let [x : i64 0]
              (begin
                (while (< x w)
                  (begin
                    (let [ch : u8 (byte_buf.bytes-get gbytes (+ (* y (+ w 1)) x) 0)]
                      (begin
                        (if (= ch (cast 'O' : u8))
                          (boxmap.insert! boxes (+ (* y w2) (* x scale)) 1)
                          unit)
                        (if (= ch (cast '@' : u8))
                          (begin
                            (set! rx (* x scale))
                            (set! ry y))
                          unit)
                        unit))
                    (set! x (+ x 1))
                    unit))
                (set! y (+ y 1))
                unit))))
      (let [m : i64 (+ h 1)]
        (begin
          (while (< m nlines)
            (let
              [line : String (strvec.get (& lines) m "")]
              [mlen : i64 (string.string-length (& line))]
              [k2 : i64 0]
              (begin
                (while (< k2 mlen)
                  (begin
                    (let [mv : char (string.string-ref (& line) k2)]
                      (let [np : RPos (apply-move gbytes w h (&mut boxes) w2 scale rx ry mv)]
                        (begin
                          (set! rx np.x)
                          (set! ry np.y)
                          unit)))
                    (set! k2 (+ k2 1))
                    unit))
                (set! m (+ m 1))
                unit))))
      (let
        [total : i64 0]
        [y2 : i64 0]
        (begin
          (while (< y2 h)
            (let [x2 : i64 0]
              (begin
                (while (< x2 w2)
                  (begin
                    (if (>= (boxmap.lookup-value-or (boxmap.get (& boxes) (+ (* y2 w2) x2)) 0) 1)
                      (set! total (+ total (+ (* y2 100) x2)))
                      unit)
                    (set! x2 (+ x2 1))
                    unit))
                (set! y2 (+ y2 1))
                unit)))
          total)))))))

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
        (let [cur : String (strvec.get (& lines) i "")]
          (if (= (string.string-length (& cur)) 0)
            (set! h i)
            (set! i (+ i 1)))))
      (let [first : String (strvec.get (& lines) 0 "")]
        (set! w (string.string-length (& first))))
      (let
        [gbytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
        (begin
              (io.print-string "day15 part1: ")
              (io.print (simulate gbytes w h (& lines) nlines 1))
              (io.print-newline)
              (io.print-string "day15 part2: ")
              (io.print (simulate gbytes w h (& lines) nlines 2))
              (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day15 "inputs/day15.txt")
    0))
