;; Day 16: Reindeer Maze
;; Dijkstra over (x, y, dir) states; part 2 = forward + backward distance,
;; cells on any best path satisfy fwd[s] + bwd[s] == best.
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; dir: 0=N, 1=E, 2=S, 3=W
(define (dir-dx [dir : i64]) : i64
  (if (= dir 1) 1 (if (= dir 3) (- 0 1) 0)))

(define (dir-dy [dir : i64]) : i64
  (if (= dir 0) (- 0 1) (if (= dir 2) 1 0)))

(define (wall? [bytes : (& bytes)] [w : i64] [h : i64] [x : i64] [y : i64]) : bool
  (if (and (>= x 0) (< x w) (>= y 0) (< y h))
    (= (byte_buf.bytes-get bytes (+ (* y (+ w 1)) x) 0) (cast '#' : u8))
    true))

(define (state-of [w : i64] [x : i64] [y : i64] [dir : i64]) : i64
  (+ (* (+ (* y w) x) 4) dir))

;; ---- binary min-heap over encoded entries: cost * nstates + state ----

(define (heap-push [h : (&mut h ivec.Vec)] [entry : i64]) : unit
  (let
    [i : i64 (ivec.len (& h))]
    (begin
      (ivec.push (&mut h) entry)
      (while (and (> i 0)
                  (< (ivec.get (& h) i 0) (ivec.get (& h) (/ (- i 1) 2) 0)))
        (let
          [p : i64 (/ (- i 1) 2)]
          [pv : i64 (ivec.get (& h) p 0)]
          (begin
            (ivec.set h i pv)
            (ivec.set h p entry)
            (set! i p))))
      unit)))

(define (heap-pop [h : (&mut h ivec.Vec)]) : i64
  (let
    [n : i64 (ivec.len (& h))]
    [top : i64 (ivec.get (& h) 0 0)]
    (begin
      (ivec.set h 0 (ivec.get (& h) (- n 1) 0))
      (ivec.pop (&mut h))
      (let [i : i64 0]
        (begin
          (while (< (+ (* i 2) 1) (ivec.len (& h)))
            (let
              [l : i64 (+ (* i 2) 1)]
              [r : i64 (+ (* i 2) 2)]
              [m : i64 l]
              (begin
                (if (and (< r (ivec.len (& h)))
                         (< (ivec.get (& h) r 0) (ivec.get (& h) l 0)))
                  (set! m r)
                  unit)
                (if (< (ivec.get (& h) m 0) (ivec.get (& h) i 0))
                  (let [tmp : i64 (ivec.get (& h) i 0)]
                    (begin
                      (ivec.set h i (ivec.get (& h) m 0))
                      (ivec.set h m tmp)
                      (set! i m)))
                  (set! i (ivec.len (& h)))))))
          unit))
      top)))

(define (relax [dist : (&mut dist ivec.Vec)] [heap : (&mut heap ivec.Vec)]
               [nstates : i64] [s : i64] [c : i64]) : unit
  (let [old : i64 (ivec.get (& dist) s 0)]
    (if (or (= old -1) (< c old))
      (begin
        (ivec.set dist s c)
        (heap-push (&mut heap) (+ (* c nstates) s)))
      unit)))

;; Dijkstra from all states in `starts` (cost 0). bwd = traverse the reversed
;; graph (moves go backwards; turns are symmetric).
(define (dijkstra [bytes : (& bytes)] [w : i64] [h : i64]
                  [starts : (& starts ivec.Vec)] [bwd : bool]) : ivec.Vec
  (let
    [nstates : i64 (* (* w h) 4)]
    [dist : ivec.Vec (ivec.new)]
    [heap : ivec.Vec (ivec.new)]
    [i : i64 0]
    [si : i64 0]
    (begin
      (while (< i nstates)
        (begin
          (ivec.push (&mut dist) -1)
          (set! i (+ i 1))))
      (while (< si (ivec.len (& starts)))
        (begin
          (relax (&mut dist) (&mut heap) nstates (ivec.get (& starts) si 0) 0)
          (set! si (+ si 1))))
      (while (> (ivec.len (& heap)) 0)
        (let
          [entry : i64 (heap-pop (&mut heap))]
          [c : i64 (/ entry nstates)]
          [s : i64 (- entry (* c nstates))]
          (begin
            (if (= c (ivec.get (& dist) s 0))
              (let
                [dir : i64 (% s 4)]
                [cell : i64 (/ s 4)]
                [y : i64 (/ cell w)]
                [x : i64 (- cell (* y w))]
                (begin
                  ;; move (or reverse-move in backward mode)
                  (let
                    [mvx : i64 (if bwd (- 0 (dir-dx dir)) (dir-dx dir))]
                    [mvy : i64 (if bwd (- 0 (dir-dy dir)) (dir-dy dir))]
                    [nx : i64 (+ x mvx)]
                    [ny : i64 (+ y mvy)]
                    (begin
                      (if (not (wall? bytes w h nx ny))
                        (relax (&mut dist) (&mut heap) nstates
                               (state-of w nx ny dir) (+ c 1))
                        unit)))
                  ;; turns
                  (let [nd : i64 0]
                    (begin
                      (while (< nd 4)
                        (begin
                          (if (not (= nd dir))
                            (relax (&mut dist) (&mut heap) nstates
                                   (state-of w x y nd) (+ c 1000))
                            unit)
                          (set! nd (+ nd 1))))))
                  unit))
              unit))))
      dist)))

(define (solve-day16 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [first : String (strvec.get (& lines) 0 "")]
    [w : i64 (string.string-length (& first))]
    [h : i64 (strvec.len (& lines))]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [sx : i64 0]
    [sy : i64 0]
    [ex : i64 0]
    [ey : i64 0]
    (begin
      (let [y : i64 0]
        (begin
          (while (< y h)
            (let [x : i64 0]
              (begin
                (while (< x w)
                  (begin
                    (let [ch : u8 (byte_buf.bytes-get bytes (+ (* y (+ w 1)) x) 0)]
                      (begin
                        (if (= ch (cast 'S' : u8))
                          (begin
                            (set! sx x)
                            (set! sy y))
                          unit)
                        (if (= ch (cast 'E' : u8))
                          (begin
                            (set! ex x)
                            (set! ey y))
                          unit)))
                    (set! x (+ x 1))))
                (set! y (+ y 1)))))
      (let
        [starts : ivec.Vec (ivec.new)]
        (begin
          (ivec.push (&mut starts) (state-of w sx sy 1))
          (let [fwd : ivec.Vec (dijkstra bytes w h (& starts) false)]
            (begin
              (let
                [best : i64 1000000000]
                [d : i64 0]
                (begin
                  (while (< d 4)
                    (begin
                      (let [v : i64 (ivec.get (& fwd) (state-of w ex ey d) 0)]
                        (if (and (>= v 0) (< v best))
                          (set! best v)
                          unit))
                      (set! d (+ d 1))))
                  (io.print-string "day16 part1: ")
                  (io.print best)
                  (io.print-newline)
                  (let
                    [bstart : ivec.Vec (ivec.new)]
                    [dd : i64 0]
                    (begin
                      (while (< dd 4)
                        (begin
                          (ivec.push (&mut bstart) (state-of w ex ey dd))
                          (set! dd (+ dd 1))))
                      (let [bwd : ivec.Vec (dijkstra bytes w h (& bstart) true)]
                        (begin
                          (let
                            [seen : ivec.Vec (ivec.new)]
                            [cnt : i64 0]
                            [cy : i64 0]
                            (begin
                              (let [t : i64 0]
                                (begin
                                  (while (< t (* w h))
                                    (begin
                                      (ivec.push (&mut seen) 0)
                                      (set! t (+ t 1))))))
                              (while (< cy h)
                                (let [cx : i64 0]
                                  (begin
                                    (while (< cx w)
                                      (begin
                                        (let
                                          [cd : i64 0]
                                          [on? : bool false]
                                          (begin
                                            (while (and (not on?) (< cd 4))
                                              (begin
                                                (let
                                                  [fs : i64 (ivec.get (& fwd) (state-of w cx cy cd) 0)]
                                                  [bs : i64 (ivec.get (& bwd) (state-of w cx cy cd) 0)]
                                                  (if (and (>= fs 0) (>= bs 0) (= (+ fs bs) best))
                                                    (set! on? true)
                                                    unit))
                                                (set! cd (+ cd 1))))
                                            (if (and on? (= (ivec.get (& seen) (+ (* cy w) cx) 0) 0))
                                              (begin
                                                (ivec.set (&mut seen) (+ (* cy w) cx) 1)
                                                (set! cnt (+ cnt 1)))
                                              unit)))
                                        (set! cx (+ cx 1))))
                                    (set! cy (+ cy 1)))))
                              (io.print-string "day16 part2: ")
                              (io.print cnt)
                              (io.print-newline))))))))))))))))))

(define (main) : i64
  (begin
    (solve-day16 "inputs/day16.txt")
    0))
