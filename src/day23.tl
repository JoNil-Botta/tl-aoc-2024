;; Day 23: LAN Party
;; Part 1: triangles containing a node starting with 't'.
;; Part 2: maximum clique via Bron-Kerbosch (branch and bound), answer = sorted
;; names joined by commas.
(import stdlib.io)
(import stdlib.string)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; two-letter name id -> "xy" string
(define (name-str [id : i64]) : String
  (let
    [b1 : byte_buf.ByteBuf
      (byte_buf.push (byte_buf.new) (cast (+ 97 (/ id 26)) : u8))]
    [b2 : byte_buf.ByteBuf
      (byte_buf.push b1 (cast (+ 97 (% id 26)) : u8))]
    (byte_buf.bytes-to-string (byte_buf.as-bytes (& b2)))))

;; replace dst contents with src contents
(define (copy-into [dst : (&mut dst ivec.Vec)] [src : (& src ivec.Vec)]) : unit
  (begin
    (while (> (ivec.len (& dst)) 0)
      (begin
        (ivec.pop (&mut dst))
        unit))
    (let [k : i64 0]
      (begin
        (while (< k (ivec.len (& src)))
          (begin
            (ivec.push (&mut dst) (ivec.get (& src) k 0))
            (set! k (+ k 1))))
        unit))))

(define (append-val [src : (& src ivec.Vec)] [v : i64]) : ivec.Vec
  (let
    [r : ivec.Vec (ivec.new)]
    [k : i64 0]
    (begin
      (while (< k (ivec.len (& src)))
        (begin
          (ivec.push (&mut r) (ivec.get (& src) k 0))
          (set! k (+ k 1))))
      (ivec.push (&mut r) v)
      r)))

;; cands[from..] intersected with the neighbors of v
(define (neighbors-from [cands : (& cands ivec.Vec)] [from : i64] [v : i64]
                        [adjmat : (& adjmat ivec.Vec)]) : ivec.Vec
  (let
    [r : ivec.Vec (ivec.new)]
    [k : i64 from]
    (begin
      (while (< k (ivec.len (& cands)))
        (begin
          (let [w : i64 (ivec.get (& cands) k 0)]
            (if (= (ivec.get (& adjmat) (+ (* v 676) w) 0) 1)
              (ivec.push (&mut r) w)
              unit))
          (set! k (+ k 1))))
      r)))

;; Bron-Kerbosch with branch and bound; best keeps the largest clique
(define (bronk [clique : (& clique ivec.Vec)] [cands : (& cands ivec.Vec)]
               [idx : i64] [adjmat : (& adjmat ivec.Vec)]
               [best : (&mut best ivec.Vec)]) : unit
  (let [n : i64 (ivec.len (& cands))]
    (if (>= idx n)
      (if (> (ivec.len (& clique)) (ivec.len (& best)))
        (copy-into best clique)
        unit)
      (if (<= (+ (ivec.len (& clique)) (- n idx)) (ivec.len (& best)))
        unit
        (begin
          (let
            [v : i64 (ivec.get (& cands) idx 0)]
            [nc : ivec.Vec (append-val clique v)]
            [ncands : ivec.Vec (neighbors-from cands (+ idx 1) v adjmat)]
            (bronk (& nc) (& ncands) 0 adjmat best))
          (bronk clique cands (+ idx 1) adjmat best))))))

(define (solve-day23 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [adjmat : ivec.Vec (ivec.new)]
    [deg : ivec.Vec (ivec.new)]
    [idmap : ivec.Vec (ivec.new)]
    [ids : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i 456976)
        (begin
          (ivec.push (&mut adjmat) 0)
          (set! i (+ i 1))))
      (set! i 0)
      (while (< i 676)
        (begin
          (ivec.push (&mut deg) 0)
          (ivec.push (&mut idmap) 0)
          (set! i (+ i 1))))
      (set! i 0)
      (while (< i nlines)
        (let
          [line : String (strvec.get (& lines) i "")]
          [a : i64 (- (cast (string.string-ref (& line) 0) : i64) 97)]
          [b : i64 (- (cast (string.string-ref (& line) 1) : i64) 97)]
          [c : i64 (- (cast (string.string-ref (& line) 3) : i64) 97)]
          [d : i64 (- (cast (string.string-ref (& line) 4) : i64) 97)]
          [id1 : i64 (+ (* a 26) b)]
          [id2 : i64 (+ (* c 26) d)]
          (begin
            (if (= (ivec.get (& idmap) id1 0) 0)
              (begin
                (ivec.set (&mut idmap) id1 1)
                (ivec.push (&mut ids) id1))
              unit)
            (if (= (ivec.get (& idmap) id2 0) 0)
              (begin
                (ivec.set (&mut idmap) id2 1)
                (ivec.push (&mut ids) id2))
              unit)
            (ivec.set (&mut adjmat) (+ (* id1 676) id2) 1)
            (ivec.set (&mut adjmat) (+ (* id2 676) id1) 1)
            (ivec.set (&mut deg) id1 (+ (ivec.get (& deg) id1 0) 1))
            (ivec.set (&mut deg) id2 (+ (ivec.get (& deg) id2 0) 1))
            (set! i (+ i 1)))))
      ;; part 1: triangles with a 't' node
      (let
        [p1 : i64 0]
        [ai : i64 0]
        (begin
          (while (< ai (ivec.len (& ids)))
            (let
              [u : i64 (ivec.get (& ids) ai 0)]
              [bi : i64 (+ ai 1)]
              (begin
                (while (< bi (ivec.len (& ids)))
                  (let
                    [v : i64 (ivec.get (& ids) bi 0)]
                    (begin
                      (if (= (ivec.get (& adjmat) (+ (* u 676) v) 0) 1)
                        (let [ci : i64 (+ bi 1)]
                          (begin
                            (while (< ci (ivec.len (& ids)))
                              (let [w : i64 (ivec.get (& ids) ci 0)]
                                (if (and (= (ivec.get (& adjmat) (+ (* u 676) w) 0) 1)
                                         (= (ivec.get (& adjmat) (+ (* v 676) w) 0) 1)
                                         (or (= (/ u 26) 19)
                                             (= (/ v 26) 19)
                                             (= (/ w 26) 19)))
                                  (set! p1 (+ p1 1))
                                  unit))
                              (set! ci (+ ci 1)))))
                        unit)
                      (set! bi (+ bi 1)))))
                (set! ai (+ ai 1)))))
          (io.print-string "day23 part1: ")
          (io.print-string (string.int->string p1))
          (io.print-newline))
      ;; part 2: sort ids by degree descending as initial candidates
      (let
        [cands : ivec.Vec (ivec.new)]
        [clique : ivec.Vec (ivec.new)]
        [best : ivec.Vec (ivec.new)]
        [j : i64 0]
        (begin
          (while (< j (ivec.len (& ids)))
            (begin
              (ivec.push (&mut cands) (ivec.get (& ids) j 0))
              (set! j (+ j 1))))
          ;; insertion sort by degree descending
          (let [a2 : i64 1]
            (begin
              (while (< a2 (ivec.len (& cands)))
                (let
                  [key : i64 (ivec.get (& cands) a2 0)]
                  [p : i64 (- a2 1)]
                  (begin
                    (while (and (>= p 0)
                                (> (ivec.get (& deg) key 0)
                                   (ivec.get (& deg) (ivec.get (& cands) p 0) 0)))
                      (begin
                        (ivec.set (&mut cands) (+ p 1) (ivec.get (& cands) p 0))
                        (set! p (- p 1))))
                    (ivec.set (&mut cands) (+ p 1) key)
                    (set! a2 (+ a2 1))))))
          (bronk (& clique) (& cands) 0 (& adjmat) (&mut best))
          ;; print best sorted ascending
          (let
            [b2 : i64 0]
            (begin
              (while (< b2 (ivec.len (& best)))
                (let [p : i64 (- b2 1)]
                  (begin
                    (while (and (>= p 0)
                                (< (ivec.get (& best) (+ p 1) 0)
                                   (ivec.get (& best) p 0)))
                      (begin
                        (let [tmp : i64 (ivec.get (& best) p 0)]
                          (begin
                            (ivec.set (&mut best) p (ivec.get (& best) (+ p 1) 0))
                            (ivec.set (&mut best) (+ p 1) tmp)))
                        (set! p (- p 1))))
                    (set! b2 (+ b2 1)))))
              (io.print-string "day23 part2: ")
              (let [k2 : i64 0]
                (begin
                  (while (< k2 (ivec.len (& best)))
                    (begin
                      (if (> k2 0)
                        (io.print-string ",")
                        unit)
                      (io.print-string (name-str (ivec.get (& best) k2 0)))
                      (set! k2 (+ k2 1))))
                  (io.print-newline))))))))))))

(define (main) : i64
  (begin
    (solve-day23 "inputs/day23.txt")
    0))
