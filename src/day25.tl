;; Day 25: Code Chronicle
;; 500 blocks (250 locks + 250 keys), each 7 rows x 5 columns.
;; Locks: top row all '#'; keys: bottom row all '#'.
;; Part 1: count lock/key pairs where pin height + key depth <= 5 in every column.
;; Part 2 is the free star (50/50).
(import stdlib.io)
(import stdlib.string)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; Column height for a lock (count '#' down from row 1) or a key
;; (count '#' up from row 5).
(define (col-height [block : (& block strvec.Vec)] [c : i64] [from-top? : bool]) : i64
  (let [h : i64 0]
    (begin
      (if from-top?
        (let [r : i64 1]
          (begin
            (while (< r 6)
              (let [row : String (strvec.get (& block) r "")]
                (begin
                  (if (= (string.string-ref (& row) c) '#')
                    (begin
                      (set! h (+ h 1))
                      (set! r (+ r 1)))
                    (set! r 6)))))
            unit))
        (let [r : i64 5]
          (begin
            (while (>= r 1)
              (let [row : String (strvec.get (& block) r "")]
                (begin
                  (if (= (string.string-ref (& row) c) '#')
                    (begin
                      (set! h (+ h 1))
                      (set! r (- r 1)))
                    (set! r 0)))))
            unit)))
      h)))

;; Process one 7-row block into locks or keys (5 heights each).
(define (process-block [block : (& block strvec.Vec)]
                       [locks : (&mut locks ivec.Vec)]
                       [keys : (&mut keys ivec.Vec)]) : unit
  (let
    [top : String (strvec.get (& block) 0 "")]
    [is-lock? : bool (= (string.string-ref (& top) 0) '#')]
    [c : i64 0]
    (begin
      (while (< c 5)
        (begin
          (if is-lock?
            (ivec.push (&mut locks) (col-height block c true))
            (ivec.push (&mut keys) (col-height block c false)))
          (set! c (+ c 1)))))))

;; Does lock li and key ki fit together?
(define (fits? [locks : (& locks ivec.Vec)] [keys : (& keys ivec.Vec)]
               [li : i64] [ki : i64]) : bool
  (let [c : i64 0] [ok? : bool true]
    (begin
      (while (and ok? (< c 5))
        (begin
          (if (> (+ (ivec.get (& locks) (+ (* li 5) c) 0)
                    (ivec.get (& keys) (+ (* ki 5) c) 0)) 5)
            (set! ok? false)
            unit)
          (set! c (+ c 1))))
      ok?)))

(define (solve-day25 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [locks : ivec.Vec (ivec.new)]
    [keys : ivec.Vec (ivec.new)]
    [block : strvec.Vec (strvec.new)]
    [i : i64 0]
    (begin
      (while (< i nlines)
        (let [line : String (strvec.get (& lines) i "")]
          (begin
            (if (> (string.string-length (& line)) 0)
              (strvec.push (&mut block) line)
              unit)
            (if (>= (strvec.len (& block)) 7)
              (begin
                (process-block (& block) (&mut locks) (&mut keys))
                (while (> (strvec.len (& block)) 0)
                  (begin
                    (strvec.pop (&mut block))
                    unit)))
              unit)
            (set! i (+ i 1)))))
      (let
        [nl : i64 (/ (ivec.len (& locks)) 5)]
        [nk : i64 (/ (ivec.len (& keys)) 5)]
        [total : i64 0]
        [li : i64 0]
        (begin
          (while (< li nl)
            (let [ki : i64 0]
              (begin
                (while (< ki nk)
                  (begin
                    (if (fits? (& locks) (& keys) li ki)
                      (set! total (+ total 1))
                      unit)
                    (set! ki (+ ki 1))))
                (set! li (+ li 1)))))
          (io.print-string "day25 part1: ")
          (io.print-string (string.int->string total))
          (io.print-newline)
          (io.print-string "day25 part2: free star (50/50 stars complete)")
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day25 "inputs/day25.txt")
    0))
