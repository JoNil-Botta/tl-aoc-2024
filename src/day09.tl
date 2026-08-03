;; Day 9: Disk Fragmenter
(import stdlib.io)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)

(defstruct Span
  (start i64)
  (len i64))

(import (vector.vector Span) as spanvec)

;; checksum of a layout (skip -1)
(define (checksum [layout : (& layout ivec.Vec)]) : i64
  (let
    [n : i64 (ivec.len layout)]
    [total : i64 0]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (let [v : i64 (ivec.get layout i 0)]
            (if (>= v 0)
              (set! total (+ total (* i v)))
              unit))
          (set! i (+ i 1))))
      total)))

;; copy a layout vec
(define (copy-layout [layout : (& layout ivec.Vec)]) : ivec.Vec
  (let
    [n : i64 (ivec.len layout)]
    [out : ivec.Vec (ivec.with-capacity n)]
    [i : i64 0]
    (begin
      (while (< i n)
        (begin
          (ivec.push (&mut out) (ivec.get layout i 0))
          (set! i (+ i 1))))
      out)))

(define (solve-day09 [path : String]) : unit
  (let
    [text : String (u.read-input path)]
    [bytes : (& text bytes) (byte_buf.str-as-bytes (& text))]
    [n : i64 (byte_buf.bytes-length bytes)]
    [layout : ivec.Vec (ivec.new)]
    [files : spanvec.Vec (spanvec.new)]
    [spans : spanvec.Vec (spanvec.new)]
    [i : i64 0]
    (begin
      ;; build layout + file/free records from the digit run
      (while (< i n)
        (let
          [d : i64 (- (cast (byte_buf.bytes-get bytes i 0) : i64) 48)]
          [pos : i64 (ivec.len (& layout))]
          (begin
            (if (= (bit-and i 1) 0)
              (let [k : i64 0]
                (begin
                  (while (< k d)
                    (begin
                      (ivec.push (&mut layout) (/ i 2))
                      (set! k (+ k 1))))
                  (spanvec.push (&mut files) (Span pos d))
                  unit))
              (let [k : i64 0]
                (begin
                  (while (< k d)
                    (begin
                      (ivec.push (&mut layout) -1)
                      (set! k (+ k 1))))
                  (if (> d 0)
                    (begin
                      (spanvec.push (&mut spans) (Span pos d))
                      unit)
                    unit)
                  unit)))
            (set! i (+ i 1)))))
      ;; part 1: block-by-block compaction (on a copy)
      (let
        [layout1 : ivec.Vec (copy-layout (& layout))]
        [l : i64 0]
        [r : i64 (- (ivec.len (& layout1)) 1)]
        (begin
          (while (< l r)
            (begin
              (if (>= (ivec.get (& layout1) l 0) 0)
                (set! l (+ l 1))
                (if (< (ivec.get (& layout1) r 0) 0)
                  (set! r (- r 1))
                  (begin
                    (ivec.set (&mut layout1) l (ivec.get (& layout1) r 0))
                    (ivec.set (&mut layout1) r -1)
                    (set! l (+ l 1))
                    (set! r (- r 1)))))))
          (io.print-string "day09 part1: ")
          (io.print (checksum (& layout1)))
          (io.print-newline)))
      ;; part 2: whole-file moves
      (let
        [fid : i64 (- (spanvec.len (& files)) 1)]
        (begin
          (while (>= fid 0)
            (begin
              (let
                [fs : Span (spanvec.get (& files) fid (Span 0 0))]
                [flen : i64 fs.len]
                [fstart : i64 fs.start]
                [si : i64 0]
                [moved? : bool false]
                [past? : bool false]
                (begin
                  (while (and (not moved?) (not past?) (< si (spanvec.len (& spans))))
                    (begin
                      (let
                        [ss : Span (spanvec.get (& spans) si (Span 0 0))]
                        [sstart : i64 ss.start]
                        [slen : i64 ss.len]
                        (begin
                          (if (>= sstart fstart)
                            ;; spans are position-sorted: nothing after can fit
                            (set! past? true)
                            (if (>= slen flen)
                              (begin
                                ;; move file into span
                                (let [k : i64 0]
                                  (begin
                                    (while (< k flen)
                                      (begin
                                        (ivec.set (&mut layout) (+ sstart k) fid)
                                        (ivec.set (&mut layout) (+ fstart k) -1)
                                        (set! k (+ k 1))))
                                    (if (= slen flen)
                                      ;; mark consumed (len 0) instead of shifting
                                      (spanvec.set (&mut spans) si (Span sstart 0))
                                      (spanvec.set (&mut spans) si (Span (+ sstart flen) (- slen flen))))
                                    (set! moved? true))))
                              unit)))
                      (set! si (+ si 1))))))
                (set! fid (- fid 1)))))
          (io.print-string "day09 part2: ")
          (io.print (checksum (& layout)))
          (io.print-newline))))))

(define (main) : i64
  (begin
    (solve-day09 "inputs/day09.txt")
    0))
