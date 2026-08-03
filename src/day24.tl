;; Day 24: Crossed Wires
;; Part 1: simulate the gates, read the z bits.
;; Part 2: find the 4 swapped output pairs. Structural rules flag suspicious
;; wires, then all pairings of the candidates are verified by simulation
;; (x=y=0, every single-bit test, plus deterministic random tests).
;; Wire ids: 3-char names encoded as (c0-'a')*676 + (c1-'a')*26 + (c2-'a').
(import stdlib.io)
(import stdlib.string)
(import stdlib.byte_buf)
(import stdlib.vector)
(import util as u)
(import (vector.vector i64) as ivec)
(import (vector.vector String) as strvec)

;; name char value: a-z = 0..25, 0-9 = 26..35
(define (name-ch [ch : char]) : i64
  (if (and (>= (cast ch : i64) 97) (<= (cast ch : i64) 122))
    (- (cast ch : i64) 97)
    (+ (- (cast ch : i64) 48) 26)))

(define (wid3 [c0 : i64] [c1 : i64] [c2 : i64]) : i64
  (+ (* c0 1296) (+ (* c1 36) c2)))

(define (name-chr [v : i64]) : u8
  (if (< v 26)
    (cast (+ 97 v) : u8)
    (cast (+ 48 (- v 26)) : u8)))

(define (name-str [id : i64]) : String
  (let
    [b1 : byte_buf.ByteBuf
      (byte_buf.push (byte_buf.new) (name-chr (/ id 1296)))]
    [b2 : byte_buf.ByteBuf
      (byte_buf.push b1 (name-chr (% (/ id 36) 36)))]
    [b3 : byte_buf.ByteBuf
      (byte_buf.push b2 (name-chr (% id 36)))]
    (byte_buf.bytes-to-string (byte_buf.as-bytes (& b3)))))

;; z bit index from a z-wire id
(define (z-bit-of [id : i64]) : i64
  (let
    [n : i64 (- id 33362)]
    (+ (* (/ n 36) 10) (% n 36))))

;; find the gate whose output is w, or -1
(define (find-producer [go : (& go ivec.Vec)] [w : i64] [ngates : i64]) : i64
  (let
    [g : i64 0]
    [r : i64 -1]
    (begin
      (while (and (< g ngates) (= r -1))
        (begin
          (if (= (ivec.get (& go) g 0) w)
            (set! r g)
            unit)
          (set! g (+ g 1))))
      r)))

;; fixpoint evaluation; every wire set is appended to touched
(define (evaluate [vals : (&mut vals ivec.Vec)]
                  [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                  [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                  [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                  [touched : (&mut touched ivec.Vec)] [ngates : i64]) : unit
  (let
    [q : ivec.Vec (ivec.new)]
    [head : i64 0]
    [g : i64 0]
    (begin
      (while (< g ngates)
        (begin
          (let
            [va : i64 (ivec.get (& vals) (ivec.get (& ga) g 0) 0)]
            [vb : i64 (ivec.get (& vals) (ivec.get (& gb) g 0) 0)]
            (if (and (>= va 0) (>= vb 0))
              (ivec.push (&mut q) g)
              unit))
          (set! g (+ g 1))))
      (while (< head (ivec.len (& q)))
        (let
          [cg : i64 (ivec.get (& q) head 0)]
          (begin
            (set! head (+ head 1))
            (let
              [wo : i64 (ivec.get (& go) cg 0)]
              [va : i64 (ivec.get (& vals) (ivec.get (& ga) cg 0) 0)]
              [vb : i64 (ivec.get (& vals) (ivec.get (& gb) cg 0) 0)]
              [vo : i64 (ivec.get (& vals) wo 0)]
              (if (and (>= va 0) (>= vb 0) (< vo 0))
                (begin
                  (ivec.set (&mut vals) wo
                    (if (= (ivec.get (& gop) cg 0) 0)
                      (bit-and va vb)
                      (if (= (ivec.get (& gop) cg 0) 1)
                        (bit-or va vb)
                        (bit-xor va vb))))
                  (ivec.push (&mut touched) wo)
                  (let
                    [cc1 : i64 (ivec.get (& cons) wo 0)]
                    [cc2 : i64 (ivec.get (& cons2) wo 0)]
                    (begin
                      (if (>= cc1 0)
                        (ivec.push (&mut q) cc1)
                        unit)
                      (if (>= cc2 0)
                        (ivec.push (&mut q) cc2)
                        unit)))
                  unit)
                unit))))
      unit))))

;; one addition test: set x/y, evaluate, read z, compare with expected
(define (run-test [x : i64] [y : i64] [exp : i64]
                  [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                  [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                  [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                  [vals : (&mut vals ivec.Vec)] [touched : (&mut touched ivec.Vec)]
                  [nbits : i64] [ngates : i64]) : bool
  (begin
    (while (> (ivec.len (& touched)) 0)
      (begin
        (ivec.set (&mut vals) (ivec.get (& touched) (- (ivec.len (& touched)) 1) 0) -1)
        (ivec.pop (&mut touched))
        unit))
    (let [i : i64 0]
      (begin
        (while (< i nbits)
          (begin
            (ivec.set (&mut vals)
              (+ 30770 (+ (* (/ i 10) 36) (% i 10)))
              (bit-and (shr x i) 1))
            (ivec.set (&mut vals)
              (+ 32066 (+ (* (/ i 10) 36) (% i 10)))
              (bit-and (shr y i) 1))
            (set! i (+ i 1))))))
    (evaluate vals ga gb gop go cons cons2 touched ngates)
    (let
      [z : i64 0]
      [i : i64 0]
      (begin
        (while (< i (+ nbits 1))
          (begin
            (let [zv : i64 (ivec.get (& vals) (+ 33362 (+ (* (/ i 10) 36) (% i 10))) 0)]
              (if (= zv 1)
                (set! z (bit-or z (shl 1 i)))
                unit))
            (set! i (+ i 1))))
        (= z exp)))))

;; apply the pairs' swaps to go, run all tests, restore, return pass?
(define (apply-swaps [pairs : (& pairs ivec.Vec)]
                 [go : (&mut go ivec.Vec)]
                 [producer : (&mut producer ivec.Vec)]) : unit
  (let [k : i64 0]
    (begin
      (while (< k (ivec.len (& pairs)))
        (begin
          (let
            [w1 : i64 (ivec.get (& pairs) k 0)]
            [w2 : i64 (ivec.get (& pairs) (+ k 1) 0)]
            [p1 : i64 (ivec.get (& producer) w1 0)]
            [p2 : i64 (ivec.get (& producer) w2 0)]
            (begin
              (ivec.set go p1 w2)
              (ivec.set go p2 w1)
              (ivec.set producer w1 p2)
              (ivec.set producer w2 p1)))
          (set! k (+ k 2))))
      unit)))

(define (restore-swaps [pairs : (& pairs ivec.Vec)]
                       [go : (&mut go ivec.Vec)]
                       [producer : (&mut producer ivec.Vec)]) : unit
  (let [k : i64 0]
    (begin
      (while (< k (ivec.len (& pairs)))
        (begin
          (let
            [w1 : i64 (ivec.get (& pairs) k 0)]
            [w2 : i64 (ivec.get (& pairs) (+ k 1) 0)]
            [p1 : i64 (ivec.get (& producer) w1 0)]
            [p2 : i64 (ivec.get (& producer) w2 0)]
            (begin
              (ivec.set go p1 w1)
              (ivec.set go p2 w2)
              (ivec.set producer w1 p2)
              (ivec.set producer w2 p1)))
          (set! k (+ k 2))))
      unit)))

;; run the full test battery; go is read-only here
(define (run-all-tests [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                       [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                       [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                       [vals : (&mut vals ivec.Vec)] [touched : (&mut touched ivec.Vec)]
                       [nbits : i64] [ngates : i64]) : bool
  (let [ok? : bool (run-test 0 0 0 ga gb gop go cons cons2 vals touched nbits ngates)]
    (begin
      (let [i : i64 0]
        (begin
          (while (and ok? (< i nbits))
            (begin
              (let [b : i64 (shl 1 i)]
                (begin
                  (if ok?
                    (set! ok? (run-test b 0 b ga gb gop go cons cons2 vals touched nbits ngates))
                    unit)
                  (if ok?
                    (set! ok? (run-test 0 b b ga gb gop go cons cons2 vals touched nbits ngates))
                    unit)
                  (if ok?
                    (set! ok? (run-test b b (shl b 1) ga gb gop go cons cons2 vals touched nbits ngates))
                    unit)))
              (set! i (+ i 1)))))
      (let [j : i64 0]
        (begin
          (while (and ok? (< j 12))
            (begin
              (let
                [tx : i64 (bit-and (* (+ j 1) 2654435761) 35184372088831)]
                [ty : i64 (bit-and (* (+ j 1) 40503) 35184372088831)]
                (if ok?
                  (set! ok? (run-test tx ty (bit-and (+ tx ty) 70368744177663)
                                      ga gb gop go cons cons2 vals touched nbits ngates))
                  unit))
              (set! j (+ j 1)))))
      ok?)))))

(define (verify [pairs : (& pairs ivec.Vec)]
                [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                [gop : (& gop ivec.Vec)] [go : (&mut go ivec.Vec)]
                [producer : (&mut producer ivec.Vec)]
                [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                [vals : (&mut vals ivec.Vec)] [touched : (&mut touched ivec.Vec)]
                [nbits : i64] [ngates : i64]) : bool
  (let [result : bool false]
    (begin
      (apply-swaps pairs go producer)
      (let [ok? : bool (run-all-tests ga gb gop go cons cons2 vals touched nbits ngates)]
        (set! result ok?))
      (restore-swaps pairs go producer)
      result)))

;; copy src into dst
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

;; search all pairings of the candidates; return true when a verified
;; configuration is found, with the pairs copied into answer
(define (search [s : i64] [sus : (& sus ivec.Vec)] [used : (&mut used ivec.Vec)]
                [pairs : (&mut pairs ivec.Vec)]
                [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                [gop : (& gop ivec.Vec)] [go : (&mut go ivec.Vec)]
                [producer : (&mut producer ivec.Vec)]
                [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                [vals : (&mut vals ivec.Vec)] [touched : (&mut touched ivec.Vec)]
                [nbits : i64] [ngates : i64]
                [answer : (&mut answer ivec.Vec)]) : bool
  (let [s2 : i64 s]
    (begin
      (while (and (< s2 (ivec.len (& sus))) (not (= (ivec.get (& used) s2 0) 0)))
        (set! s2 (+ s2 1)))
      (if (>= s2 (ivec.len (& sus)))
        (verify pairs ga gb gop go producer cons cons2 vals touched nbits ngates)
        (let
          [found? : bool false]
          [j : i64 (+ s2 1)]
          (begin
            ;; option 1: leave s2 unpaired (skip)
            (ivec.set used s2 2)
            (if (search (+ s2 1) sus used pairs ga gb gop go producer cons cons2
                        vals touched nbits ngates answer)
              (set! found? true)
              unit)
            (ivec.set used s2 0)
            ;; option 2: pair s2 with each unused j
            (while (and (not found?) (< j (ivec.len (& sus))))
              (begin
                (if (= (ivec.get (& used) j 0) 0)
                  (begin
                    (ivec.push pairs (ivec.get (& sus) s2 0))
                    (ivec.push pairs (ivec.get (& sus) j 0))
                    (ivec.set used s2 1)
                    (ivec.set used j 1)
                    (if (search (+ s2 1) sus used pairs ga gb gop go producer cons cons2
                                vals touched nbits ngates answer)
                      (begin
                        (set! found? true)
                        (copy-into answer pairs))
                      unit)
                    (ivec.set used s2 0)
                    (ivec.set used j 0)
                    (ivec.pop pairs)
                    (ivec.pop pairs)
                    unit)
                  unit)
                (set! j (+ j 1))))
            found?))))))

;; structural check: collect suspicious output wires
(define (find-suspects [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                       [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                       [cons : (& cons ivec.Vec)] [cons2 : (& cons2 ivec.Vec)]
                       [ngates : i64] [sus : (&mut sus ivec.Vec)]) : unit
  (let
    [g : i64 0]
    [c1 : i64 0]
    [c2 : i64 0]
    [c3 : i64 0]
    [c4 : i64 0]
    [c5 : i64 0]
    (begin
      (while (< g ngates)
        (begin
          (let
            [o : i64 (ivec.get (& go) g 0)]
            [op : i64 (ivec.get (& gop) g 0)]
            [zb : i64 (/ o 1296)]
            (begin
              (if (= zb 25)
                (let [bit : i64 (z-bit-of o)]
                  (begin
                    (if (or (and (= bit 45) (not (= op 1)))
                            (and (not (= bit 45)) (not (= op 2))))
                      (begin
                        (ivec.push (&mut sus) o)
                        (set! c1 (+ c1 1)))
                      unit)
                    ;; z-gate inputs: one XOR producer, one AND/OR producer
                    (if (and (>= bit 1) (<= bit 44) (= op 2))
                      (let
                        [pa : i64 (find-producer go (ivec.get (& ga) g 0) ngates)]
                        [pb : i64 (find-producer go (ivec.get (& gb) g 0) ngates)]
                        (if (or (< pa 0) (< pb 0)
                                (not (or (and (= (ivec.get (& gop) pa 0) 2)
                                              (or (= (ivec.get (& gop) pb 0) 0)
                                                  (= (ivec.get (& gop) pb 0) 1)))
                                         (and (= (ivec.get (& gop) pb 0) 2)
                                              (or (= (ivec.get (& gop) pa 0) 0)
                                                  (= (ivec.get (& gop) pa 0) 1))))))
                          (begin
                            (ivec.push (&mut sus) o)
                            (set! c2 (+ c2 1)))
                          unit))
                      unit)))
                unit)
              ;; XOR with non-z output (an hsum): consumers must be XOR or AND,
              ;; never OR
              (if (and (= op 2) (not (= zb 25)))
                (let
                  [c1 : i64 (ivec.get (& cons) o 0)]
                  [c2 : i64 (ivec.get (& cons2) o 0)]
                  (if (or (and (>= c1 0) (= (ivec.get (& gop) c1 0) 1))
                          (and (>= c2 0) (= (ivec.get (& gop) c2 0) 1)))
                    (begin
                      (ivec.push (&mut sus) o)
                      (set! c3 (+ c3 1)))
                    unit))
                unit)
              ;; AND: consumers must be OR, except the x00 AND y00 gate
              (if (and (= op 0) (not (= zb 25)))
                (let
                  [c1 : i64 (ivec.get (& cons) o 0)]
                  [c2 : i64 (ivec.get (& cons2) o 0)]
                  [special? : bool
                    (and (= (ivec.get (& ga) g 0) 30770)
                         (= (ivec.get (& gb) g 0) 32066))]
                  (if (or (and (>= c1 0) (not special?) (not (= (ivec.get (& gop) c1 0) 1)))
                          (and (>= c2 0) (not special?) (not (= (ivec.get (& gop) c2 0) 1))))
                    (begin
                      (ivec.push (&mut sus) o)
                      (set! c4 (+ c4 1)))
                    unit))
                unit)
              ;; OR: consumers must not be OR
              (if (and (= op 1) (not (= zb 25)))
                (let
                  [c1 : i64 (ivec.get (& cons) o 0)]
                  [c2 : i64 (ivec.get (& cons2) o 0)]
                  (if (or (and (>= c1 0) (= (ivec.get (& gop) c1 0) 1))
                          (and (>= c2 0) (= (ivec.get (& gop) c2 0) 1)))
                    (begin
                      (ivec.push (&mut sus) o)
                      (set! c5 (+ c5 1)))
                    unit))
                unit))
          (set! g (+ g 1)))))
      (io.print-string "suspects: ")
      (io.print-string (string.int->string (ivec.len (& sus))))
      (io.print-newline))
))

(define (x-wid [i : i64]) : i64
  (+ 30770 (+ (* (/ i 10) 36) (% i 10))))
(define (y-wid [i : i64]) : i64
  (+ 32066 (+ (* (/ i 10) 36) (% i 10))))

;; find gate index with op and inputs {a,b} (either order), or -1
(define (find-gate [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                   [gop : (& gop ivec.Vec)] [ngates : i64]
                   [a : i64] [b : i64] [op : i64]) : i64
  (let [g : i64 0] [r : i64 -1]
    (begin
      (while (and (< g ngates) (< r 0))
        (begin
          (if (and (= (ivec.get (& gop) g 0) op)
                   (or (and (= (ivec.get (& ga) g 0) a) (= (ivec.get (& gb) g 0) b))
                       (and (= (ivec.get (& ga) g 0) b) (= (ivec.get (& gb) g 0) a))))
            (set! r g)
            unit)
          (set! g (+ g 1))))
      r)))

;; output wire of the gate with op that consumes wire w, or -1
(define (find-consuming [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                        [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                        [ngates : i64] [w : i64] [op : i64]) : i64
  (let [g : i64 0] [r : i64 -1]
    (begin
      (while (and (< g ngates) (< r 0))
        (begin
          (if (and (= (ivec.get (& gop) g 0) op)
                   (or (= (ivec.get (& ga) g 0) w) (= (ivec.get (& gb) g 0) w)))
            (set! r g)
            unit)
          (set! g (+ g 1))))
      (if (>= r 0) (ivec.get (& go) r 0) -1))))

;; output wire of the h-gate for bit i (XOR of x_i, y_i), or -1
(define (find-h [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                [ngates : i64] [i : i64]) : i64
  (let [g : i64 (find-gate ga gb gop ngates (x-wid i) (y-wid i) 2)]
    (if (>= g 0) (ivec.get (& go) g 0) -1)))

;; output wire of the p-gate for bit i (AND of x_i, y_i), or -1
(define (find-p [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                [ngates : i64] [i : i64]) : i64
  (let [g : i64 (find-gate ga gb gop ngates (x-wid i) (y-wid i) 0)]
    (if (>= g 0) (ivec.get (& go) g 0) -1)))

;; Deterministic adder check: for each bit, verify the z gate against the
;; expected ripple-carry structure and collect the swapped wire pairs.
(define (adder-check [ga : (& ga ivec.Vec)] [gb : (& gb ivec.Vec)]
                     [gop : (& gop ivec.Vec)] [go : (& go ivec.Vec)]
                     [producer : (& producer ivec.Vec)]
                     [ngates : i64] [pairs : (&mut pairs ivec.Vec)]) : unit
  (let [bit : i64 0]
    (begin
      (while (< bit 46)
        (let
          [zid : i64 (+ 33362 (+ (* (/ bit 10) 36) (% bit 10)))]
          [gz : i64 (ivec.get (& producer) zid 0)]
          (begin
            (if (>= gz 0)
              (let
                [opz : i64 (ivec.get (& gop) gz 0)]
                (begin
                  (if (= bit 45)
                    (if (not (= opz 1))
                      (let
                        [P : i64 (find-p ga gb gop go ngates 44)]
                        (if (>= P 0)
                          (let [O : i64 (find-consuming ga gb gop go ngates P 1)]
                            (if (and (>= O 0) (not (= O zid)))
                              (begin
                                (ivec.push (&mut pairs) zid)
                                (ivec.push (&mut pairs) O))
                              unit))
                          unit))
                      unit)
                    (if (not (= opz 2))
                      (let [H : i64 (find-h ga gb gop go ngates bit)]
                        (if (>= H 0)
                          (let [O : i64 (find-consuming ga gb gop go ngates H 2)]
                            (if (and (>= O 0) (not (= O zid)))
                              (begin
                                (ivec.push (&mut pairs) zid)
                                (ivec.push (&mut pairs) O))
                              unit))
                          unit))
                      (let
                        [A : i64 (ivec.get (& ga) gz 0)]
                        [B : i64 (ivec.get (& gb) gz 0)]
                        [H : i64 (find-h ga gb gop go ngates bit)]
                        (if (and (>= bit 1) (>= H 0) (not (= A H)) (not (= B H)))
                          (let
                            [pa : i64 (ivec.get (& producer) A 0)]
                            [pb : i64 (ivec.get (& producer) B 0)]
                            [X : i64 -1]
                            (begin
                              (if (or (< pa 0) (not (= (ivec.get (& gop) pa 0) 1)))
                                (set! X A)
                                unit)
                              (if (and (< X 0) (or (< pb 0) (not (= (ivec.get (& gop) pb 0) 1))))
                                (set! X B)
                                unit)
                              (if (>= X 0)
                                (begin
                                  (ivec.push (&mut pairs) H)
                                  (ivec.push (&mut pairs) X))
                                unit)))
                          unit))))
                  unit))
              unit))
            (set! bit (+ bit 1)))))))

(define (solve-day24 [fpath : String]) : unit
  (let
    [text : String (u.read-input fpath)]
    [lines : strvec.Vec (u.split-lines text)]
    [nlines : i64 (strvec.len (& lines))]
    [vals : ivec.Vec (ivec.new)]
    [ga : ivec.Vec (ivec.new)]
    [gb : ivec.Vec (ivec.new)]
    [gop : ivec.Vec (ivec.new)]
    [go : ivec.Vec (ivec.new)]
    [cons : ivec.Vec (ivec.new)]
    [cons2 : ivec.Vec (ivec.new)]
    [i : i64 0]
    (begin
      (while (< i 46656)
        (begin
          (ivec.push (&mut vals) -1)
          (ivec.push (&mut cons) -1)
          (ivec.push (&mut cons2) -1)
          (set! i (+ i 1))))
      (set! i 0)
      (while (< i nlines)
        (let [line : String (strvec.get (& lines) i "")]
          (begin
            (if (> (string.string-length (& line)) 5)
              (if (= (string.string-ref (& line) 3) ':')
              (let
                [w : i64 (wid3 (name-ch (string.string-ref (& line) 0))
                               (name-ch (string.string-ref (& line) 1))
                               (name-ch (string.string-ref (& line) 2)))]
                (ivec.set (&mut vals) w
                  (- (cast (string.string-ref (& line) 5) : i64) 48)))
              (let
                [a : i64 (wid3 (name-ch (string.string-ref (& line) 0))
                               (name-ch (string.string-ref (& line) 1))
                               (name-ch (string.string-ref (& line) 2)))]
                [opc : char (string.string-ref (& line) 4)]
                [bpos : i64 (if (= (string.string-ref (& line) 6) ' ') 7 8)]
                [b : i64 (wid3 (name-ch (string.string-ref (& line) bpos))
                               (name-ch (string.string-ref (& line) (+ bpos 1)))
                               (name-ch (string.string-ref (& line) (+ bpos 2))))]
                [opos : i64 (+ bpos 7)]
                [o : i64 (wid3 (name-ch (string.string-ref (& line) opos))
                               (name-ch (string.string-ref (& line) (+ opos 1)))
                               (name-ch (string.string-ref (& line) (+ opos 2))))]
                (begin
                  (ivec.push (&mut ga) a)
                  (ivec.push (&mut gb) b)
                  (ivec.push (&mut gop)
                    (if (= opc 'A') 0 (if (= opc 'O') 1 2)))
                  (ivec.push (&mut go) o)
                  (let [g : i64 (- (ivec.len (& go)) 1)]
                    (begin
                      (if (< (ivec.get (& cons) a 0) 0)
                        (ivec.set (&mut cons) a g)
                        (ivec.set (&mut cons2) a g))
                      (if (< (ivec.get (& cons) b 0) 0)
                        (ivec.set (&mut cons) b g)
                        (ivec.set (&mut cons2) b g))
                      unit)))))
              unit)
            (set! i (+ i 1)))))
      (let
        [ngates : i64 (ivec.len (& go))]
        [touched : ivec.Vec (ivec.new)]
        [producer : ivec.Vec (ivec.new)]
        (begin
          (let [w : i64 0]
            (begin
              (while (< w 46656)
                (begin
                  (ivec.push (&mut producer) -1)
                  (set! w (+ w 1))))
              (let [g2 : i64 0]
                (begin
                  (while (< g2 ngates)
                    (begin
                      (ivec.set (&mut producer) (ivec.get (& go) g2 0) g2)
                      (set! g2 (+ g2 1))))
                  unit))))
          ;; part 1
          (evaluate (&mut vals) (& ga) (& gb) (& gop) (& go)
                   (& cons) (& cons2) (&mut touched) ngates)
          (let
            [z : i64 0]
            [k : i64 0]
            (begin
              (while (< k 46)
                (begin
                  (let [zv : i64 (ivec.get (& vals) (+ 33362 (+ (* (/ k 10) 36) (% k 10))) 0)]
                    (if (= zv 1)
                      (set! z (bit-or z (shl 1 k)))
                      unit))
                  (set! k (+ k 1))))
              (io.print-string "day24 part1: ")
              (io.print-string (string.int->string z))
              (io.print-newline))
            ;; part 2
            (let
              [pairs : ivec.Vec (ivec.new)]
              (begin
                (adder-check (& ga) (& gb) (& gop) (& go) (& producer)
                             ngates (&mut pairs))
                (if (verify (& pairs) (& ga) (& gb) (& gop) (&mut go) (&mut producer)
                            (& cons) (& cons2)
                            (&mut vals) (&mut touched) 45 ngates)
                  (begin
                    ;; sort pairs ascending and print names
                    (let [b : i64 0]
                      (begin
                        (while (< b (ivec.len (& pairs)))
                          (let [p : i64 (- b 1)]
                            (begin
                              (while (and (>= p 0)
                                          (< (ivec.get (& pairs) (+ p 1) 0)
                                             (ivec.get (& pairs) p 0)))
                                (begin
                                  (let [tmp : i64 (ivec.get (& pairs) p 0)]
                                    (begin
                                      (ivec.set (&mut pairs) p (ivec.get (& pairs) (+ p 1) 0))
                                      (ivec.set (&mut pairs) (+ p 1) tmp)))
                                  (set! p (- p 1))))
                              (set! b (+ b 1)))))))
                    (io.print-string "day24 part2: ")
                    (let [k2 : i64 0]
                      (begin
                        (while (< k2 (ivec.len (& pairs)))
                          (begin
                            (if (> k2 0)
                              (io.print-string ",")
                              unit)
                            (io.print-string (name-str (ivec.get (& pairs) k2 0)))
                            (set! k2 (+ k2 1))))
                        (io.print-newline))))
                  (io.print-string "day24 part2: NOT FOUND"
))))))))))(define (main) : i64
  (begin
    (solve-day24 "inputs/day24.txt")
    0))
