#lang racket

(struct inst (dir len color) #:transparent)
; dir : 'u 'd 'l 'r

(struct vec (x y) #:transparent)
; x, y : number

; function which parses an instruction
(define (parse-instruction inst-str)
  (define parts (string-split inst-str))
  (inst (match (first parts) ("U" 'u) ("D" 'd) ("L" 'l) ("R" 'r))
        (string->number (second parts))
        (third parts)))

; function which parses a list of instructions
(define (parse-instruction-list insts-lines)
  (map parse-instruction insts-lines))

; function which converts an instruction into a delta
(define (inst->vec i)
  (define l (inst-len i))
  (match (inst-dir i)
    ('u (vec 0 (- l)))
    ('d (vec 0 l))
    ('l (vec (- l) 0))
    ('r (vec l 0 ))))

; sum of vectors
(define (vec+ a b)
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))

; difference of vectors
(define (vec- a b)
  (vec (- (vec-x a) (vec-x b))
       (- (vec-y a) (vec-y b))))

; product of vector by scalar
(define (vec* a k)
  (vec (* k (vec-x a))
       (* k (vec-y a))))

; cross product of vectors
(define (vec-cross a b)
  (- (* (vec-x a) (vec-y b))
     (* (vec-y a) (vec-x b))))

; length of a vector
(define (vec-length v)
  (sqrt (+ (sqr (vec-x v)) (sqr (vec-y v)))))

; unit vector
(define (vec-unit v)
  (vec* v (/ 1 (vec-length v))))

; sign
(define (sign r)
  (cond [(> r 0) 1]
        [(< r 0) -1]
        [(= r 0) 0]))

; function which builds a polygon from a list of deltas
(define (polygon deltas pos)
  (define (iter pos deltas points)
    (if (empty? deltas)
        points
        (iter (vec+ pos (first deltas)) (rest deltas) (cons (vec+ pos (first deltas)) points))))
  (iter pos deltas (list (vec 0 0))))

; dilates this problem's specific polygon class by d on its edges...
; you oughta see my sketches for this haha
(define (dilate-polygon poly d)
  (define n (length poly))
  (define poly* (append (list (list-ref poly (- n 2))) poly (list (list-ref poly 1))))
  (define (dilate-vertex prv vtx nxt)
    (define u (vec-unit (vec- vtx prv)))
    (define v (vec-unit (vec- nxt vtx)))
    (define s (sign (vec-cross u v)))
    (define delta (vec* (vec- u v) s))
    (vec+ vtx (vec* delta d)))
  (define (dilate-polygon prv lst)
    (cond [(= (length lst) 1) empty]
          [else (cons (dilate-vertex prv (first lst) (first (rest lst))) (dilate-polygon (first lst) (rest lst)))]))
  (dilate-polygon (first poly*) (rest poly*)))

; function which calculates a simple polygon's area
(define (polygon-area poly)
  (define x (map vec-x poly))
  (define y (map vec-y poly))
  (define n (length poly))
  (abs (/ (- (apply + (map * (take x (- n 1)) (drop y 1)))
        (apply + (map * (take y (- n 1)) (drop x 1))))
     2)))

; part 1
(define (part1 file)
  (define poly (polygon (map inst->vec (parse-instruction-list (file->lines file))) (vec 0 0)))
  (polygon-area (dilate-polygon poly -1/2)))

; part 2
(define (parse-instruction-2 inst-str)
  (define hex (last (string-split inst-str)))
  (define distance (string->number (substring hex 2 7) 16))
  (define dir (list-ref '(r d l u) (string->number (substring hex 7 8))))
  (inst dir distance ""))

(define (parse-instruction-list-2 insts-lines)
  (map parse-instruction-2 insts-lines))

(define (part2 file)
  (define poly (polygon (map inst->vec (parse-instruction-list-2 (file->lines file))) (vec 0 0)))
  (polygon-area (dilate-polygon poly -1/2)))
