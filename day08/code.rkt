#lang racket

(struct mappy (sequence nodes) #:transparent)

(define (string->mappy str)
  (define lines (string-split str #rx"[\newline\return]+"))
  (define sequence (string->list (first lines)))
  (define node-strs (rest lines))
  (define (parse-decl str)
    (define halves (string-split str " = "))
    (define name (first halves))
    (define choices-str (second halves))
    (define choices (string-split (substring choices-str 1 (sub1 (string-length choices-str))) ", "))
    (cons name (cons (first choices) (second choices))))
  (define (build-nodes nodes)
    (cond
      [(empty? nodes) (hash)]
      [else
       (define node (first nodes))
       (hash-set (build-nodes (rest nodes)) (car node) (cdr node))]))
  (define nodes (build-nodes (map parse-decl node-strs)))
  (mappy sequence nodes))

(define (traverse mappy)
  (define seq (mappy-sequence mappy))
  (define nodes (mappy-nodes mappy))
  (define (walk position seq-i steps)
    (if (equal? position "ZZZ")
        steps
        (walk
         ;new-position
         ((if (equal? (list-ref seq seq-i) #\L) car cdr) (hash-ref nodes position))
         ;new-seq-i
         (remainder (add1 seq-i) (length seq))
         ;new-steps
         (add1 steps))))
  (walk "AAA" 0 0))

(define (part1 file)
  (traverse (string->mappy (file->string file))))

;(define (walk positions seq-i steps)
;    (define (map-position pos)
;      ((if (equal? (list-ref seq seq-i) #\L) car cdr) (hash-ref nodes pos)))
;    (if (andmap (λ (s) (string-suffix? s "Z")) (set->list positions))
;        steps
;        (walk
;         ;new-positions
;         (list->set (set-map positions map-position))
;         ;new-seq-i
;         (remainder (add1 seq-i) (length seq))
;         ;new-steps
;         (add1 steps))))

(define (ghost-traverse mappy)
  (define seq (mappy-sequence mappy))
  (define nodes (mappy-nodes mappy)) 
  (define (walk position seq-i steps)
    (if (string-suffix? position "Z")
        steps
        (walk
         ;new-position
         ((if (equal? (list-ref seq seq-i) #\L) car cdr) (hash-ref nodes position))
         ;new-seq-i
         (remainder (add1 seq-i) (length seq))
         ;new-steps
         (add1 steps)))) 
  (define start-positions (filter (λ (s) (string-suffix? s "A")) (hash-keys nodes)))
  (apply lcm (map (λ (p) (walk p 0 0)) start-positions)))

(define (part2 file)
  (ghost-traverse (string->mappy (file->string file))))