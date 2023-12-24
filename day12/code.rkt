#lang racket

; type which describes cell state (. # ?)
; just a char

; type for row
; list of chars

; type for defect spans
; list of integers

; function which parses a line into a row and defect spans
(define (parse-line str)
  (define halves (string-split str))
  (define states (string->list (first halves)))
  (define spans (map string->number (string-split (second halves) ",")))
  (list states spans))

; function which takes a filled row and returns its defect spans
(define (defect-spans row)
  (define (spanning start i lst)
    (cond
      [(< i 0) (cons (- start i) lst)]
      [else
       (define c (list-ref row i))
       (cond [(equal? c #\#) (spanning start (sub1 i) lst)]
             [(or (equal? c #\.)
                  (equal? c #\?)) (wooing (sub1 i) (cons (- start i) lst))])]))
  (define (wooing i lst)
    (cond
      [(< i 0) lst]
      [else
       (define c (list-ref row i))
       (cond [(equal? c #\#) (spanning i (sub1 i) lst)]
             [(or (equal? c #\.)
                  (equal? c #\?)) (wooing (sub1 i) lst)])]))
  (define fc (last row))
  (define len (length row))
  (if (equal? fc #\#)
      (spanning (sub1 len) (sub1 len) empty)
      (wooing (sub1 len) empty)))

; function which tests a filled row against a series of spans
(define (check-spans row spans)
  (equal? (defect-spans row) spans))

; function which takes a row and returns its unknown cells
(define (find-unknowns row)
  (indexes-of row #\?))

; function which exhaustively tests all possibilities and counts valid 1s
(define (count-valid-fillings row spans)
  (define (prune? row)
    ; prune condition: any number in spans > biggest in desired spans
    (> (count (λ (x) (equal? x #\#)) row) (apply + spans)))
  (define (inner row unknowns)
    (cond [(empty? unknowns) (if (check-spans row spans) 1 0)]
          [else (if (prune? row)
                    0
                    (+ (inner (list-set row (first unknowns) #\.) (rest unknowns))
                       (inner (list-set row (first unknowns) #\#) (rest unknowns))))]))
  (inner row (find-unknowns row)))

; function which does that for all rows
(define (total-possibilities rows)
  (apply + (map (λ (p) (count-valid-fillings (first p) (second p))) rows)))

; function which solves part 1
(define (part1 file)
  (total-possibilities (map parse-line (file->lines file))))

; function which repeats row info 5 times
(define (unfold-row roww)
  (match-define (list row spans) roww)
  (define ? (list #\?))
  (list (append row ? row ? row ? row ? row)
        (append spans spans spans spans spans)))

; function which solves part 2
;(define (part2 file)
;  (total-possibilities (map (λ (l) (unfold-row (parse-line l))) (file->lines file))))


; an alternative: test all possible configurations of the given spans
; and see if they match the mask.
; immediately PRUNE if:
; - current placement invalidates mask
; - not enough length to complete the placements

(define (fits-mask cells mask)
  (cond [(not (= (length cells) (length mask))) #f]
        [(empty? cells) #t]
        [(equal? (first mask) #\?) (fits-mask (rest cells) (rest mask))]
        [(equal? (first mask) (first cells)) (fits-mask (rest cells) (rest mask))]
        [else #f]))

(define (permutations spans mask)
  (define (iter cells spans cells-i)
    (define space-left (- (length cells) cells-i))
    (define space-needed (+ (apply + spans) (length spans) -1))
    (cond 
      [(empty? spans) (when (fits-mask cells mask) (println (list->string cells)))]
      [(> space-needed space-left) #f]
      [else
       (define span (first spans))
       (for ([i (range (- space-left space-needed -1))])
         (define start (+ cells-i i))
         (define spliced-cells (append (take cells start) (make-list span #\#) (drop cells (+ start span))))
         (iter spliced-cells (rest spans) (+ start span 1)))]))
  (iter (make-list (length mask) #\.) spans 0))

; another alternative (kudos to reddit)
; recurse on the mask and the spans. cond on the first character.

(define (recurse mask spans)
  (define (count-length sym mask)
    (cond [(empty? mask) 0]
          [(not (equal? (first mask) sym)) 0]
          [else (+ 1 (count-length sym (rest mask)))]))
  (cond [(and (empty? mask) (empty? spans)) 1]
        [(empty? mask) 0]
        [(empty? spans) (if (equal? (first mask) #\#)
                            0
                            (recurse (rest mask) spans))]
        [(equal? (first mask) #\.) (recurse (rest mask) spans)]
        [(equal? (count-length #\# mask) (first spans))
         (cond [(and (> (length mask) (first spans))
                     (equal? (first (drop mask (first spans))) #\?))
                (recurse (drop mask (+ (first spans) 1)) (rest spans))]
               [else (recurse (drop mask (first spans)) (rest spans))])]
        [(equal? (first mask) #\?) (+ (recurse (cons #\. (rest mask)) spans)
                                      (recurse (cons #\# (rest mask)) spans))]
        [else (define hashes (count-length #\# mask))
              (define rest-mask (drop mask hashes))
              (cond
                [(empty? rest-mask) 0] ; too few hashes
                [(> hashes (first spans)) 0] ; too many hashes
                [(equal? (first rest-mask) #\.) 0] ; too few hashes
                [(equal? (first rest-mask) #\?) (+ (recurse (list-set mask hashes #\#) spans)
                                                   (recurse (list-set mask hashes #\.) spans))])]))


; cant believe this monstrosity works
; https://stackoverflow.com/a/66304604
(define (memo f)
  (let ((lookup (make-hash)))
    (lambda (x y)
      (unless (hash-has-key? lookup (cons x y))
        (hash-set! lookup (cons x y) (f x y)))
      (hash-ref lookup (cons x y)))))
(set! recurse (memo recurse))

(define (total-possibilities* rows)
  (apply + (map (λ (p) (apply recurse p)) rows)))

(define (part2 file)
  (total-possibilities* (map (λ (l) (unfold-row (parse-line l))) (file->lines file))))