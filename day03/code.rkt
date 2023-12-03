#lang racket

; first, parse the schematic to a grid representation
; then, mark all tiles neighboring any symbol
; then, find all numbers overlapping these zones.

(struct rect (x y w h) #:transparent)

(define (rect-intersects ar br)
  (let ([ar.x (rect-x ar)]
        [ar.y (rect-y ar)]
        [ar.w (rect-w ar)]
        [ar.h (rect-h ar)]
        [br.x (rect-x br)]
        [br.y (rect-y br)]
        [br.w (rect-w br)]
        [br.h (rect-h br)])
    (not (or
          (or (> ar.x (+ br.x br.w))
              (< (+ ar.x ar.w) br.x))
          (or (> ar.y (+ br.y br.h))
              (< (+ ar.y ar.h) br.y))))))
; NOT (
;      ( ar.x > br.x + br.w OR ar.x + ar.w < br.x )
;      OR
;      ( ar.y > br.y + br.h OR ar.y + ar.h < br.y )
; )

(define (rect-expand r radius)
  (rect (- (rect-x r) radius)
        (- (rect-y r) radius)
        (+ (rect-w r) (* 2 radius))
        (+ (rect-h r) (* 2 radius))))

(struct token (type rect value) #:transparent)

(define (parse str)
  (define (scan-number str start i line line-i tokens)
    (define ch (if (< i (string-length str))
                   (string-ref str i)
                   #f))
    (if (and ch (char-numeric? ch))
        (scan-number str start (add1 i) line (add1 line-i) tokens)
        (scan str i line line-i (cons
                                 (token 'number
                                        (rect (+ start line-i (- i)) line (- i start) 1)
                                        (string->number (substring str start i)))
                                 tokens))))
  (define (scan str i line line-i tokens)
    (define ch (if (< i (string-length str))
                   (string-ref str i)
                   #f))
    (cond
      [(not ch) (reverse tokens)]
      [(char-numeric? ch) (scan-number str i (add1 i) line (add1 line-i) tokens)]
      [(equal? ch #\newline) (scan str (add1 i) (+ line 1) 1 tokens)]
      [(equal? ch #\.) (scan str (add1 i) line (add1 line-i) tokens)]
      [(equal? ch #\return) (scan str (add1 i) line (add1 line-i) tokens)]
      [else (scan str (add1 i) line (add1 line-i)
                  (cons
                   (token 'symbol
                          (rect line-i line 1 1)
                          (string-ref str i))
                   tokens))]))
  (scan str 0 1 1 empty))

(define (split-symbols-numbers tokens)
  (define (iter l-num l-sym tokens)
    (define tok (and (not (empty? tokens)) (first tokens)))
    (cond
      [(empty? tokens) (list l-num l-sym)]
      [(eq? (token-type tok) 'number) (iter (cons tok l-num) l-sym (rest tokens))]
      [(eq? (token-type tok) 'symbol) (iter l-num (cons tok l-sym) (rest tokens))]
      [else (iter l-num l-sym (rest tokens))]))
  (iter empty empty tokens))

(define (list-adjacent-numbers numbers symbols)
  (define (iter i-num i-sym adj)
    (cond [(empty? symbols) adj]
          [(empty? numbers) adj]
          ; reached last symbol yet no intersection: skip
          [(= i-sym (length symbols)) (iter (add1 i-num) 0 adj)]
          ; reached last number return adj
          [(= i-num (length numbers)) adj]
          ; otherwise...
          [else (if (rect-intersects (token-rect (list-ref numbers i-num)) (token-rect (list-ref symbols i-sym)))
                    (iter (add1 i-num) 0 (cons (list-ref numbers i-num) adj))
                    (iter i-num (add1 i-sym) adj))]))
  (iter 0 0 empty))

(define (part-1 file)
  (define str (file->string file))
  (foldl
   (lambda (tok accum) (+ accum (token-value tok)))
   0
   (apply list-adjacent-numbers (split-symbols-numbers (parse str)))))

(define (filter-gears symbols)
  (filter (lambda (t) (equal? (token-value t) #\*)) symbols))

(define (list-gear-adjacencies gears numbers)
  (define (iter i-gear i-number adj list)
    (cond [(or (empty? gears) (empty? numbers)) list]
          [(eq? i-gear (length gears)) list]
          [(eq? i-number (length numbers)) (iter (add1 i-gear) 0 empty (cons adj list))]
          [else (if (rect-intersects (token-rect (list-ref numbers i-number)) (token-rect (list-ref gears i-gear)))
                    (iter i-gear (add1 i-number) (cons (list-ref numbers i-number) adj) list)
                    (iter i-gear (add1 i-number) adj list))]))
  (iter 0 0 empty empty))

(define (part-2 file)
  (define str (file->string file))
  (let* ([ret (split-symbols-numbers (parse str))]
         [gears (filter-gears (second ret))]
         [numbers (first ret)]
         [adjacencies (filter (lambda (a) (= (length a) 2)) (list-gear-adjacencies gears numbers))])
    (apply +
           (map
            (lambda (inner-list) (apply *
                                        (map token-value inner-list)))
            adjacencies))))