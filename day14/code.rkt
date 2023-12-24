#lang racket

; this one would be so simple in an imperative language :sob: :sob:

; procedure which splits a text into lines
(define (lines str)
  (string-split str #rx"[\newline\return]+"))

; procedure which parses a grid text into an actual grid
(define (parse-grid grid-str)
  (map string->list (lines grid-str)))

(define (get-grid grid x y)
  (list-ref (list-ref grid y) x))

(define (set-grid grid x y v)
  (list-set grid y (list-set (list-ref grid y) x v)))

; procedure which given a rock and a grid moves the rock north
; as much as possible and returns the new grid
;(define (move-rock-upwards rx ry grid)
;  (define grid1 (set-grid grid rx ry #\.))
;  (define (iter y)
;    (cond [(= y 0) (set-grid grid1 rx y #\O)]
;          [(not (equal? (get-grid grid1 rx (sub1 y)) #\.)) (set-grid grid1 rx y #\O)]
;          [else (iter (sub1 y))]))
;  (iter ry))

(define (move-rock rx ry dx dy grid)
  (define w (length (first grid)))
  (define h (length grid))
  (define grid1 (set-grid grid rx ry #\.))
  (define (iter px py)
    (define tx (+ px dx))
    (define ty (+ py dy))
    (cond [(not (and (<= 0 tx (sub1 w))
                     (<= 0 ty (sub1 h))))
           (set-grid grid1 px py #\O)]
          [else
           (define tile (get-grid grid1 tx ty))
           (if (equal? tile #\.)
               (iter tx ty)
               (set-grid grid1 px py #\O))]))
  (iter rx ry))

(define (move-rock-eastwards rx ry grid)
  (move-rock rx ry 1 0 grid))

(define (move-rock-westwards rx ry grid)
  (move-rock rx ry -1 0 grid))

; procedure which tilts the entire grid up or down
(define (tilt-grid-vert grid ygrav)
  (define (do-row grid y)
    (define (do-col grid x)
      (if (equal? (get-grid grid x y) #\O)
          (move-rock x y 0 ygrav grid)
          grid))
    (define (do-cols grid x)
      (if (< x 0)
          grid
          (do-cols (do-col grid x) (sub1 x))))
    (do-cols grid (sub1 (length (first grid)))))
  (cond
    [(< ygrav 0)
     (define (do-rows grid y)
       (if (>= y (length grid))
           grid
           (do-rows (do-row grid y) (add1 y))))
     (do-rows grid 0)]
    [(> ygrav 0)
     (define (do-rows grid y)
       (if (< y 0)
           grid
           (do-rows (do-row grid y) (sub1 y))))
     (do-rows grid (sub1 (length grid)))]))

(define (tilt-grid-up grid)
  (tilt-grid-vert grid -1))

(define (tilt-grid-down grid)
  (tilt-grid-vert grid 1))

; procedure which tilts grid left or right
(define (tilt-grid-horiz grid xgrav)
  (define (do-col grid x)
    (define (do-row grid y)
      (if (equal? (get-grid grid x y) #\O)
          (move-rock x y xgrav 0 grid)
          grid))
    (define (do-rows grid y)
      (if (< y 0)
          grid
          (do-rows (do-row grid y) (sub1 y))))
    (do-rows grid (sub1 (length grid))))
  (cond
    [(< xgrav 0)
     (define (do-cols grid x)
       (if (>= x (length (first grid)))
           grid
           (do-cols (do-col grid x) (add1 x))))
     (do-cols grid 0)]
    [(> xgrav 0)
     (define (do-cols grid x)
       (if (< x 0)
           grid
           (do-cols (do-col grid x) (sub1 x))))
     (do-cols grid (sub1 (length (first grid))))]))

(define (tilt-grid-left grid)
  (tilt-grid-horiz grid -1))

(define (tilt-grid-right grid)
  (tilt-grid-horiz grid 1))

; procedure which turns a grid back into text
(define (grid->string grid)
  (string-join (map list->string grid) "\n"))

; procedure which calculates the load
(define (grid-load grid)
  (define height (length grid))
  (define (row-load y)
    (* (count (λ (x) (equal? x #\O)) (list-ref grid y))
       (- height y)))
  (apply + (map row-load (range height))))

; part 1
(define (part1 file)
  (define grid (parse-grid (file->string file #:mode 'text)))
  (grid-load (tilt-grid-up grid)))

; part 2
; oH GOD
(define (cycle grid)
  (tilt-grid-right
   (tilt-grid-down
    (tilt-grid-left
     (tilt-grid-up grid)))))

(define (cycle-until-loop grid)
  (define prev (make-hash (list (cons (grid->string grid) 0))))
  (define (iter i p-grid)
    (define n-grid (cycle p-grid))
    (define n-grid-s (grid->string n-grid))
    (if (hash-has-key? prev n-grid-s)
        (cons (hash-ref prev n-grid-s) i)
        (begin
          (hash-set! prev n-grid-s (add1 i))
          (iter (add1 i) n-grid))))
  (iter 0 grid))

(define (cycle-for grid i)
  (if (= i 0)
      grid
      (cycle-for (cycle grid) (sub1 i))))

(define (part2 file)
  (define iters 1000000000)
  (define grid (parse-grid (file->string file #:mode 'text)))
  (define cycle-info (cycle-until-loop grid))
  (define cycle-length (- (cdr cycle-info) (car cycle-info) -1))
  (define rem (remainder (- iters (car cycle-info)) cycle-length))
  (define total-cycles (+ rem (car cycle-info)))
  (define end-grid (cycle-for grid total-cycles))
  (grid-load end-grid))