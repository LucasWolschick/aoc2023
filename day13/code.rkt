#lang racket

; procedure which checks if a list is reflected across gap 
(define (reflected? lst n)
  (define len (length lst))
  (define mid (/ len 2))
  (cond
    [(or (= len 0) (= n 0) (= n len)) #t]
    [(> n mid) (reflected? (rest lst) (sub1 n))]
    [(< n mid) (reflected? (drop-right lst 1) n)]
    [else
     (define f (first lst))
     (define l (last lst))
     (if (equal? f l)
         (reflected? (rest (drop-right lst 1)) (sub1 n))
         #f)]))

; procedure which returns all axes of reflection of a given list
(define (reflect-axes lst)
  (define (iter n)
    (cond [(= n 0) empty]
          [(reflected? lst n) (cons n (iter (sub1 n)))]
          [else (iter (sub1 n))]))
  (iter (sub1 (length lst))))

; procedure which takes a list of lists and finds common axes
; of reflection
(define (common-axes lsts axes)
  (cond [(empty? lsts) (list->set (range axes))]
        [(set-intersect (list->set (reflect-axes (first lsts)))
                        (common-axes (rest lsts) axes))]))

; procedure which transposes a list of lists
(define (transpose lsts)
  (define M (sub1 (length lsts)))
  (define N (sub1 (length (first lsts))))
  (define (ij lst i j)
    (list-ref (list-ref lst i) j))
  (define (iter i j new-lsts)
    (cond [(< j 0) new-lsts]
          [(< i 0) (iter M (sub1 j) new-lsts)]
          [else (iter (sub1 i) j
                      (list-set new-lsts j (cons (ij lsts i j) (list-ref new-lsts j))))]))
  (iter M N (make-list (add1 N) empty)))

(define test
  '(
    (1 2 1 1 2 2 1 1 2)
    (2 2 1 2 1 1 2 1 2)
    (1 1 2 2 2 2 2 2 1)
    (1 1 2 2 2 2 2 2 1)
    (2 2 1 2 1 1 2 1 2)
    (2 2 1 1 2 2 1 1 2)
    (1 2 1 2 1 1 2 1 2)))

;(common-axes test (length (first test)))
;(common-axes (transpose test) (length (first (transpose test))))

; procedure which splits a text on newlines
(define (lines str)
  (string-split str #rx"[\newline\return]+"))

; procedure which takes a text description of a grid and makes it into a list of lists
(define (grid->matrix grid-str)
  (map string->list (lines grid-str)))

; procedure which splits a sequence of text grids into a list of grids
(define (grids->list all-grids-str)
  (map grid->matrix (string-split all-grids-str "\n\n")))

(define (all-grid-axes grid)
  (define grid-t (transpose grid))
  (define v-axes (set->list(common-axes grid (length (first grid)))))
  (define h-axes (set->list(common-axes grid-t (length (first grid-t)))))
  (define axes (append v-axes (map (λ (x) (* x 100)) h-axes)))
  axes)

; part 1
(define (part1 file)
  (define grids (grids->list (file->string file #:mode 'text)))
  (define axes (flatten (map all-grid-axes grids)))
  (apply + axes))

; procedure which tries to find a smudge which changes the reflection axis and returns the axis
(define (smudge grid)
  (define orig-axes (all-grid-axes grid))
  (define M (sub1 (length grid)))
  (define N (sub1 (length (first grid))))
  (define (grid-toggle grid i j a b)
    (define elem (list-ref (list-ref grid i) j))
    (if (equal? elem a)
        (list-set grid i (list-set (list-ref grid i) j b))
        (list-set grid i (list-set (list-ref grid i) j a))))
  (define (iter i j)
    (cond [(< i 0) orig-axes]
          [(< j 0) (iter (sub1 i) N)]
          [else
           ; try flipping...
           (define grid* (grid-toggle grid i j #\. #\#))
           (define new-axes (all-grid-axes grid*))
           (define difference (set-subtract new-axes orig-axes))
           ;(println (list orig-axes new-axes difference))
           (if (and (not (empty? difference)) (not (equal? difference orig-axes)))
               (first difference)
               (iter i (sub1 j)))]))
  (iter M N))

; part 2
(define (part2 file)
  (define grids (grids->list (file->string file #:mode 'text)))
  (define smudged-grids (map smudge grids))
  (apply + smudged-grids))