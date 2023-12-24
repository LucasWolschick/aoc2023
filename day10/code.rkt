#lang racket

(struct point (x y) #:transparent)
(struct cell (n? e? s? w?) #:transparent)
(struct maze (w h cells start) #:transparent)

(define mapping (hash
                 "|" (cell #t #f #t #f)
                 "-" (cell #f #t #f #t)
                 "L" (cell #t #t #f #f)
                 "J" (cell #t #f #f #t)
                 "7" (cell #f #f #t #t)
                 "F" (cell #f #t #t #f)
                 "." (cell #f #f #f #f)
                 "S" (cell #f #f #f #f)
                 ))

(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

(define (dim-maze maze-str)
  (define lines (split-lines maze-str))
  (point (string-length (first lines)) (length lines)))

(define (make-maze w h)
  (define cells (make-list h (make-list w (cell #f #f #f #f))))
  (maze w h cells (point 0 0)))

(define (set-cell m x y cell)
  (define cells (maze-cells m))
  (struct-copy maze m [cells (list-set cells y (list-set (list-ref cells y) x cell))]))

(define (get-cell m x y)
  (cond
    [(or (not (<= 0 x (- (maze-w m) 1)))
         (not (<= 0 y (- (maze-h m) 1))))
     (hash-ref mapping ".")]
    [else (list-ref (list-ref (maze-cells m) y) x)]))

(define (parse-maze maze-str)
  (define lines (split-lines maze-str))
  (define dim (dim-maze maze-str))
  (define (parse-line y m)
    (define (parse-column x m)
      (define c (substring (list-ref lines y) x (+ x 1) ))
      (define cell (hash-ref mapping c))
      (if (equal? c "S")
          (struct-copy maze (set-cell m x y cell) [start (point x y)])
          (set-cell m x y cell)))
    (define (parse-columns x m)
      (if (< x 0)
          m
          (parse-columns (- x 1) (parse-column x m))))
    (parse-columns (- (point-x dim) 1) m))
  (define (parse-lines y m)
    (if (< y 0)
        m
        (parse-lines (- y 1) (parse-line y m))))
  (define base-maze (parse-lines (- (point-y dim) 1) (make-maze (point-x dim) (point-y dim))))
  (define (fix-start base-maze)
    (define start (maze-start base-maze))
    (define sx (point-x start))
    (define sy (point-y start))
    (define n (cell-s? (get-cell base-maze sx (- sy 1))))
    (define e (cell-w? (get-cell base-maze (+ sx 1) sy)))
    (define s (cell-n? (get-cell base-maze sx (+ sy 1))))
    (define w (cell-e? (get-cell base-maze (- sx 1) sy)))
    (match (list n e s w)
      ['(#t #t #f #f) (set-cell base-maze sx sy (hash-ref mapping "L"))]
      ['(#f #t #t #f) (set-cell base-maze sx sy (hash-ref mapping "F"))]
      ['(#f #f #t #t) (set-cell base-maze sx sy (hash-ref mapping "7"))]
      ['(#t #f #f #t) (set-cell base-maze sx sy (hash-ref mapping "J"))]
      ['(#t #f #t #f) (set-cell base-maze sx sy (hash-ref mapping "|"))]
      ['(#f #t #f #t) (set-cell base-maze sx sy (hash-ref mapping "-"))]
      [_ base-maze]))
  (fix-start base-maze))

(define (graph n)
  (make-list n '()))

(define (graph-n g)
  (length g))

(define (add-edge g u v)
  (define (add-edge* g u v)
    (list-set g u (append (list-ref g u) (list v))))
  (add-edge* (add-edge* g u v) v u))

(define (neighbors g u)
  (list-ref g u))

(define (is-adjacent? g u v)
  (list? (member v (neighbors g u))))

(define (maze->graph m)
  (define (xy x y)
    (+ (* y (maze-w m)) x))
  
  (define (horiz g i j)
    (define left (get-cell m i j))
    (define right (get-cell m (+ i 1) j))
    (if (and (cell-e? left) (cell-w? right))
        (add-edge g (xy i j) (xy (+ i 1) j))
        g))
  (define (horizs-line g i j)
    (if (< i -1)
        g
        (horizs-line (horiz g i j) (- i 1) j)))
  (define (horizs-columns g i j)
    (if (< j -1)
        g
        (horizs-columns (horizs-line g i j) i (- j 1))))
  
  (define (vert g i j)
    (define up (get-cell m i j))
    (define down (get-cell m i (+ j 1)))
    (if (and (cell-s? up) (cell-n? down))
        (add-edge g (xy i j) (xy i (+ j 1)))
        g))
  (define (verts-line g i j)
    (if (< i -1)
        g
        (verts-line (vert g i j) (- i 1) j)))
  (define (verts-columns g i j)
    (if (< j -1)
        g
        (verts-columns (verts-line g i j) i (- j 1))))

  (define (all g i j)
    (verts-columns (horizs-columns g i j) i j))
  
  (define w (maze-w m))
  (define h (maze-h m))
  (define dim (* w h))
  
  (all (graph dim) (- w 1) (- h 1)))

(struct bfs-results (d pi visited) #:transparent)

(define (bfs-results-new n)
  (bfs-results
   (make-list n -inf.0)
   (make-list n null)
   (make-list n #f)))

(define (bfs G s)
  (define (visited? res v)
    (list-ref (bfs-results-visited res) v))
  (define (visit u v res queue)
    (define mvisited (bfs-results-visited res))
    (define md (bfs-results-d res))
    (define mpi (bfs-results-pi res))
    (if (not (visited? res v))
        (values
         (append queue (list v))
         (struct-copy bfs-results res
                      [visited (list-set mvisited v #t)]
                      [d (list-set md v (add1 (list-ref md u)))]
                      [pi (list-set mpi v u)]))
        (values queue res)))
  (define (visit-neighbors u res queue)
    (define nes (neighbors G u))
    (define (iter lst res queue)
      (cond
        [(empty? lst) (values res queue)]
        [else (let-values ([(q r) (visit u (first lst) res queue)])
                (iter (rest lst) r q))]))
    (iter nes res queue))
  (define (iter res queue)
    (cond
      [(empty? queue) res]
      [else
       (define u (first queue))
       (define q (rest queue))
       (let-values ([(r q) (visit-neighbors u res q)])
         (iter r q))]))
  (define base-res (bfs-results-new (graph-n G)))
  (define start-res (struct-copy bfs-results
                                 base-res
                                 [visited (list-set (bfs-results-visited base-res) s #t)]
                                 [d (list-set (bfs-results-d base-res) s 0)]))
  (iter start-res (list s)))
       
(define (bfs-on-maze maze)
  (define G (maze->graph maze))
  (define (xy x y)
    (+ (* y (maze-w maze)) x))
  (define s (maze-start maze))
  (define sxy (xy (point-x s) (point-y s)))
  (bfs G sxy))

(define (largest-distance maze)
  (define results (bfs-on-maze maze))
  (apply max (bfs-results-d results)))

(define (part1 file)
  (largest-distance (parse-maze (file->string file))))

; for part2, cast a ray from every line in the graph
; starting "outside", hitting a visited node turns it
; "inside", and starting "inside" turns it "outside"
; while "inside", any "." is considered an "inside tile".

(define (count-inside maze graph res)
  (define (xy x y)
    (+ (* y (maze-w maze)) x))
  
  (define gnd (hash-ref mapping "."))
  (define lne (hash-ref mapping "-"))
  (define jay (hash-ref mapping "J"))
  (define svn (hash-ref mapping "7"))
  (define eff (hash-ref mapping "F"))
  (define ell (hash-ref mapping "L"))
  (define pip (hash-ref mapping "|"))
  
  (define (iter-line line count)
    (define (iter col where count)
      ; inside can be 'inside, 'outside, 'border-from-J-out, 'border-from-7-out, 'border-from-J-in, 'border-from-7-in
      ;
      ; we are on the right edge of cell (col, line)
      (cond [(< col 0) count]
            [else
             (define is-boundary? (list-ref (bfs-results-visited res) (xy col line)))
             (define cell (get-cell maze col line))

             (define is-gnd? (equal? cell gnd))
             (define is-lne? (equal? cell lne))
             (define is-jay? (equal? cell jay))
             (define is-svn? (equal? cell svn))
             (define is-eff? (equal? cell eff))
             (define is-ell? (equal? cell ell))
             (define is-pip? (equal? cell pip))
             
             (define new-where
               (cond
                 [(not is-boundary?) where]
                 [(and (eq? where 'inside)
                       is-gnd?) 'inside]
                 ;[(and (eq? where 'inside)
                 ;      is-lne?) ERROR]
                 [(and (eq? where 'inside)
                       is-jay?) 'border-from-J-in]
                 [(and (eq? where 'inside)
                       is-svn?) 'border-from-7-in]
                 ;[(and (eq? where 'inside)
                 ;      is-eff?) 'outside]
                 ;[(and (eq? where 'inside)
                 ;      is-ell?) 'outside]
                 [(and (eq? where 'inside)
                       is-pip?) 'outside]

                 [(and (eq? where 'outside)
                       is-gnd?) 'outside]
                 ;[(and (eq? where 'outside)
                 ;      is-lne?) ERROR]
                 [(and (eq? where 'outside)
                       is-jay?) 'border-from-J-out]
                 [(and (eq? where 'outside)
                       is-svn?) 'border-from-7-out]
                 ;[(and (eq? where 'outside)
                 ;      is-eff?) 'inside]
                 ;[(and (eq? where 'outside)
                 ;      is-ell?) 'inside]
                 [(and (eq? where 'outside)
                       is-pip?) 'inside]

                 ;[(and (eq? where 'border-from-J-in)
                 ;      is-gnd?) 'border-from-J-in]
                 [(and (eq? where 'border-from-J-in)
                       is-lne?) 'border-from-J-in]
                 ;[(and (eq? where 'border-from-J-in)
                 ;      is-jay?) 'border-from-J-in]
                 ;[(and (eq? where 'border-from-J-in)
                 ;      is-svn?) 'border-from-J-in]
                 [(and (eq? where 'border-from-J-in)
                       is-eff?) 'outside]
                 [(and (eq? where 'border-from-J-in)
                       is-ell?) 'inside]
                 ;[(and (eq? where 'border-from-J-in)
                 ;      is-pip?) 'border-from-J-in]

                 ;[(and (eq? where 'border-from-J-out)
                 ;      is-gnd?) 'border-from-J-out]
                 [(and (eq? where 'border-from-J-out)
                       is-lne?) 'border-from-J-out]
                 ;[(and (eq? where 'border-from-J-out)
                 ;      is-jay?) 'border-from-J-out]
                 ;[(and (eq? where 'border-from-J-out)
                 ;      is-svn?) 'border-from-J-out]
                 [(and (eq? where 'border-from-J-out)
                       is-eff?) 'inside]
                 [(and (eq? where 'border-from-J-out)
                       is-ell?) 'outside]
                 ;[(and (eq? where 'border-from-J-out)
                 ;      is-pip?) 'border-from-J-out]

                 ;[(and (eq? where 'border-from-7-in)
                 ;      is-gnd?) 'border-from-7-in]
                 [(and (eq? where 'border-from-7-in)
                       is-lne?) 'border-from-7-in]
                 ;[(and (eq? where 'border-from-7-in)
                 ;      is-jay?) 'border-from-7-in]
                 ;[(and (eq? where 'border-from-7-in)
                 ;      is-svn?) 'border-from-7-in]
                 [(and (eq? where 'border-from-7-in)
                       is-eff?) 'inside]
                 [(and (eq? where 'border-from-7-in)
                       is-ell?) 'outside]
                 ;[(and (eq? where 'border-from-7-in)
                 ;      is-pip?) 'border-from-7-in]

                 ;[(and (eq? where 'border-from-7-out)
                 ;      is-gnd?) 'border-from-7-out]
                 [(and (eq? where 'border-from-7-out)
                       is-lne?) 'border-from-7-out]
                 ;[(and (eq? where 'border-from-7-out)
                 ;      is-jay?) 'border-from-7-out]
                 ;[(and (eq? where 'border-from-7-out)
                 ;      is-svn?) 'border-from-7-out]
                 [(and (eq? where 'border-from-7-out)
                       is-eff?) 'outside]
                 [(and (eq? where 'border-from-7-out)
                       is-ell?) 'inside]
                 ;[(and (eq? where 'border-from-7-out)
                 ;      is-pip?) 'border-from-7-out]

                 [else (raise "wtf")]))
             ; check new-where makes sense
             ;(println (list where new-where col line))
             (iter (sub1 col) new-where (if (and (eq? where 'inside) (not is-boundary?))
                                             (add1 count)
                                             count))]))
    (iter (sub1 (maze-w maze)) 'outside count))
  (define (iter-lines lines count)
    (if (< lines 0)
        count
        (iter-lines (sub1 lines) (iter-line lines count))))
  (iter-lines (sub1 (maze-h maze)) 0))

(define (part2 file)
  (define maze (parse-maze (file->string file)))
  (define graph (maze->graph maze))
  (define results (bfs-on-maze maze))
  (count-inside maze graph results))