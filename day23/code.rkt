#lang racket

(require "../vec2.rkt")

(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

(define (parse-maze maze-str)
  (define lines (split-lines maze-str))
  (map string->list lines))

(define (maze-get maze pos)
  (list-ref (list-ref maze (vec2-y pos)) (vec2-x pos)))

(define (maze-set maze pos val)
  (list-set maze (vec2-y pos) (list-set (list-ref maze (vec2-y pos)) (vec2-x pos) val)))

(define (maze-bounds maze)
  (vec2 (length (first maze)) (length maze)))

(define (maze-bounded? maze pos)
  (define bounds (maze-bounds maze))
  (and (<= 0 (vec2-x pos) (sub1 (vec2-x bounds)))
       (<= 0 (vec2-y pos) (sub1 (vec2-y bounds)))))

(define (maze-kill-slopes maze)
  (map (λ (line) (map (λ (cell) (if (or (equal? cell #\>)
                                        (equal? cell #\v)
                                        (equal? cell #\<)
                                        (equal? cell #\^)) #\.
                                                           cell)) line)) maze))

(define (maze-neighbors maze pos)
  (define tile (maze-get maze pos))
  (define candidates (match tile
    [#\# empty]
    [#\> (list (vec2+ pos (vec2 1 0)))]
    [#\< (list (vec2+ pos (vec2 -1 0)))]
    [#\^ (list (vec2+ pos (vec2 0 -1)))]
    [#\v (list (vec2+ pos (vec2 0 1)))]
    [#\. (list (vec2+ pos (vec2 1 0))
               (vec2+ pos (vec2 0 1))
               (vec2+ pos (vec2 -1 0))
               (vec2+ pos (vec2 0 -1)))]))
  (define (okay? pos)
    (and (maze-bounded? maze pos)
         (not (equal? #\# (maze-get maze pos)))))
  (filter okay? candidates))

; returns the largest number of steps which can be
; taken when traveling in maze from pos to tgt, not visiting
; a same tile twice.
(define (hike maze pos tgt (neighbor-func maze-neighbors))
  (define (iter pos visited)
    (cond [(not (maze-bounded? maze pos)) (cons -inf.0 visited)]
          [(equal? pos tgt) (cons 0 visited)]
          [else
           (define neighbors (neighbor-func maze pos))
           (define valid-neighbors (filter (curry (negate set-member?) visited) neighbors))
           (if (empty? valid-neighbors)
               (cons -inf.0 visited)
               (argmax car (map (λ (neighbor)
                                  (define res (iter neighbor (set-add visited neighbor)))
                                  (cons (add1 (car res)) (cdr res))) valid-neighbors)))]))
  (iter pos (set)))

(define (show-maze maze)
  (string-join (map list->string maze) "\n"))

(define (show-maze-with-visited maze visited-set)
  (show-maze (foldl (λ (c m) (maze-set m c #\O)) maze (set->list visited-set))))

(define (part1 file)
  (define maze (parse-maze (file->string file #:mode 'text)))
  (define bounds (maze-bounds maze))
  (define res (hike maze (vec2 1 0) (vec2 (- (vec2-x bounds) 2) (- (vec2-y bounds) 1))))
  (car res))

; collapses a maze without slopes into a smaller graph with crossroads as vertices
; a crossroad is any tile in the maze where 
; a longest path will necessarily be of the form
; start -> intersection1 -> intersection2 -> ... -> intersectionn -> end
; as any intermediary nodes which are not intersections are either straight paths
; or dead ends

(define (collapse-maze maze)
  (match-define (vec2 w h) (maze-bounds maze))
  (define (find-crossroads pos)
    (match-define (vec2 x y) pos)
    (define (is-crossroad? pos) (and (equal? (maze-get maze pos) #\.)
                                     (not (= (length (maze-neighbors maze pos)) 2))))
    (cond [(< y 0) empty]
          [(< x 0) (find-crossroads (vec2 (sub1 w) (sub1 y)))]
          [(is-crossroad? pos) (cons pos (find-crossroads (vec2 (sub1 x) y)))]
          [else (find-crossroads (vec2 (sub1 x) y))]))
  (find-crossroads (vec2 (sub1 w) (sub1 h))))

(define (part2 file)
  (define maze (maze-kill-slopes (parse-maze (file->string file #:mode 'text))))
  (define bounds (maze-bounds maze))
  (define res (hike maze (vec2 1 0) (vec2 (- (vec2-x bounds) 2) (- (vec2-y bounds) 1))))
  (displayln (show-maze maze))
  (displayln (show-maze-with-visited maze (cdr res)))
  (car res))

(define (test file)
  (define maze (maze-kill-slopes (parse-maze (file->string file #:mode 'text))))
  (define bounds (maze-bounds maze))
  (collapse-maze maze))