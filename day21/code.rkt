#lang racket

; function which splits (you get the idea)
(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

; function which parses a maze
(define (parse-maze maze-str)
  (define lines (split-lines maze-str))
  (map string->list lines))

(struct pos (x y) #:transparent)
; Position
;  x, y : Integer

; function which gets a maze's cell
(define (get-maze maze x y)
  (list-ref (list-ref maze y) x))

; function gets maze bounds
(define (maze-bounds maze)
  (pos (length (first maze)) (length maze)))

; function which sees if a maze's position
; is bounded
(define (bounded? maze p)
  (match-define (pos x y) p)
  (match-define (pos w h) (maze-bounds maze))
  (and (<= 0 x (sub1 w))
       (<= 0 y (sub1 h))))

; function which finds a maze's starting position
(define (find-start maze)
  (match-define (pos w h) (maze-bounds maze))
  (define (iter x y)
    (cond
      [(< y 0) #f]
      [(< x 0) (iter (sub1 w) (sub1 y))]
      [(equal? (get-maze maze x y) #\S) (pos x y)]
      [else (iter (sub1 x) y)]))
  (iter (sub1 w) (sub1 h)))

; function which returns a cell's valid neighbors in a maze
(define (cell-neighbors maze p)
  (match-define (pos x y) p)
  (define init-neighbors (list (pos (add1 x) y)
                               (pos (sub1 x) y)
                               (pos x (add1 y))
                               (pos x (sub1 y))))
  (define (valid? c)
    (match-define (pos x y) c)
    (and (bounded? maze c)
         (not (equal? (get-maze maze x y) #\#))))
  (filter valid? init-neighbors))

; function which maps a maze and a position to
; an index
(define (pos-idx maze p)
  (match-define (pos w h) (maze-bounds maze))
  (match-define (pos x y) p)
  (+ (* y w) x))

; function which maps a maze and an index to
; a position
(define (idx-pos maze idx)
  (match-define (pos w h) (maze-bounds maze))
  (pos (remainder idx w) (quotient idx h)))

(struct bfsres (d visited) #:transparent)
; BFS-Results
;  d : (list number)
;  visited: (list boolean)

; builds a brand-spanking-new bfs results object of length n
; with start s
(define (bfsres-new n s)
  (define basic (bfsres (make-list n +inf.0) (make-list n #f)))
  (struct-copy bfsres basic
               [d (list-set (bfsres-d basic) s 0)]
               [visited (list-set (bfsres-visited basic) s #t)]))

; runs bfs on a maze from position s returing a BFS-Results
(define (bfs maze s)
  (match-define (pos w h) (maze-bounds maze))
  (define sz (* w h))
  (define props (bfsres-new sz (pos-idx maze s)))
  (define Q (list s))

  (define (while-loop Q props)
    (cond [(empty? Q) props]
          [else
           (define u (first Q))
           (define neighbors (cell-neighbors maze u))
           (define (for-loop props neighbors Q-new)
             (cond [(empty? neighbors) (list props Q-new)]
                   [(not (list-ref (bfsres-visited props) (pos-idx maze (first neighbors))))
                    (define v (first neighbors))
                    (define new-props
                      (struct-copy bfsres props
                                   [visited (list-set (bfsres-visited props) (pos-idx maze v)
                                                      #t)]
                                   [d (list-set (bfsres-d props) (pos-idx maze v)
                                                (add1 (list-ref (bfsres-d props) (pos-idx maze u))))]))
                    (for-loop new-props (rest neighbors) (append Q-new (list v)))]
                   [else (for-loop props (rest neighbors) Q-new)]))
           (match-define (list new-props queue-additions) (for-loop props neighbors empty))
           (while-loop (append (rest Q) queue-additions) new-props)]))
  (while-loop Q props))

(define (reachable-steps maze start steps)
  (define res (bfs maze start))
  (define distances (filter integer? (bfsres-d res)))
  (count (λ (x) (and (<= x steps) (= (remainder x 2) (remainder steps 2)))) distances))

; part 1
(define (part1 file (steps 64))
  (define maze (parse-maze (file->string file #:mode 'text)))
  (define start (find-start maze))
  (reachable-steps maze start steps))

; part 2
; ... we do a more analytical approach.
; the bit pattern is independent of walls
; 0 1 0 1 0 1 ...
; we need to determine the borders of our region,
; which is a rhombus (due to manhattan distance)
(define (part2 file (steps 26501365)) 0)