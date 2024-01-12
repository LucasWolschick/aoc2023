#lang racket

(provide (struct-out graph)
         (struct-out bfsres)
         bfsres-new
         bfsres-distance
         bfsres-distance-set
         bfsres-visited
         bfsres-visited-set
         bfsres-parent
         bfsres-parent-set
         bfs)

(struct graph (neighbors))
; Graph
;  neighbors : V -> ( list V )

(struct bfsres (d v p) #:transparent)
; BFS-Results
;  d : (hash V number) - minimum distance
;  v : (hash V boolean) - visited or not
;  p : (hash V V | null) - parent

; builds a brand-spanking-new bfs results object of length n
; with start s
(define (bfsres-new s)
  (define basic (bfsres (hash) (hash) (hash)))
  (struct-copy bfsres basic
               [d (hash-set (bfsres-d basic) s 0)]
               [v (hash-set (bfsres-v basic) s #t)]
               [p (hash-set (bfsres-p basic) s null)]))

;; returns a vertex's minimum distance from the origin
(define (bfsres-distance r v)
  (hash-ref (bfsres-d r) v +inf.0))

;; sets a bfsres's distance of a vertex to value
(define (bfsres-distance-set r v value)
  (struct-copy bfsres r [d (hash-set (bfsres-d r) v value)]))

;; returns whether a vertex was visited or not
(define (bfsres-visited r v)
  (hash-ref (bfsres-v r) v #f))

;; sets a bfsres's visited of a vertex to value
(define (bfsres-visited-set r v value)
  (struct-copy bfsres r [v (hash-set (bfsres-v r) v value)]))

;; returns a vertex's parent
(define (bfsres-parent r v)
  (hash-ref (bfsres-p r) v null))

;; sets a bfsres's parent of a vertex to value
(define (bfsres-parent-set r v value)
  (struct-copy bfsres r [p (hash-set (bfsres-p r) v value)]))

; runs bfs on a graph from position s returing a BFS-Results
(define (bfs graph s)
  (define props (bfsres-new s))
  (define Q (list s))

  (define (while-loop Q props)
    (cond [(empty? Q) props]
          [else
           (define u (first Q))
           (define neighbors ((graph-neighbors graph) u))
           (define (for-loop props neighbors Q-new)
             (cond [(empty? neighbors) (list props Q-new)]
                   [(not (bfsres-visited props (first neighbors)))
                    (define v (first neighbors))
                    (define new-props
                      (struct-copy bfsres props
                                   [v (hash-set (bfsres-v props) v #t)]
                                   [d (hash-set (bfsres-d props) v (add1 (hash-ref (bfsres-d props) u)))]
                                   [p (hash-set (bfsres-p props) v u)]))
                    (for-loop new-props (rest neighbors) (append Q-new (list v)))]
                   [else (for-loop props (rest neighbors) Q-new)]))
           (match-define (list new-props queue-additions) (for-loop props neighbors empty))
           (while-loop (append (rest Q) queue-additions) new-props)]))
  (while-loop Q props))