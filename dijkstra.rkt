#lang racket

(require "bfs.rkt")
(require data/heap)

(provide (all-defined-out))

(struct weighted-graph graph (w))
; Weighted-Graph > Graph
;  w : V V -> Number

; Dijkstra(G, w, s)
;   Initialize-Single-Source(G, R, s)
;   Q = G.V
;   while Q != {}
;     u = Extract-Min(Q)
;     for v in G.adj[u]
;       Relax(R, u,v,w)
(define (dijkstra G s)
  (define R (initialize-single-source G s))
  (define Q (make-heap (λ (l r)
                         (<= (cdr l) (cdr r)))))
  ; Q contains (state, R[state].d) pairs
  (heap-add! Q (cons s 0))

  ; Relax(R, u, v, w)
  ;   if R[v].d > R[u].d + w(u, v)
  ;     R[v].d = R[u].d + w(u, v)
  ;     R[v].pi = u
  (define (relax R u v)
    (define new-w (+ (bfsres-distance R u) ((weighted-graph-w G) u v)))
    (if (> (bfsres-distance R v) new-w)
        (begin
          (heap-add! Q (cons v new-w))
          (bfsres-visited-set (bfsres-distance-set (bfsres-parent-set R v u) v new-w) v #t))
        R))
  
  (define (while-loop R)
    (cond [(= (heap-count Q) 0) R]
          [else
           (define u (car (heap-min Q)))
           (heap-remove-min! Q)
           (while-loop (for-loop R u ((graph-neighbors G) G u)))]))
  (define (for-loop R u GadjU)
    (if (empty? GadjU)
        R
        (for-loop (relax R u (first GadjU)) u (rest GadjU))))
  (while-loop R))

; Initialize-Single-Source(G, R, s)
;   for v in G.V
;     R[v].d = inf
;     R[v].pi = nil
;   R[s].d = 0
(define (initialize-single-source s)
  (bfsres-new s))