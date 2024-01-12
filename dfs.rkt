#lang racket

(require "bfs.rkt")

(provide (all-defined-out))

(struct dfsres (d v p f) #:transparent)
; DFS-Results
;  d : (hash V number) - time discovered
;  v : (hash V boolean) - visited or not
;  p : (hash V V | null) - parent
;  f : (hash V number) - time finished

; builds a brand-spanking-new dfs results object
(define (dfsres-new)
  (dfsres (hash) (hash) (hash) (hash)))

;; returns a vertex's discovery time
(define (dfsres-discovery r v)
  (hash-ref (dfsres-d r) v null))

;; sets a dfsres's distance of a vertex to value
(define (dfsres-discovery-set r v value)
  (struct-copy dfsres r [d (hash-set (dfsres-d r) v value)]))

;; returns whether a vertex was visited or not
(define (dfsres-visited r v)
  (hash-ref (dfsres-v r) v #f))

;; sets a dfsres's visited of a vertex to value
(define (dfsres-visited-set r v value)
  (struct-copy dfsres r [v (hash-set (dfsres-v r) v value)]))

;; returns a vertex's parent
(define (dfsres-parent r v)
  (hash-ref (dfsres-p r) v null))

;; sets a dfsres's parent of a vertex to value
(define (dfsres-parent-set r v value)
  (struct-copy dfsres r [p (hash-set (dfsres-p r) v value)]))

;; returns a vertex's finished time
(define (dfsres-finished r v)
  (hash-ref (dfsres-f r) v null))

;; sets a dfsres's distance of a vertex to value
(define (dfsres-finished-set r v value)
  (struct-copy dfsres r [f (hash-set (dfsres-f r) v value)]))

;; runs ay DFS
(define (dfs G all-vertices)
  (define res (dfsres-new))
  (define time 0)
  (define (for-loop verts props time)
    (cond [(empty? verts) (list props time)]
          [(dfsres-visited props (first verts)) (for-loop (rest verts) props time)]
          [else (apply for-loop (rest verts) (dfs-visit (first verts) props time))]))
  (define (dfs-visit u props time)
    (define pre-time (add1 time))
    (define pre-props (dfsres-discovery-set
                       (dfsres-visited-set props u #t)
                       u pre-time))
    (define (for-adju GadjU props time)
      (cond [(empty? GadjU) (list props time)]
            [(dfsres-visited props (first GadjU)) (for-adju (rest GadjU) props time)]
            [else
             (define new-props (dfsres-parent-set props (first GadjU) u))
             (apply for-adju (rest GadjU) (dfs-visit (first GadjU) new-props time))]))
    (define for-res (for-adju ((graph-neighbors G) u) pre-props pre-time))
    (define new-time (add1 (second for-res)))
    (define post-props (dfsres-finished-set (first for-res) u new-time))
    (list post-props new-time))
  (first (for-loop all-vertices res time)))

;; connected components (undirected graph)
(define (connected-components G all-vertices)
  (define doofus (dfs G all-vertices))
  (define roots (filter (λ (x) (equal? (dfsres-parent doofus x) null)) all-vertices))
  (map (λ (root) (/ (- (dfsres-finished doofus root) (dfsres-discovery doofus root) -1) 2)) roots))