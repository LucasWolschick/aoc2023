#lang racket

(require data/heap)

; function which splits a string into lines
(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

(struct pos (x y) #:transparent)
; a position

(struct state (pos dir spd) #:transparent)
; a state
; pos :  Pos
; dir : 'u | 'd | 'l | 'r
; sps : 0 | 1 | 2 | 3

(struct graph (w bounds) #:transparent)
; a graph
; w : (list (list Integer)) - weights
; bounds: Pos - bounds (width, height)

; function which parses a map into a graph
(define (parse-map map-str)
  (define lines (split-lines map-str))
  (define w (map (λ (line) (map (λ (c) (- (char->integer c) (char->integer #\0))) (string->list line))) lines))
  (graph w (pos (length (first w)) (length w))))

; function which gets a graph and returns a list of all vertices (states)
(define (graph-vertices g)
  (match-define (pos width height) (graph-bounds g))
  (define positions (map (curry apply pos) (cartesian-product (range width) (range height))))
  (define states (cartesian-product positions '(u d l r) '(0 1 2 3)))
  (map (curry apply state) states))

; function which generates all possible states for a position
(define (possible-states pos)
  (define states (cartesian-product (list pos) '(u d l r) '(0 1 2 3)))
  (map (curry apply state) states))

; function which generates all possible ultra states for a position
(define (ultra-possible-states pos)
  (define states (cartesian-product (list pos) '(u d l r) '(0 1 2 3 4 5 6 7 8 9 10)))
  (map (curry apply state) states))

; function which takes a state and returns its neighbors for regular crucible
(define (normal-neighbors G v)
  (match-define (pos width height) (graph-bounds G))
  (match-define (pos x y) (state-pos v))
  (define d (state-dir v))
  (define s (state-spd v))
  (define candidates
    (cond
      [(and (equal? d 'u) (< s 3)) (list (state (pos x (sub1 y)) 'u (add1 s))
                                         (state (pos (sub1 x) y) 'l 1)
                                         (state (pos (add1 x) y) 'r 1))]
      [(and (equal? d 'u) (= s 3)) (list (state (pos (sub1 x) y) 'l 1)
                                         (state (pos (add1 x) y) 'r 1))]
    
      [(and (equal? d 'd) (< s 3)) (list (state (pos x (add1 y)) 'd (add1 s))
                                         (state (pos (sub1 x) y) 'l 1)
                                         (state (pos (add1 x) y) 'r 1))]
      [(and (equal? d 'd) (= s 3)) (list (state (pos (sub1 x) y) 'l 1)
                                         (state (pos (add1 x) y) 'r 1))]
    
      [(and (equal? d 'l) (< s 3)) (list (state (pos x (sub1 y)) 'u 1)
                                         (state (pos x (add1 y)) 'd 1)
                                         (state (pos (sub1 x) y) 'l (add1 s)))]
      [(and (equal? d 'l) (= s 3)) (list (state (pos x (sub1 y)) 'u 1)
                                         (state (pos x (add1 y)) 'd 1))]
    
      [(and (equal? d 'r) (< s 3)) (list (state (pos x (sub1 y)) 'u 1)
                                         (state (pos x (add1 y)) 'd 1)
                                         (state (pos (add1 x) y) 'r (add1 s)))]
      [(and (equal? d 'r) (= s 3)) (list (state (pos x (sub1 y)) 'u 1)
                                         (state (pos x (add1 y)) 'd 1))]
      [else empty]))

  ; filter those with out-of-bounds positions
  (define (bounded? state)
    (match-define (pos x y) (state-pos state))
    (and (<= 0 x (sub1 width))
         (<= 0 y (sub1 height))))
  (filter bounded? candidates))

(define (w G u v)
  (match-define (state (pos x y) _ _) v)
  (list-ref (list-ref (graph-w G) y) x))

; Dijkstra(G, w, s)
;   Initialize-Single-Source(G, R, s)
;   Q = G.V
;   while Q != {}
;     u = Extract-Min(Q)
;     for v in G.adj[u]
;       Relax(R, u,v,w)
(define (dijkstra G s neighbors)
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
    (define new-w (+ (spres-get-d R u) (w G u v)))
    (if (> (spres-get-d R v) new-w)
        (begin
          (heap-add! Q (cons v new-w))
          (spres-set-d (spres-set-pi R v u) v new-w))
        R))
  
  (define (while-loop R)
    (cond [(= (heap-count Q) 0) R]
          [else
           (define u (car (heap-min Q)))
           (heap-remove-min! Q)
           (while-loop (for-loop R u (neighbors G u)))]))
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
(struct spres (d pi) #:transparent)

(define (spres-new)
  (spres (hash) (hash)))

(define (spres-get-d s v)
  (hash-ref (spres-d s) v +inf.0))

(define (spres-set-d s v val)
  (struct-copy spres s [d (hash-set (spres-d s) v val)]))

(define (spres-get-pi s v)
  (hash-ref (spres-pi s) v null))

(define (spres-set-pi s v val)
  (struct-copy spres s [pi (hash-set (spres-pi s) v val)]))

(define (initialize-single-source G s)
  (spres-set-d (spres-new) s 0))


; oh lawdy
(define (part1 file)
  (define graph (parse-map (file->string file)))
  (define start (state (pos 0 0) 'd 0))
  (define result (dijkstra graph start normal-neighbors))
  (define bounds (graph-bounds graph))
  (define corner (pos (sub1 (pos-x bounds)) (sub1 (pos-y bounds))))
  (define possibilities (possible-states corner))
  (apply min (map (curry spres-get-d result) possibilities)))

; ow lawdy 2
(define (ultra-neighbors G v)
  (match-define (pos width height) (graph-bounds G))
  (match-define (pos x y) (state-pos v))
  (define d (state-dir v))
  (define s (state-spd v))
  (define candidates
    (cond
      [(= s 0) (list (state (pos x (sub1 y)) 'u 1)
                     (state (pos x (add1 y)) 'd 1)
                     (state (pos (sub1 x) y) 'l 1)
                     (state (pos (add1 x) y) 'r 1))]
      
      [(and (equal? d 'u) (< s 4)) (list (state (pos x (sub1 y)) 'u (add1 s)))]
      [(and (equal? d 'u) (< s 10)) (list (state (pos x (sub1 y)) 'u (add1 s))
                                          (state (pos (sub1 x) y) 'l 1)
                                          (state (pos (add1 x) y) 'r 1))]
      [(and (equal? d 'u) (= s 10)) (list (state (pos (sub1 x) y) 'l 1)
                                          (state (pos (add1 x) y) 'r 1))]

      [(and (equal? d 'd) (< s 4)) (list (state (pos x (add1 y)) 'd (add1 s)))]
      [(and (equal? d 'd) (< s 10)) (list (state (pos x (add1 y)) 'd (add1 s))
                                          (state (pos (sub1 x) y) 'l 1)
                                          (state (pos (add1 x) y) 'r 1))]
      [(and (equal? d 'd) (= s 10)) (list (state (pos (sub1 x) y) 'l 1)
                                          (state (pos (add1 x) y) 'r 1))]

      [(and (equal? d 'l) (< s 4)) (list (state (pos (sub1 x) y) 'l (add1 s)))]
      [(and (equal? d 'l) (< s 10)) (list (state (pos x (sub1 y)) 'u 1)
                                          (state (pos x (add1 y)) 'd 1)
                                          (state (pos (sub1 x) y) 'l (add1 s)))]
      [(and (equal? d 'l) (= s 10)) (list (state (pos x (sub1 y)) 'u 1)
                                          (state (pos x (add1 y)) 'd 1))]

      [(and (equal? d 'r) (< s 4)) (list (state (pos (add1 x) y) 'r (add1 s)))]
      [(and (equal? d 'r) (< s 10)) (list (state (pos x (sub1 y)) 'u 1)
                                          (state (pos x (add1 y)) 'd 1)
                                          (state (pos (add1 x) y) 'r (add1 s)))]
      [(and (equal? d 'r) (= s 10)) (list (state (pos x (sub1 y)) 'u 1)
                                          (state (pos x (add1 y)) 'd 1))]
      [else empty]))
  ; filter those with out-of-bounds positions
  (define (bounded? state)
    (match-define (pos x y) (state-pos state))
    (and (<= 0 x (sub1 width))
         (<= 0 y (sub1 height))))
  (filter bounded? candidates))

(define (part2 file)
  (define graph (parse-map (file->string file)))
  (define start (state (pos 0 0) 'd 0))
  (define result (dijkstra graph start ultra-neighbors))
  (define bounds (graph-bounds graph))
  (define corner (pos (sub1 (pos-x bounds)) (sub1 (pos-y bounds))))
  (define possibilities (filter (λ (state) (>= (state-spd state) 4)) (ultra-possible-states corner)))
  (apply min (map (curry spres-get-d result) possibilities)))