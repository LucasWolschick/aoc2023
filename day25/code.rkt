#lang racket

(require "../bfs.rkt")

(define (parse-declaration decl-str)
  (match-define (list name adjs) (string-split decl-str ": "))
  (define adj-list (string-split adjs))
  (list name adj-list))

(define (parse-graph declarations)
  (define (iter declarations adjacencies)
    (cond [(empty? declarations) adjacencies]
          [else
           (match-define (list name adjs) (first declarations))
           (define added (hash-set adjacencies name (append (hash-ref adjacencies name empty) adjs)))
           (define final (foldl (λ (adj adjacencies) (hash-set adjacencies adj (append (hash-ref adjacencies adj empty) (list name)))) added adjs))
           (iter (rest declarations) final)]))
  (iter declarations (hash)))

(define (to-graph-structure g)
  (graph (λ (u) (hash-ref g u))))

(define (build-path-from-bfs brr from to)
  (define (get-vertices v)
    (cond [(null? v) empty]
          [(equal? v to) (list v)]
          [else (append (list v) (get-vertices (bfsres-parent brr v)))]))
  (define (get-edges vtx-lst)
    (cond [(empty? vtx-lst) empty]
          [(empty? (rest vtx-lst)) empty]
          [else (append (list (cons (first vtx-lst) (second vtx-lst)))
                        (get-edges (rest vtx-lst)))]))
  (get-edges (get-vertices from)))

(define (remove-path g path)
  (cond [(empty? path) g]
        [else
         (match-define (cons u v) (first path))
         (define p1 (hash-set g u (remove v (hash-ref g u))))
         (define p2 (hash-set p1 v (remove u (hash-ref g v))))
         (remove-path p2 (rest path))]))

(define (find-path g s t)
  (define brr (bfs (to-graph-structure g) s))
  (if (bfsres-visited brr t)
      (build-path-from-bfs brr t s)
      #f))

(define (find-reachable g v)
  (define brr (bfs (to-graph-structure g) v))
  (filter-map (λ (x) (and (cdr x) (car x))) (hash->list (bfsres-v brr))))

(define (same-component g s t k)
  (define (remove-paths g k)
    (if (= k 0)
        g
        (remove-paths (remove-path g (find-path g s t)) (- k 1))))
  (define gee (remove-paths g k))
  (cond
    [(find-path gee s t) (list (set s t) (set))]
    [else (list (list->set (find-reachable gee s))
                (list->set (find-reachable gee t)))]))

(define (find-cut g k1 k2)
  (for/list ([u (hash-keys g)]
             #:do
             [(define v (hash-ref g u))]
             #:when (or (and (set-member? k1 u)
                             (set-member? k2 v))
                        (and (set-member? k2 u)
                              (set-member? k1 v))))
             (cons u v)))

(define (solve g k)
  (define (process V k1 k2)
    (cond [(empty? V) (list k1 k2)]
          [else
           (define t (first V))
           (cond [(and (not (set-member? k1 t))
                       (not (set-member? k2 t)))
                  (match-define (list ka kb) (same-component g s t k))
                  (process (rest V) (set-union ka k1) (set-union kb k2))]
                 [else (process (rest V) k1 k2)])]))
  (match-define (cons s V) (hash-keys g))
  (define k1 (set s))
  (define k2 (set))
  (process V k1 k2))

(define (part1 file)
  (define graph (parse-graph (map parse-declaration (file->lines file))))
  (match-define (list k1 k2) (solve graph 3))
  (* (set-count k1) (set-count k2)))