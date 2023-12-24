#lang racket

(struct pos (x y) #:transparent)
(struct starfield (stars w h) #:transparent)

(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

(define (parse-starmap star-str)
  (define lines (split-lines star-str))
  (define w (string-length (first lines)))
  (define h (length lines))
  (define (parse-line y stars)
    (define (parse-column x stars)
      (define c (string-ref (list-ref lines y) x))
      (if (equal? c #\#)
          (cons (pos x y) stars)
          stars))
    (define (parse-columns x stars)
      (if (< x 0)
          stars
          (parse-columns (sub1 x) (parse-column x stars))))
    (parse-columns (sub1 w) stars))
  (define (parse-lines y stars)
    (if (< y 0)
        stars
        (parse-lines (sub1 y) (parse-line y stars))))
  (starfield (parse-lines (sub1 h) empty) w h))

(define (find-empty-rows sf)
  (define stars (starfield-stars sf))
  (define (iter row lst)
    (if (< row 0)
        lst
        (if (ormap (λ (s) (equal? (pos-y s) row)) stars)
            (iter (sub1 row) lst)
            (iter (sub1 row) (cons row lst)))))
  (iter (sub1 (starfield-h sf)) empty))

(define (find-empty-columns sf)
  (define stars (starfield-stars sf))
  (define (iter col lst)
    (if (< col 0)
        lst
        (if (ormap (λ (s) (equal? (pos-x s) col)) stars)
            (iter (sub1 col) lst)
            (iter (sub1 col) (cons col lst)))))
  (iter (sub1 (starfield-w sf)) empty))

(define (expand sf delta)
  (define (expand-cols cols increment new-sf)
    (define (expand-col col increment new-sf)
      (define limit (+ col (* delta increment)))
      (define stars (starfield-stars new-sf))
      (struct-copy starfield new-sf
                   [stars (map (λ (s) (if (> (pos-x s) limit)
                                          (pos (+ (pos-x s) delta)
                                               (pos-y s))
                                          s))
                               stars)]
                   [w (+ (starfield-w new-sf) delta)]))
    (if (empty? cols)
        new-sf
        (expand-cols (rest cols) (add1 increment) (expand-col (first cols) increment new-sf))))
  (define (expand-rows rows increment new-sf)
    (define (expand-row row increment new-sf)
      (define limit (+ row (* delta increment)))
      (define stars (starfield-stars new-sf))
      (struct-copy starfield new-sf
                   [stars (map (λ (s) (if (> (pos-y s) limit)
                                          (pos (pos-x s)
                                               (+ (pos-y s) delta))
                                          s))
                               stars)]
                   [h (+ (starfield-h new-sf) delta)]))
    (if (empty? rows)
        new-sf
        (expand-rows (rest rows) (add1 increment) (expand-row (first rows) increment new-sf))))
  (define expanded-cols (expand-cols (find-empty-columns sf) 0 sf))
  (define expanded-rows (expand-rows (find-empty-rows expanded-cols) 0 expanded-cols))
  expanded-rows)

(define (distance s1 s2)
  (+ (abs (- (pos-x s1) (pos-x s2)))
     (abs (- (pos-y s1) (pos-y s2)))))

(define (distances stars)
  (define stars2 (cartesian-product stars stars))
  (map (λ (p) (distance (first p) (second p))) stars2))

(define (part1 file)
  (/ (apply + (distances (starfield-stars
                          (expand (parse-starmap (file->string file)) 1))))
     2))

(define (part2 file)
  (/ (apply + (distances (starfield-stars
                          (expand (parse-starmap (file->string file)) (sub1 1000000)))))
     2))