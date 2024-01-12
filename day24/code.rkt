#lang racket

(require "../vec3.rkt" "../vec2.rkt")
(require math/matrix math/array)

(struct hail (pos vel) #:transparent)

(define (parse-hail hail-str)
  (define (parse-triplet triplet-str)
    (apply vec3 (map string->number
                     (map string-trim
                          (string-split triplet-str ",")))))
  (define halves (string-split hail-str "@"))
  (apply hail (map parse-triplet halves)))

(define (hail-at h t)
  (vec3+ (hail-pos h) (vec3* t (hail-vel h))))

(define (intersect-hail-xy h1 h2)
  (match-define (hail (vec3 px py _) (vec3 vx vy _)) h1)
  (match-define (hail (vec3 pu pv _) (vec3 vu vv _)) h2)
  (define coeff (matrix [[vx (- vu)]
                         [vy (- vv)]]))
  (define indep (matrix [[(- pu px)]
                         [(- pv py)]]))
  (define solv (matrix-solve coeff indep (const #f)))
  (cond [solv
         (define coef-h1 (array-ref solv #(0 0)))
         (define coef-h2 (array-ref solv #(1 0)))
         (list (vec3+ (vec3* coef-h1 (hail-vel h1)) (hail-pos h1)) coef-h1 coef-h2)]
        [else null]))

(define (count-collisions-xy hails min max)
  (define pairs (combinations hails 2))
  (define colls (map (λ (pair) (cons pair (apply intersect-hail-xy pair))) pairs))
  (define (valid-collision? collision-pair)
    (define collision (cdr collision-pair))
    (define pair (car collision-pair))
    (cond [(null? collision) #f]
          [(negative? (second collision)) #f]
          [(negative? (third collision)) #f]
          [(not (and (<= (vec3-x min) (vec3-x (first collision)) (vec3-x max))
                     (<= (vec3-y min) (vec3-y (first collision)) (vec3-y max))
                     (<= (vec3-z min) (vec3-z (first collision)) (vec3-z max)))) #f]
          [else #t]))
  (count valid-collision? colls))

(define (part1 file (min 200000000000000) (max 400000000000000))
  (define hails (map parse-hail (file->lines file)))
  (count-collisions-xy hails
                       (vec3 min min -inf.0)
                       (vec3 max max +inf.0)))