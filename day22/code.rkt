#lang racket

(struct pos (x y z) #:transparent)
(struct brick (position size) #:transparent)

; parse a position to a pos
(define (parse-position pos-str)
  (apply pos (map string->number (string-split pos-str ","))))

; parse a brick to a brick
(define (parse-brick brick-str)
  (define positions (map parse-position (string-split brick-str "~")))
  (define size (pos- (pos+ (pos 1 1 1) (second positions)) (first positions)))
  (brick (first positions) size))

; euclidean distance between two pos
(define (distance p1 p2)
  (match-define (pos x1 y1 z1) p1)
  (match-define (pos x2 y2 z2) p2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)) (sqr (- z1 z2)))))

; add a pos
(define (pos+ p1 p2)
  (match-define (pos x1 y1 z1) p1)
  (match-define (pos x2 y2 z2) p2)
  (pos (+ x1 x2) (+ y1 y2) (+ z1 z2)))

; sub a pos
(define (pos- p1 p2)
  (match-define (pos x1 y1 z1) p1)
  (match-define (pos x2 y2 z2) p2)
  (pos (- x1 x2) (- y1 y2) (- z1 z2)))

; mult a pos
(define (pos* k p)
  (match-define (pos x y z) p)
  (pos (* k x) (* k y) (* k z)))

; length of pos
(define (pos-len p)
  (match-define (pos x y z) p)
  (sqrt (+ (sqr x) (sqr y) (sqr z))))

; unit vector
(define (pos-unit p)
  (define 1/len (/ 1 (pos-len p)))
  (pos* 1/len p))

; lerp between two pos
(define (lerp p1 p2 alpha)
  (pos+ (pos* (- 1 alpha) p1) (pos* alpha p2)))

; aabb intersect for single axis
; takes segment start and segment length
; assumes length > 0
(define (aabb-axis x1 w1 x2 w2)
  (define y1 (+ x1 w1))
  (define y2 (+ x2 w2))
  (define (f x1 y1 x2 y2)
    ; invariant: x1 <= x2
    (cond [(>= x2 y1) #f]
          ; x1 <= x2 < y1
          [else #t]))
  (cond [(< x2 x1) (f x2 y2 x1 y1)]
        [else (f x1 y1 x2 y2)]))

; returns whether two bricks intersect
(define (brick-intersect b1 b2)
  (match-define (brick p1 s1) b1)
  (match-define (pos b1x b1y b1z) p1)
  (match-define (pos b1w b1h b1d) s1)
  (match-define (brick p2 s2) b2)
  (match-define (pos b2x b2y b2z) p2)
  (match-define (pos b2w b2h b2d) s2)
  (and (aabb-axis b1x b1w b2x b2w)
       (aabb-axis b1y b1h b2y b2h)
       (aabb-axis b1z b1d b2z b2d)))

; returns a brick moved 1 space down
(define (lower-brick b)
  (match-define (brick p1 p2) b)
  (define delta (pos 0 0 -1))
  (brick (pos+ p1 delta) p2))

; returns a brick moved 1 space up
(define (raise-brick b)
  (match-define (brick p1 p2) b)
  (define delta (pos 0 0 1))
  (brick (pos+ p1 delta) p2))

; returns a brick's lower corner
(define (brick-lower-corner b)
  (pos+ (brick-position b) (brick-size b)))

; returns whether a brick is clipping the floor
(define (brick-clipping-floor? b)
  (< (pos-z (brick-position b)) 0))

; returns whether a brick is intersecting the floor
; or a brick in the brick list
(define (brick-clipping? b bricks)
  (or (brick-clipping-floor? b)
      (ormap (curry brick-intersect b) bricks)))

; settles all bricks in a brick list
; orders the brick list by height of lower right corner
(define (settle-bricks bricks)
  (define sorted-bricks (sort bricks
                              (λ (lhs rhs)
                                (< (pos-z (brick-position lhs))
                                   (pos-z (brick-position rhs))))))
  ;(println sorted-bricks)
  (define (settle-brick unsettled settled)
    (if (empty? unsettled)
        settled
        (if (brick-clipping? (first unsettled) settled)
            (settle-brick (rest unsettled) (cons (raise-brick (first unsettled)) settled))
            (settle-brick (cons (lower-brick (first unsettled)) (rest unsettled)) settled))))
  (settle-brick (cons (lower-brick (first sorted-bricks)) (rest sorted-bricks)) empty))

; checks to see if a brick set is stable
(define (are-bricks-stable other-bricks)
  (set-empty? (set-subtract (settle-bricks other-bricks) other-bricks)))

; counts how many bricks are supporting other bricks in a list of bricks
(define (count-supports bricks)
  (define parallel-universes (map (λ (i) (append (take bricks i) (drop bricks (add1 i)))) (range (length bricks))))
  (count are-bricks-stable parallel-universes))

(define (part1 file)
  (define bricks (map parse-brick (file->lines file)))
  (define settled-bricks (settle-bricks bricks))
  (count-supports settled-bricks))
  ;settled-bricks)