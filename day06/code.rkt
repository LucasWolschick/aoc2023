#lang racket

(require examples)

(define (get-times-distances str)
  (define lines (string-split str "\n"))
  (define times (map string->number (rest (string-split (first lines)))))
  (define distances (map string->number (rest (string-split (second lines)))))
  (map (λ (x y) (cons x y)) times distances))

;; T: how long the button is held for
;; D: race duration
;; d: distance
;; d = T*(D - T)
;; roots = 0, D
;; apex = D/2
;; d_max = D/2*(D - D/2) = D/2*D/2 = D^2/4

(examples
 (check-equal? (distance-traveled 0 7) 0)
 (check-equal? (distance-traveled 1 7) 6)
 (check-equal? (distance-traveled 2 7) 10)
 (check-equal? (distance-traveled 3 7) 12)
 (check-equal? (distance-traveled 4 7) 12)
 (check-equal? (distance-traveled 5 7) 10)
 (check-equal? (distance-traveled 6 7) 6)
 (check-equal? (distance-traveled 7 7) 0))

(define (distance-traveled held-length duration)
  (* held-length (- duration held-length)))

;; now we need to know what interval of T values
;; is above some threshold M.
;;
;; T*(D - T) > M
;; - T^2 + DT - M > 0
;; a = -1
;; b = D
;; c = -M
;; delta = b^2 - 4ac = D^2 - 4M
;; x1 = (-b - sqrt(delta)) * 2a
;; x2 = (-b + sqrt(delta)) * 2a
;; are the boundaries of our interval.
;; since our a is negative and our restriction is positive
;; our values will be the integer values between x1 and x2
;; so, floor(x2) - ceil(x1) + 1 is our answer...

(define (solve-quadratic a b c)
  (define delta (- (sqr b) (* 4 a c)))
  (cond
    [(< delta 0) empty]
    [(= delta 0)
     (list (/ (- b) (* 2 a)))]
    [else
     (define sdelta (sqrt delta))
     (define roots
       (list (/ (- (- b) sdelta) (* 2 a))
             (/ (+ (- b) sdelta) (* 2 a))))
     (if (< a 0)
         (reverse roots)
         roots)]))

(define (num-ways duration record)
  (define roots (solve-quadratic -1 duration (- record)))
  (cond
    [(or (empty? roots) (empty? (rest roots))) 0]
    [else
     (define x1 (if (integer? (first roots))
                    (add1 (first roots))
                    (ceiling (first roots))))
     (define x2 (if (integer? (second roots))
                    (sub1 (second roots))
                    (floor (second roots))))
     (if (< x1 x2)
         (+ (- x2 x1) 1)
         0)]))

(define (race-pair-to-ways pair)
  (num-ways (car pair) (cdr pair)))

;; great
(define (part1 file)
  (apply *
         (map race-pair-to-ways (get-times-distances (file->string file)))))

;; okay!
(define (concatenate-pairs pairs)
  (define conc
    (foldl (λ (pair result)
             (cons (string-append (car result) (number->string (car pair)))
                   (string-append (cdr result) (number->string (cdr pair)))))
           (cons "" "")
           pairs))
  (cons (string->number (car conc)) (string->number (cdr conc))))

(define (part2 file)
  (race-pair-to-ways (concatenate-pairs (get-times-distances (file->string file)))))