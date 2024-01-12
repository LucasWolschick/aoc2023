#lang racket

(provide (all-defined-out))

(struct vec2 (x y) #:transparent)
; Vec2 is a vector in R^2
;  x, y : Number

;; Vec2 Vec2 -> Vec2
;;
;; Sums two Vec2s.
;;  lhs, rhs : vec2?  
(define (vec2+ lhs rhs)
  (vec2 (+ (vec2-x lhs) (vec2-x rhs))
        (+ (vec2-y lhs) (vec2-y rhs))))

;; vec2 vec2 -> vec2
;;
;; Subtracts a vec2 from another.
;;  lhs, rhs : vec2?
(define (vec2- lhs rhs)
  (vec2 (- (vec2-x lhs) (vec2-x rhs))
        (- (vec2-y lhs) (vec2-y rhs))))

;; Number vec2 -> vec2
;;
;; Multiplies a vec2 by a scalar.
;;  k : number?
;;  v : vec2?
(define (vec2* k v)
  (vec2 (* k (vec2-x v))
        (* k (vec2-y v))))

;; vec2 -> Number
;;
;; Computes a vec2's length.
;;  v : vec2?
(define (vec2-length v)
  (sqrt (+ (sqr (vec2-x v))
           (sqr (vec2-y v)))))

;; vec2 -> vec2
;;
;; Computes a unit vec2 parallel to the vec2.
;;  v : vec2?
(define (vec2-unit v)
  (define 1/len (/ 1 (vec2-length v)))
  (vec2* 1/len v))

;; vec2 vec2 -> Number
;;
;; Computes the dot (inner) product between two vec2.
;;  lhs, rhs : vec2?
(define (vec2-dot lhs rhs)
  (+ (* (vec2-x lhs) (vec2-x rhs))
     (* (vec2-y lhs) (vec2-y rhs))))

;; vec2 vec2 -> Number
;;
;; Computes the cross (wedge) product between two vec2.
;;  lhs, rhs : vec2?
(define (vec2-cross lhs rhs)
  (match-define (vec2 x1 y1) lhs)
  (match-define (vec2 x2 y2) rhs)
  (- (* x1 y2) (* x2 y1)))