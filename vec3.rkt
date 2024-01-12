#lang racket

(provide (all-defined-out))

(struct vec3 (x y z) #:transparent)
; Vec3 is a vector in R^3
;  x, y, z : Number

;; Vec3 Vec3 -> Vec3
;;
;; Sums two Vec3s.
;;  lhs, rhs : vec3?  
(define (vec3+ lhs rhs)
  (vec3 (+ (vec3-x lhs) (vec3-x rhs))
        (+ (vec3-y lhs) (vec3-y rhs))
        (+ (vec3-z lhs) (vec3-z rhs))))

;; Vec3 Vec3 -> Vec3
;;
;; Subtracts a Vec3 from another.
;;  lhs, rhs : vec3?
(define (vec3- lhs rhs)
  (vec3 (- (vec3-x lhs) (vec3-x rhs))
        (- (vec3-y lhs) (vec3-y rhs))
        (- (vec3-z lhs) (vec3-z rhs))))

;; Number Vec3 -> Vec3
;;
;; Multiplies a Vec3 by a scalar.
;;  k : number?
;;  v : vec3?
(define (vec3* k v)
  (vec3 (* k (vec3-x v))
        (* k (vec3-y v))
        (* k (vec3-z v))))

;; Vec3 -> Number
;;
;; Computes a Vec3's length.
;;  v : vec3?
(define (vec3-length v)
  (sqrt (+ (sqr (vec3-x v))
           (sqr (vec3-y v))
           (sqr (vec3-z v)))))

;; Vec3 -> Vec3
;;
;; Computes a unit Vec3 parallel to the Vec3.
;;  v : vec3?
(define (vec3-unit v)
  (define 1/len (/ 1 (vec3-length v)))
  (vec3* 1/len v))

;; Vec3 Vec3 -> Number
;;
;; Computes the dot (inner) product between two Vec3.
;;  lhs, rhs : vec3?
(define (vec3-dot lhs rhs)
  (+ (* (vec3-x lhs) (vec3-x rhs))
     (* (vec3-y lhs) (vec3-y rhs))
     (* (vec3-z lhs) (vec3-z rhs))))

;; Vec3 Vec3 -> Vec3
;;
;; Computes the cross (wedge) product between two Vec3.
;;  lhs, rhs : vec3?
(define (vec3-cross lhs rhs)
  (match-define (vec3 x1 y1 z1) lhs)
  (match-define (vec3 x2 y2 z2) rhs)
  (vec3 (- (* y1 z2) (* y2 z1))
        (- (* z1 x2) (* z2 x1))
        (- (* x1 y2) (* x2 y1))))