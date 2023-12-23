#lang racket

(define (parse-history line)
  (map string->number (string-split line)))

(define (delta-sequence history)
  (cond
    [(< (length history) 2) empty]
    [(= (length history) 2) (list (- (second history) (first history)))]
    [else (cons (- (second history) (first history)) (delta-sequence (rest history)))]))

(define (all-zero? lst)
  (andmap zero? lst))

; new-value_0 = last-value_0 + new-value_1
; new-value_1 = last-value_1 + new-value_2
; ... until new-value_i = 0

(define (new-value seq)
  (define last-value (last seq))
  (if (all-zero? seq)
      0
      (+ last-value (new-value (delta-sequence seq)))))

(define (part1 file)
  (define seqs (map parse-history (file->lines file)))
  (apply + (map new-value seqs)))

; prev-value: value in negative history
; first-value: first value in sequence
; prev-value_0 = first-value_0 - prev-value_1
; prev-value_1 = first-value_1 - prev-value_2
; ... until prev-value_i = 0

(define (prev-value seq)
  (define first-value (first seq))
  (if (all-zero? seq)
      0
      (- first-value (prev-value (delta-sequence seq)))))

(define (part2 file)
  (define seqs (map parse-history (file->lines file)))
  (apply + (map prev-value seqs)))