#lang racket

; The HASH algorithm is a way to turn any string of characters into a single number in the range 0 to 255.
; To run the HASH algorithm on a string, start with a current value of 0. Then, for each character in the string starting from the beginning:
;
; Determine the ASCII code for the current character of the string.
; Increase the current value by the ASCII code you just determined.
; Set the current value to itself multiplied by 17.
; Set the current value to the remainder of dividing itself by 256.

(define (HASH str)
  (define chars (string->list str))
  (define (munch chars curr)
    (cond
      [(empty? chars) curr]
      [else
       (define c (first chars))
       (define new-curr (remainder (* 17 (+ (char->integer c) curr)) 256))
       (munch (rest chars) new-curr)]))
  (munch chars 0))

(define (split-input whole)
  (define terms (string-split whole ","))
  (apply + (map HASH terms)))

(define (part1 file)
  (split-input (apply append (file->lines file))))

;; Lens
(struct lens (label focal-power) #:transparent)

;; Command is either
;;  (remove-lens label)
;;  (add-lens label focal-power)

(struct remove-lens (label) #:transparent)
(struct add-lens (label focal-power) #:transparent)

; procedure that parses a string into a command
(define (parse-command str)
  (define parts (string-split str #rx"[=-]"))
  (if (= (length parts) 1)
      (remove-lens (first parts))
      (add-lens (first parts) (string->number (second parts)))))

; procedure that gets a command's label
(define (get-label command)
  (cond
    [(remove-lens? command) (remove-lens-label command)]
    [(add-lens? command) (add-lens-label command)]))

; procedure that gets a label's box
(define (get-label-box label)
  (HASH label))

; procedure that takes a box and a lens and
; - replaces a lens with the same label in the box if it exists
; - or appends the new lens to the box
(define (add-lens-to-box box lens)
  (define i (index-where box (λ (l) (equal? (lens-label l) (lens-label lens)))))
  (if i
      (list-set box i lens)
      (append box (list lens))))

; procedure that takes a box and a lens label and
; removes the lens from the box
(define (remove-lens-from-box box label)
  (filter (λ (l) (not (equal? (lens-label l) label))) box))

; procedure that takes a box array and a command
; and takes the box array to the next state
(define (update-array box-array command)
  (define box-i (get-label-box (get-label command)))
  (cond
    [(add-lens? command) (list-set box-array box-i (add-lens-to-box (list-ref box-array box-i) (lens (add-lens-label command) (add-lens-focal-power command))))]
    [(remove-lens? command) (list-set box-array box-i (remove-lens-from-box (list-ref box-array box-i) (remove-lens-label command)))]))

; procedure that evaluates a series of commands
; on a box array
(define (eval-commands box-array commands)
  (if (empty? commands)
      box-array
      (eval-commands (update-array box-array (first commands)) (rest commands))))

; procedure that parses commands from big string
(define (parse-commands big-string)
  (define terms (string-split big-string ","))
  (map parse-command terms))

; procedure that calculates the focusing power of a lens
(define (focusing-power box-i lens-i lens-length)
  (* (+ 1 box-i)
     (+ 1 lens-i)
     lens-length))

; procedure that calculates the focusing power of all lenses
; in a box array
(define (focusing-power-of-box-array box-array)
  (define (do-box box-i box)
    (apply + (map (λ (l lens-i) (focusing-power box-i lens-i (lens-focal-power l))) box (range (length box)))))
  (apply + (map do-box (range (length box-array)) box-array)))

; part 2
(define (part2 file)
  (define box-array (make-list 256 empty))
  (define commands (parse-commands (apply append (file->lines file))))
  (define final-array (eval-commands box-array commands))
  (focusing-power-of-box-array final-array))