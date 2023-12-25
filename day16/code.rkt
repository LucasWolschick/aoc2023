#lang racket

; function which splits a string into lines
(define (split-lines str)
  (string-split str #rx"[\newline\return]+"))

; function which parses a string into a maze
(define (parse-maze maze-str)
  (define lines (split-lines maze-str))
  (map string->list lines))

; Direction can be 'up, 'down, 'left, 'right

; Position represents a 2d position
(struct pos (x y) #:transparent)

; Ray represents a ray in the maze, containing a position and a direction
; pos : Position
; dir : Direction - direction of movement
(struct ray (pos dir) #:transparent)

; function which takes a position and direction and returns new position
(define (move p dir)
  (cond
    [(equal? dir 'up) (pos (pos-x p) (sub1 (pos-y p)))]
    [(equal? dir 'down) (pos (pos-x p) (add1 (pos-y p)))]
    [(equal? dir 'left) (pos (sub1 (pos-x p)) (pos-y p))]
    [(equal? dir 'right) (pos (add1 (pos-x p)) (pos-y p))]))

; function which gets a maze's bounds
(define (maze-bounds maze)
  (define h (length maze))
  (define w (length (first maze)))
  (pos w h))

; function which returns whether a coordinate is inside a maze
(define (is-bounded? maze p)
  (define bounds (maze-bounds maze))
  (and (<= 0 (pos-x p) (sub1 (pos-x bounds)))
       (<= 0 (pos-y p) (sub1 (pos-y bounds)))))

; function which returns a maze's tile
(define (get-maze maze p)
  (if (not (is-bounded? maze p))
      #\.
      (list-ref (list-ref maze (pos-y p)) (pos-x p))))

; function which sets a maze tile
(define (set-maze maze p v)
  (match-define (pos x y) p)
  (if (not (is-bounded? maze p))
      maze
      (list-set maze y (list-set (list-ref maze y) x v))))

; function which builds an energy maze
(define (make-energy-maze maze)
  (match-define (pos w h) (maze-bounds maze))
  (make-list h (make-list w #f)))

; function which takes a list of rays and an energy maze and energizes it
(define (energize-maze energy-maze rays)
  (if (empty? rays)
      energy-maze
      (energize-maze (set-maze energy-maze (ray-pos (first rays)) #t) (rest rays))))

; function which counts energized tiles in an energy maze
(define (energized-tiles energy-maze)
  (define (true? b) (not (false? b)))
  (count true? (flatten energy-maze)))

; function which takes a ray and a maze and returns a list of updated rays
(define (step-ray r maze)
  (define dir (ray-dir r))
  (define tgt-pos (move (ray-pos r) (ray-dir r)))
  (define tgt-cell (get-maze maze tgt-pos))
  (cond
    [(equal? tgt-cell #\.) (list (ray tgt-pos dir))]
    [(equal? tgt-cell #\\)
     (cond [(equal? dir 'up) (list (ray tgt-pos 'left))]
           [(equal? dir 'down) (list (ray tgt-pos 'right))]
           [(equal? dir 'left) (list (ray tgt-pos 'up))]
           [(equal? dir 'right) (list (ray tgt-pos 'down))])]
    [(equal? tgt-cell #\/)
     (cond [(equal? dir 'up) (list (ray tgt-pos 'right))]
           [(equal? dir 'down) (list (ray tgt-pos 'left))]
           [(equal? dir 'left) (list (ray tgt-pos 'down))]
           [(equal? dir 'right) (list (ray tgt-pos 'up))])]
    [(equal? tgt-cell #\|)
     (cond [(equal? dir 'up) (list (ray tgt-pos dir))]
           [(equal? dir 'down) (list (ray tgt-pos dir))]
           [(equal? dir 'left) (list (ray tgt-pos 'up)
                                     (ray tgt-pos 'down))]
           [(equal? dir 'right) (list (ray tgt-pos 'up)
                                      (ray tgt-pos 'down))])]
    [(equal? tgt-cell #\-)
     (cond [(equal? dir 'up) (list (ray tgt-pos 'left)
                                   (ray tgt-pos 'right))]
           [(equal? dir 'down) (list (ray tgt-pos 'left)
                                     (ray tgt-pos 'right))]
           [(equal? dir 'left) (list (ray tgt-pos dir))]
           [(equal? dir 'right) (list (ray tgt-pos dir))])]))

; function which takes a maze and rays and returns only bounded, deduplicated rays
(define (filter-bounded-rays maze rays)
  (define bounded (filter (λ (r) (is-bounded? maze (ray-pos r))) rays))
  (define deduped (set->list (list->set bounded)))
  deduped)

; function which takes a maze, an energy maze and a list of rays and
; steps all rays once, returning the new energy maze and rays
(define (step-rays maze energy-maze rays)
  (define new-energy-maze (energize-maze energy-maze rays))
  (define new-rays (flatten (map (λ (r) (step-ray r maze)) rays)))
  (define filtered-rays (filter-bounded-rays maze new-rays))
  (list new-energy-maze filtered-rays))

; function which steps a maze and rays until there are no more rays
; and returns the final energy maze
(define (step-until-over maze energy-maze rays)
  (cond [(empty? rays) energy-maze]
        [else
         (match-define (list new-energy-maze filtered-rays) (step-rays maze energy-maze rays))
         (step-until-over maze new-energy-maze filtered-rays)]))

; function which returns max energized cell count for a given maze and rays
(define (max-energized-cells maze rays)
  (define energy-maze (make-energy-maze maze))
  (define memo (make-hash))
  (define (iter energy-maze rays)
    (define key (list->set rays))
    (cond [(hash-has-key? memo key)
           (define ref (hash-ref memo key))
           (if (not ref)
               (energized-tiles energy-maze)
               ref)]
          [else
           (hash-set! memo key #f)
           (match-define (list new-energy-maze filtered-rays) (step-rays maze energy-maze rays))
           (define ans (iter new-energy-maze filtered-rays))
           (hash-set! memo key ans)
           ans]))
  (iter energy-maze rays))

; function which converts a energy maze to a string
; with . and #
(define (pretty-print-energy-maze energy-maze)
  (define (cvt b) (if b #\# #\.))
  (string-join (map (λ (line) (list->string (map cvt line))) energy-maze) "\n"))

; part 1
(define (part1 file)
  (define maze (parse-maze (file->string file)))
  (define rays (list (ray (pos -1 0) 'right)))
  (max-energized-cells maze rays))
  ;(define res (step-until-loop maze (make-energy-maze maze) rays))
  ;(energized-tiles res))

; part 2
; this takes a while to run
(define (part2 file)
  (define maze (parse-maze (file->string file)))
  (match-define (pos w h) (maze-bounds maze))
  (define l-rays (map (λ (y) (list (ray (pos -1 y) 'right))) (range h)))
  (define r-rays (map (λ (y) (list (ray (pos w y) 'left))) (range h)))
  (define u-rays (map (λ (x) (list (ray (pos x -1) 'down))) (range w)))
  (define d-rays (map (λ (x) (list (ray (pos x h) 'up))) (range w)))
  (define all-rays (append l-rays r-rays u-rays d-rays))
  (define max-energies (map (lambda (rays)
                              (displayln rays)
                              (max-energized-cells maze rays)) all-rays))
  (apply max max-energies))