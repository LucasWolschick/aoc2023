#lang racket

(struct draw (red green blue) #:transparent)

(define (cubelist->draw cl)
  (define (iter cl s)
    (cond
      [(empty? cl) s]
      [(eq? (second (first cl)) 'red) (iter (rest cl) (struct-copy draw s [red (first (first cl))]))]
      [(eq? (second (first cl)) 'green) (iter (rest cl) (struct-copy draw s [green (first (first cl))]))]
      [(eq? (second (first cl)) 'blue) (iter (rest cl) (struct-copy draw s [blue (first (first cl))]))]
      [else (iter (rest cl) s)]))
  (iter cl (draw 0 0 0)))
    
(define (parse-drawn-cubes s)
  (let* [(pairs (string-split s ", "))
         (pairs (map (lambda (s) (string-split s " ")) pairs))]
    (map (lambda (s) (list (string->number (first s)) (string->symbol (second s)))) pairs)))

(define (parse-game s)
  (map cubelist->draw (map parse-drawn-cubes (string-split (second (string-split s ": ")) "; "))))

(define (draw-valid? d limit)
  (and (<= (draw-red d) (draw-red limit))
                 (<= (draw-green d) (draw-green limit))
                 (<= (draw-blue d) (draw-blue limit))))

(define (game-valid? game limit)
  (andmap (lambda (d) (draw-valid? d limit)) game))

(define (part-1 file)
  (define games (map parse-game (file->lines file)))
  (define limit (draw 12 13 14))
  (apply + (filter-map (lambda (i game)
                (if (game-valid? game limit)
                    (+ i 1)
                    #f)) (range (length games)) games)))

(define (part-2 file)
  (define games (map parse-game (file->lines file)))
  (define (find-max list s)
    (cond [(empty? list) s]
          [else (find-max (rest list) (struct-copy draw s
                                                   [red (max (draw-red s) (draw-red (first list)))]
                                                   [green (max (draw-green s) (draw-green (first list)))]
                                                   [blue (max (draw-blue s) (draw-blue (first list)))]))]))
  (define (power draw) (* (draw-red draw) (draw-green draw) (draw-blue draw)))
  (apply + (map (lambda (game) (power (find-max game (draw 0 0 0)))) games)))

;(define example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
;(define example-results (list (draw 4 0 3) (draw 1 2 6) (draw 0 2 0)))
;(define parsed-results (parse-game example))