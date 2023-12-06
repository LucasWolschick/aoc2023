#lang racket

(struct interval (dest-start src-start length) #:transparent)

(define (interval-map interv src-val)
  (and (<= (interval-src-start interv) src-val (+ (interval-src-start interv) (interval-length interv) -1))
       (+ (- src-val (interval-src-start interv)) (interval-dest-start interv))))

(define (parse-seed-list seeds-str)
  (map string->number (string-split (string-trim
                                     (first (string-split seeds-str "seeds: "))))))

(define (parse-interval interval-str)
  (define parts (map string->number (string-split (string-trim interval-str))))
  (interval (first parts) (second parts) (third parts)))

(define (parse-map lines)
  (define (rec lines intervals)
    (cond [(empty? lines) intervals]
          [(empty-string? (first lines)) intervals]
          [else (rec (rest lines) (cons (parse-interval (string-trim (first lines))) intervals))]))
  (reverse (rec lines empty)))

(define (drop-until-after p lst)
  (cond [(empty? lst) lst]
        [(p (first lst)) (rest lst)]
        [else (drop-until-after p (rest lst))]))

(define (empty-string? s)
  (not (non-empty-string? s)))

(define (parse-maps lines)
  (define (p-seeds lns seeds maps)
    (p-map (rest lns) (parse-seed-list (first lns)) empty))
  (define (p-map lns seeds maps)
    (cond [(empty? lns) (list seeds maps)]
          [(empty-string? (first lns)) (p-map (rest lns) seeds maps)]
          [else (p-map (drop-until-after empty-string? lns) seeds (cons (parse-map (drop lns 1)) maps))]))
  (let ([v (p-seeds lines empty empty)])
    (values (first v) (reverse (second v)))))

(define (map-to-interval-list value intervals)
  (if (empty? intervals)
      value
      (or (interval-map (first intervals) value)
          (map-to-interval-list value (rest intervals)))))

(define (map-seed-to-location seed maps)
  (if (empty? maps)
      seed
      (map-seed-to-location (map-to-interval-list seed (first maps)) (rest maps))))
        
(define (map-seeds-to-locations seeds maps)
  (map (lambda (seed) (map-seed-to-location seed maps)) seeds))

(define (part-1 file)
  (let-values ([(seeds maps) (parse-maps (file->lines file))])
    (apply min (map-seeds-to-locations seeds maps))))