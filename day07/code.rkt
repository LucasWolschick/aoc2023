#lang racket

(define card-strengths (list #\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))

(define (split-hand hand-str)
  (string->list hand-str))

(define (parse-hand hand-str)
  (define (build-hash lst)
    (cond
      [(empty? lst) (hash)]
      [else
       (define h (build-hash (rest lst)))
       (define key (first lst))
       (hash-set h key (add1 (hash-ref h key 0)))]))
  (build-hash (split-hand hand-str)))

(define (hand? hand)
  (and (hash? hand)
       (= (apply + (hash-values hand)) 5)))

(define (contains? haystack needle)
  (not (not (index-of haystack needle))))

(define (count-of haystack needle)
  (length (indexes-of haystack needle)))

(define (five-of-a-kind? hand)
  (= (first (hash-values hand)) 5))

(define (four-of-a-kind? hand)
  (contains? (hash-values hand) 4))

(define (full-house? hand)
  (and (contains? (hash-values hand) 3)
       (contains? (hash-values hand) 2)))

(define (three-of-a-kind? hand)
  (contains? (hash-values hand) 3))

(define (two-pair? hand)
  (= (count-of (hash-values hand) 2) 2))

(define (one-pair? hand)
  (contains? (hash-values hand) 2))

(define (hand-value hand)
  (cond
    [(five-of-a-kind? hand) 0]
    [(four-of-a-kind? hand) 1]
    [(full-house? hand) 2]
    [(three-of-a-kind? hand) 3]
    [(two-pair? hand) 4]
    [(one-pair? hand) 5]
    [else 6]))

(define (card-strength card)
  (index-of card-strengths card))

(define (cards-<? lhs-str rhs-str)
  (define (remap-char c)
    (integer->char (card-strength c)))
  (string<? (list->string (map remap-char (string->list lhs-str)))
            (list->string (map remap-char (string->list rhs-str)))))

(define (hand-<? lhs-str rhs-str)
  (define lhs (parse-hand lhs-str))
  (define rhs (parse-hand rhs-str))
  (or (< (hand-value lhs) (hand-value rhs))
      (and (= (hand-value lhs) (hand-value rhs))
           (cards-<? lhs-str rhs-str))))

(struct hand-bid (hand bid) #:transparent)

(define (line->hand-bid line)
  (define l (string-split line))
  (hand-bid (first l) (string->number (second l))))

(define (sort-hand-bids hand-bids)
  (sort hand-bids (λ (lhb rhb) (hand-<? (hand-bid-hand lhb) (hand-bid-hand rhb)))))

(define (part1 file)
  (define bids (map line->hand-bid (file->lines file)))
  (define sorted (sort-hand-bids bids))
  (define ranked-sorted (map cons sorted (range (length sorted) 0 -1)))
  (define winnings (map (λ (p) (* (hand-bid-bid (car p)) (cdr p))) ranked-sorted))
  (apply + winnings))

(define card-strengths* (list #\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))

(define (card-strength* card)
  (index-of card-strengths* card))

(define (optimized-hand-value hand)
  (cond
    [(= (hash-ref hand #\J 0) 5) (hand-value hand)]
    [else
     (define pairs (hash->list hand))
     (define buffed (filter (λ (p) (not (equal? (car p) #\J))) pairs))
     (define biggest (argmax cdr buffed))
     (define joker-count (hash-ref hand #\J 0))
     (define hand-minus-jokers (hash-remove hand #\J))
     (define hand-plus-bonus (hash-update hand-minus-jokers (car biggest) (λ (x) (+ x joker-count))))
     (hand-value hand-plus-bonus)]))

(define (cards-<?* lhs-str rhs-str)
  (define (remap-char c)
    (integer->char (card-strength* c)))
  (string<? (list->string (map remap-char (string->list lhs-str)))
            (list->string (map remap-char (string->list rhs-str)))))

(define (hand-<?* lhs-str rhs-str)
  (define hand-value optimized-hand-value)
  (define lhs (parse-hand lhs-str))
  (define rhs (parse-hand rhs-str))
  (or (< (hand-value lhs) (hand-value rhs))
      (and (= (hand-value lhs) (hand-value rhs))
           (cards-<?* lhs-str rhs-str))))

(define (sort-hand-bids* hand-bids)
  (sort hand-bids (λ (lhb rhb) (hand-<?* (hand-bid-hand lhb) (hand-bid-hand rhb)))))

(define (part2 file)
  (define bids (map line->hand-bid (file->lines file)))
  (define sorted (sort-hand-bids* bids))
  (define ranked-sorted (map cons sorted (range (length sorted) 0 -1)))
  (define winnings (map (λ (p) (* (hand-bid-bid (car p)) (cdr p))) ranked-sorted))
  (apply + winnings))