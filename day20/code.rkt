#lang racket

(struct rule (name type outs) #:transparent)
; Rule
;  name : String
;  type : 'flip-flop | 'conjunction | 'broadcast
;  outs : (list String)

; function which parses a Rule
(define (parse-rule rule-str)
  (define first-c (string-ref rule-str 0))
  (match-define (list nam out) (string-split rule-str " -> "))
  (define split-out (string-split out ", "))
  (cond [(equal? first-c #\%) (rule (substring nam 1) 'flip-flop split-out)]
        [(equal? first-c #\&) (rule (substring nam 1) 'conjunction split-out)]
        [else (rule nam 'broadcast split-out)]))


; Pulse : 'low | 'high

(struct signal (origin dest pulse) #:transparent)
; Signal
;  origin : String
;  dest : String
;  pulse : Pulse

(struct flip-flop (name state outs) #:transparent)
; Flip-Flop
;  name : String
;  state : 'on | 'off
;  outs : (list Node)

(struct conjunction (name ins outs) #:transparent)
; Conjunction
;  name : String
;  ins : (hash String Pulse)
;  outs : (list Node)

(struct broadcast (name outs) #:transparent)
; Broadcast
;  outs : (list Node)

; Node : Flip-Flop | Conjunction | Broadcast

; function which takes a rule list and finds node
; names which are inputs of a given rule
(define (find-inputs rules tgt)
  (define filtered (filter (λ (x) (index-of (rule-outs x) tgt)) rules))
  (map rule-name filtered))

; function which converts a rule to a node given
; a rule list for reference
(define (rule->node rule rules)
  (define name (rule-name rule))
  (match (rule-type rule)
    ['flip-flop (flip-flop name #f (rule-outs rule))]
    ['conjunction
     (define ins (find-inputs rules (rule-name rule)))
     (define input-map (apply hash (flatten (map (curryr list 'low) ins))))
     (conjunction name input-map (rule-outs rule))]
    ['broadcast (broadcast name (rule-outs rule))]))

; a function which returns a node's name
(define (node-name node)
 ((cond [(flip-flop? node) flip-flop-name]
        [(conjunction? node) conjunction-name]
        [(broadcast? node) broadcast-name]) node))

; a function which takes a node list and makes a node hash map
(define (node-list->node-map lst)
  (apply hash (flatten (map (λ (x) (list (node-name x) x)) lst))))

(define (process-signal-flip-flop node sign)
  (match-define (signal origin dest pulse) sign)
  (match-define (flip-flop name state outs) node)
  (if (equal? pulse 'low)
      (if (equal? state 'on)
          (list (flip-flop name 'off outs)
                (map (λ (dst) (signal name dst 'low)) outs))
          (list (flip-flop name 'on outs)
                (map (λ (dst) (signal name dst 'high)) outs)))
      (list node '())))

(define (process-signal-conjunction node sign)
  (match-define (signal origin dest pulse) sign)
  (match-define (conjunction name ins outs) node)
  (define new-conjunction (conjunction name (hash-set ins origin pulse) outs))
  (define new-pulse (if (andmap (curry equal? 'high) (hash-values (conjunction-ins new-conjunction)))
                        'low
                        'high))
  (list new-conjunction
        (map (λ (dst) (signal name dst new-pulse)) outs)))

(define (process-signal-broadcast node sign)
  (match-define (signal origin dest pulse) sign)
  (match-define (broadcast name outs) node)
  (list node
        (map (λ (dst) (signal name dst pulse)) outs)))

; a function which processes a signal by a node, generating
; a new node state and new signals (list Node Signal ...)
(define (process-signal node sign)
  (match-define (signal origin dest pulse) sign)
  ((cond [(flip-flop? node) process-signal-flip-flop]
        [(conjunction? node) process-signal-conjunction]
        [(broadcast? node) process-signal-broadcast]) node sign))

(struct pulses (low high) #:transparent)
; Pulses (counter)

; a function which updates a pulses counter with a signal
(define (upd-pulses ps s)
  (define pulse (signal-pulse s))
  (cond [(equal? pulse 'high) (pulses (pulses-low ps) (add1 (pulses-high ps)))]
        [(equal? pulse 'low) (pulses (add1 (pulses-low ps)) (pulses-high ps))]))

; a function which processes signals and counts them
(define (process-signals node-map counter signals)
  (cond [(empty? signals) (list node-map counter)]
        [else
         (define s (first signals))
         (define new-counter (upd-pulses counter s))
         (define dest (signal-dest s))
         (when (and (equal? dest "rx") (equal? (signal-pulse s) 'low))
           (println s)
           (println counter))
         (cond [(hash-has-key? node-map dest)
                (define node (hash-ref node-map dest))
                (match-define (list new-node new-signals) (process-signal node s))
                (process-signals (hash-set node-map dest new-node) new-counter (append (rest signals) new-signals))]
               [else (process-signals node-map new-counter (rest signals))])]))
         

; a function which sends n signals to broadcast and sees what happens
(define (spam-broadcast node-map n)
  (define (iter node-map counter n)
    (cond [(equal? n 0) counter]
          [else
           (match-define (list new-node-map new-counter) (process-signals node-map counter (list (signal "button" "broadcaster" 'low))))
           (iter new-node-map new-counter (sub1 n))]))
  (iter node-map (pulses 0 0) n))

; part 1
(define (part1 file)
  (define rules (map parse-rule (file->lines file)))
  (define nodes (map (curryr rule->node rules) rules))
  (define node-map (node-list->node-map nodes))
  (define counter (spam-broadcast node-map 10000))
  (* (pulses-low counter) (pulses-high counter)))