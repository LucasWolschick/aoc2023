#lang racket

(struct part (x m a s) #:transparent)
; Part
;  x, m, a, s : Integer

(struct workflow (name rules) #:transparent)
; Workflow
;  name : String
;  rules : (list Rule)

; Rule : Cond-Rule | Uncond-Rule

(struct cond-rule (condition consequence) #:transparent)
; Cond-Rule
;  condition : Condition
;  consequence : Consequence

(struct uncond-rule (consequence) #:transparent)
; Uncond-Rule
;  consequence : Consequence

(struct condition (cat cmp idx) #:transparent)
; Condition
;  cat : #\x | #\m | #\a | #\s
;  cmp : < | >
;  idx : Integer

; Consequence : RuleName | 'accept | 'reject | 'next (only used internally, never stored)

; RuleName : String

; function which parses a condition
(define (parse-condition cond-str)
  (define cat (string-ref cond-str 0))
  (define cmp
    (match (string-ref cond-str 1)
      [#\< <]
      [#\> >]))
  (define idx (string->number (substring cond-str 2)))
  (condition cat cmp idx))

; function which parses a consequence
(define (parse-consequence conseq)
  (match conseq
    ["A" 'accept]
    ["R" 'reject]
    [x x]))

; function which parses an uncond-rule
(define (parse-uncond-rule rule-str)
  (uncond-rule (parse-consequence rule-str)))

; function which parses a cond-rule
(define (parse-cond-rule rule-str)
  (match-define (list cond consq) (string-split rule-str ":"))
  (cond-rule (parse-condition cond) (parse-consequence consq)))

; function which parses a rule
(define (parse-rule rule-str)
  (if (string-contains? rule-str ":")
      (parse-cond-rule rule-str)
      (parse-uncond-rule rule-str)))

; function which parses a workflow
(define (parse-workflow work-str)
  (define groups (regexp-match #px"(\\w+)\\{([^}]+)\\}" work-str))
  (define rules (map parse-rule (string-split (third groups) ",")))
  (workflow (second groups) rules))

; function which parses a part
(define (parse-part part-str)
  (define nums (map string->number (regexp-match* #px"\\d+" part-str)))
  (part (first nums) (second nums) (third nums) (fourth nums)))

; function which splits string into lines
(define (split-lines str)
  (string-split str "\n"))

; function which parses an input string
(define (parse-input-string str)
  (define groups (string-split str "\n\n"))
  (define workflows (map parse-workflow (split-lines (first groups))))
  (define parts (map parse-part (split-lines (second groups))))
  (list workflows parts))

; function which tests a part against a condition
(define (test-cond cnd part)
  (match-define (condition cat cmp idx) cnd)
  (cond [(equal? cat #\x) (cmp (part-x part) idx)]
        [(equal? cat #\m) (cmp (part-m part) idx)]
        [(equal? cat #\a) (cmp (part-a part) idx)]
        [(equal? cat #\s) (cmp (part-s part) idx)]))

; function which takes a part and a rule and returns a consequence
(define (apply-rule rule part)
  (cond [(uncond-rule? rule) (uncond-rule-consequence rule)]
        [(cond-rule? rule)
         (define condition (cond-rule-condition rule))
         (if (test-cond condition part)
             (cond-rule-consequence rule)
             'next)]))

; function which takes a part and a workflow and returns a consequence
(define (apply-workflow work part)
  (define (iter rules)
    (define consequence (apply-rule (first rules) part))
    (if (equal? consequence 'next)
        (iter (rest rules))
        consequence))
  (iter (workflow-rules work)))

; function which builds a hash of all workflows
(define (workflow-hash works)
  (define vals (flatten (map (λ (w) (list (workflow-name w) w)) works)))
  (apply hash vals))

; function which determines a part's fate in a workflow context
(define (part-fate work-hash part)
  (define (iter wf)
    (match (apply-workflow wf part)
      ['accept 'accept]
      ['reject 'reject]
      [x (iter (hash-ref work-hash x))]))
  (iter (hash-ref work-hash "in")))

; function which calculates a part's rating number
(define (part-rating p)
  (match-define (part x m a s) p)
  (+ x m a s))

; part 1
(define (part1 file)
  (match-define (list workflows parts) (parse-input-string (file->string file #:mode 'text)))
  (define work-hash (workflow-hash workflows))
  (define accepted (filter (λ (p) (equal? (part-fate work-hash p) 'accept)) parts))
  (apply + (map part-rating accepted)))

; part 2

; the plan:
; start with ranges [0, 4000]*4 at "in"
; every time there is a cond-rule, split the range into the two cases
; accept = return whole range
; reject = return empty range
; valid values = union of given ranges
; while going down, the ranges will be single
; but going up, there are going to be multiple

; function which takes a range, a comparison and an index and filters according
; to the comparison and index
(define (apply-range-condition range cmp idx)
  (filter (curryr cmp idx) range))

; function which takes a range, a comparison and an index and splits the range
; into a sequence which satisfies the comparison/idx combo and a sequence which doesn't
(define (split-range-condition range cmp idx)
  (define opposite (if (equal? cmp >) <= >=))
  (list (apply-range-condition range cmp idx)
        (apply-range-condition range opposite idx)))

(struct ranges (x m a s) #:transparent)
; Ranges

; empty range
(define empty-range (ranges empty empty empty empty))

; function which takes a Ranges and returns possible combinations
(define (ranges-combos rngs)
  (* (length (ranges-x rngs))
     (length (ranges-m rngs))
     (length (ranges-a rngs))
     (length (ranges-s rngs))))

; function which takes a Ranges and a condition and splits the universe into two
(define (split-ranges-condition rngs cnd)
  (match-define (ranges x m a s) rngs)
  (match-define (condition cat cmp idx) cnd)
  (match cat
    [#\x
     (match-define (list yas nas) (split-range-condition x cmp idx))
     (list (ranges yas m a s) (ranges nas m a s))]
    [#\m
     (match-define (list yas nas) (split-range-condition m cmp idx))
     (list (ranges x yas a s) (ranges x nas a s))]
    [#\a
     (match-define (list yas nas) (split-range-condition a cmp idx))
     (list (ranges x m yas s) (ranges x m nas s))]
    [#\s
     (match-define (list yas nas) (split-range-condition s cmp idx))
     (list (ranges x m a yas) (ranges x m a nas))]))

; function which applies a rule to a Ranges and returns a list of lists (ranges, consequence)
(define (apply-rule-ranges rule rngs)
  (cond [(uncond-rule? rule)
         (define consq (uncond-rule-consequence rule))
         (match consq
           ['accept (list (list 'accept rngs))]
           ['reject (list (list 'reject rngs))]
           [str (list (list str rngs))])]
        [(cond-rule? rule)
         (define condition (cond-rule-condition rule))
         (match-define (list yas nas) (split-ranges-condition rngs condition))
         (list (list (cond-rule-consequence rule) yas)
               (list 'next nas))]))

; function which takes a ranges and a workflow and returns a list of lists (ranges, consequence)
(define (apply-workflow-ranges work rngs)
  ; basically call apply-rule-ranges until
  ; there are no ranges with consequence 'next left
  (define rules (workflow-rules work))

  (struct witem (rngs consq) #:transparent)
  ; blah
  
  (define workspace (list (witem rngs 'next)))
  ; workspace : (list Witem)

  (define (evolve-workspace workspace new-workspace rules)
    (cond
      [(empty? rules) workspace]
      [(empty? workspace) (evolve-workspace new-workspace empty (rest rules))]
      [else
       (define rule (first rules))
       (define this-witem (first workspace))
       (match-define (witem rngs consq) this-witem)
       (cond
         [(equal? consq 'next)
          (define results (apply-rule-ranges rule rngs))
          (define new-witems (map (λ (result)
                                    (witem (second result) (first result)))
                                  results))
          (evolve-workspace (rest workspace) (append new-workspace new-witems) rules)]
         [else (evolve-workspace (rest workspace) (append new-workspace (list this-witem)) rules)])]))
  (map (λ (w) (list (witem-rngs w) (witem-consq w))) (evolve-workspace workspace empty rules)))

; function which takes a ranges and decides their fate, returning a list of accepted ranges
(define (decide-ranges-fate work-hash rngs)
  (define (iter pair-lst result-lst)
    (cond [(empty? pair-lst) result-lst]
          [else
           (match-define (list rngs consq) (first pair-lst))
           (match consq
             ['accept (iter (rest pair-lst) (cons rngs result-lst))]
             ['reject (iter (rest pair-lst) result-lst)]
             [s
              (define ans (apply-workflow-ranges (hash-ref work-hash consq) rngs))
              (iter (append (rest pair-lst) ans) result-lst)])]))
  (iter (list (list rngs "in")) empty))

; part 2 (for real)
(define (part2 file)
  (match-define (list workflows parts) (parse-input-string (file->string file #:mode 'text)))
  (define work-hash (workflow-hash workflows))
  (define accepted (decide-ranges-fate work-hash (ranges (range 1 4001) (range 1 4001) (range 1 4001) (range 1 4001))))
  (apply + (map ranges-combos accepted)))
