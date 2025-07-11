;;; Firewall Rule Optimization and Analysis
;;; Optimize firewall rules and detect conflicts/redundancies

(require "types.cl")
(require "topology.cl")

;; Firewall analysis results
(define-type RuleConflict
  (Shadowed
    (rule FirewallRule)
    (by FirewallRule)
    (reason string))
  (Redundant
    (rule1 FirewallRule)
    (rule2 FirewallRule)
    (reason string))
  (Inefficient
    (rule FirewallRule)
    (suggestion string)))

;; Rule optimization state
(define *rule-stats* (atom (make-hash-table)))
(define *optimization-cache* (atom (make-hash-table)))

;; Contract: Firewall rules maintain priority ordering
(spec:contract rules-priority-ordered?
  :requires [(list? rules)]
  :ensures [(forall ((i (in (range 0 (- (length rules) 1))))
                    (let ([r1 (nth rules i)]
                          [r2 (nth rules (+ i 1))])
                      (<= (Rule-priority r1) (Rule-priority r2))))]
  :pure true)

;; Analyze firewall rules for conflicts
(spec:contract analyze-firewall-rules
  :requires [(and (list? rules) (forall ((r (in rules))) (Rule? r)))]
  :ensures [(list? result)]
  :pure true)
(define (analyze-firewall-rules rules)
  (let ([conflicts '()]
        [sorted-rules (sort-rules-by-priority rules)])
    ;; Check for shadowed rules
    (for-each-indexed
      (lambda (i rule)
        (let ([shadows (find-shadowing-rules rule 
                         (take sorted-rules i))])
          (when (not (null? shadows))
            (set! conflicts 
                  (append conflicts
                          (map (lambda (s) 
                                 (Shadowed rule s 
                                          "Earlier rule matches all packets"))
                               shadows))))))
      sorted-rules)
    ;; Check for redundant rules
    (set! conflicts (append conflicts (find-redundant-rules sorted-rules)))
    ;; Check for inefficient rules
    (set! conflicts (append conflicts (find-inefficient-rules sorted-rules)))
    conflicts))

;; Sort rules by priority
(define (sort-rules-by-priority rules)
  (sort rules (lambda (a b) (< (Rule-priority a) (Rule-priority b)))))

;; Find rules that shadow the given rule
(define (find-shadowing-rules rule earlier-rules)
  (filter (lambda (r) (rule-shadows? r rule)) earlier-rules))

;; Check if rule1 shadows rule2
(spec:contract rule-shadows?
  :requires [(and (Rule? rule1) (Rule? rule2))]
  :ensures [(bool? result)]
  :pure true)
(define (rule-shadows? rule1 rule2)
  (and (rule-covers? rule1 rule2)
       (not (equal? rule1 rule2))))

;; Check if rule1 covers all packets that rule2 would match
(define (rule-covers? rule1 rule2)
  (and (protocol-covers? (Rule-protocol rule1) (Rule-protocol rule2))
       (ip-range-covers? (Rule-src-addr rule1) (Rule-src-mask rule1)
                        (Rule-src-addr rule2) (Rule-src-mask rule2))
       (ip-range-covers? (Rule-dst-addr rule1) (Rule-dst-mask rule1)
                        (Rule-dst-addr rule2) (Rule-dst-mask rule2))
       (port-range-covers? (Rule-src-ports rule1) (Rule-src-ports rule2))
       (port-range-covers? (Rule-dst-ports rule1) (Rule-dst-ports rule2))))

;; Check protocol coverage
(define (protocol-covers? proto1 proto2)
  (or (eq? proto1 'any)
      (eq? proto1 proto2)))

;; Check IP range coverage
(define (ip-range-covers? addr1 mask1 addr2 mask2)
  ;; Simplified: check if range1 contains range2
  ;; In practice, would do proper subnet calculations
  (or (and (equal? addr1 addr2) (equal? mask1 mask2))
      (is-subnet-of? addr2 mask2 addr1 mask1)))

;; Check if subnet2 is contained in subnet1
(define (is-subnet-of? addr2 mask2 addr1 mask1)
  ;; Mock implementation - would need proper subnet math
  #f)

;; Check port range coverage
(define (port-range-covers? ports1 ports2)
  (or (null? ports1)  ; Empty means all ports
      (and (not (null? ports2))
           (subset? ports2 ports1))))

;; Find redundant rules
(define (find-redundant-rules rules)
  (let ([redundancies '()])
    (for-each-indexed
      (lambda (i rule1)
        (for-each-indexed
          (lambda (j rule2)
            (when (and (> j i)
                       (rules-redundant? rule1 rule2))
              (set! redundancies
                    (cons (Redundant rule1 rule2 
                                    "Rules have same effect")
                          redundancies))))
          rules))
      rules)
    redundancies))

;; Check if two rules are redundant
(define (rules-redundant? rule1 rule2)
  (and (eq? (Rule-action rule1) (Rule-action rule2))
       (rule-covers? rule1 rule2)
       (rule-covers? rule2 rule1)))

;; Find inefficient rules
(define (find-inefficient-rules rules)
  (filter-map
    (lambda (rule)
      (let ([suggestion (suggest-optimization rule)])
        (if suggestion
            (Inefficient rule suggestion)
            #f)))
    rules))

;; Suggest rule optimization
(define (suggest-optimization rule)
  (cond
    ;; Suggest using 'any' for all protocols
    [(and (member? (Rule-protocol rule) '(tcp udp icmp))
          (null? (Rule-src-ports rule))
          (null? (Rule-dst-ports rule)))
     "Consider using protocol 'any' for broader coverage"]
    ;; Suggest consolidating port ranges
    [(> (length (Rule-dst-ports rule)) 10)
     "Consider using port ranges instead of individual ports"]
    ;; Suggest network aggregation
    [(can-aggregate-networks? rule)
     "Adjacent networks can be aggregated"]
    [else #f]))

;; Check if networks can be aggregated
(define (can-aggregate-networks? rule)
  ;; Mock implementation
  #f)

;; Optimize firewall ruleset
(spec:contract optimize-firewall-rules
  :requires [(list? rules)]
  :ensures [(and (list? result)
                 (<= (length result) (length rules)))]
  :pure true)
(define (optimize-firewall-rules rules)
  (let* ([conflicts (analyze-firewall-rules rules)]
         [shadowed-rules (filter-map
                          (lambda (c)
                            (match c
                              [(Shadowed rule _ _) rule]
                              [_ #f]))
                          conflicts)]
         [redundant-rules (filter-map
                           (lambda (c)
                             (match c
                               [(Redundant _ rule2 _) rule2]
                               [_ #f]))
                           conflicts)]
         [rules-to-remove (append shadowed-rules redundant-rules)])
    ;; Remove problematic rules
    (filter (lambda (r) (not (member? r rules-to-remove))) rules)))

;; Generate optimized ruleset report
(spec:contract generate-optimization-report
  :requires [(and (list? original) (list? optimized) (list? conflicts))]
  :ensures [(string? result)]
  :pure true)
(define (generate-optimization-report original optimized conflicts)
  (string-append
    "=== Firewall Optimization Report ===\n\n"
    (format "Original rules: ~a\n" (length original))
    (format "Optimized rules: ~a\n" (length optimized))
    (format "Rules removed: ~a\n" (- (length original) (length optimized)))
    "\n=== Conflicts Found ===\n\n"
    (string-join
      (map (lambda (c)
             (match c
               [(Shadowed rule by reason)
                (format "SHADOWED: Rule ~a shadowed by ~a\n  Reason: ~a"
                        (rule-to-string rule)
                        (rule-to-string by)
                        reason)]
               [(Redundant rule1 rule2 reason)
                (format "REDUNDANT: Rules ~a and ~a are redundant\n  Reason: ~a"
                        (rule-to-string rule1)
                        (rule-to-string rule2)
                        reason)]
               [(Inefficient rule suggestion)
                (format "INEFFICIENT: Rule ~a\n  Suggestion: ~a"
                        (rule-to-string rule)
                        suggestion)]))
           conflicts)
      "\n\n")
    "\n\n=== Recommendations ===\n"
    (cond
      [(> (length conflicts) 10)
       "- Consider comprehensive ruleset review\n- Implement rule grouping by service"]
      [(> (length conflicts) 5)
       "- Review and consolidate similar rules\n- Consider using object groups"]
      [else
       "- Ruleset is relatively well-optimized\n- Monitor rule hit counts for further optimization"])))

;; Convert rule to string representation
(define (rule-to-string rule)
  (format "~a ~a ~a->~a"
          (Rule-action rule)
          (Rule-protocol rule)
          (ip-to-string (Rule-src-addr rule))
          (ip-to-string (Rule-dst-addr rule))))

;; Helper functions
(define (for-each-indexed fn lst)
  (let loop ([i 0] [lst lst])
    (when (not (null? lst))
      (fn i (car lst))
      (loop (+ i 1) (cdr lst)))))

(define (filter-map fn lst)
  (filter (lambda (x) x)
          (map fn lst)))

(define (subset? set1 set2)
  (forall ((x (in set1)))
          (member? x set2)))

(define (nth lst n)
  (if (= n 0)
      (car lst)
      (nth (cdr lst) (- n 1))))

(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

;; String manipulation helpers
(define (string-append . args)
  (apply string-concat args))

(define (string-join lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-concat (car lst) sep (string-join (cdr lst) sep)))))

(define (format fmt . args)
  ;; Simplified format - in practice would do proper formatting
  (string-concat fmt " " (string-concat-all args)))

(define (string-concat-all lst)
  (if (null? lst)
      ""
      (string-concat (to-string (car lst)) 
                     (string-concat-all (cdr lst)))))

(define (to-string x)
  (cond
    [(string? x) x]
    [(int? x) (int->string x)]
    [(symbol? x) (symbol->string x)]
    [else "???"]))

;; Export firewall optimization functions
(provide RuleConflict Shadowed Redundant Inefficient
         analyze-firewall-rules optimize-firewall-rules
         generate-optimization-report
         rule-shadows? rules-redundant?)