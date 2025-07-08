;;; ClaudeScope - Self-Verifying Network Analyzer
;;; Main application module that orchestrates all components

(require "types.cl")
(require "topology.cl")
(require "contracts.cl")
(require "capture.cl")
(require "firewall.cl")
(require "replay.cl")
(require "explain.cl")

;; Application state
(define *app-state* (atom 'initialized))
(define *app-config* (atom (make-hash-table)))

;; Application modes
(define-type AppMode
  (CaptureMode (config CaptureConfig))
  (AnalysisMode (focus string))  ; topology, contracts, firewall
  (ReplayMode (session-id string))
  (ExplainMode (interactive bool)))

;; Current application mode
(define *current-mode* (atom (AnalysisMode "topology")))

;; Initialize ClaudeScope
(spec:contract init-claudescope
  :modifies [*app-state* *app-config*]
  :ensures [(eq? @*app-state* 'running)])
(define (init-claudescope)
  (println "=== ClaudeScope Network Analyzer v1.0 ===")
  (println "Self-verifying network analysis with formal contracts")
  (println "")
  
  ;; Initialize configuration
  (reset! *app-config*
          (make-hash-table
            `((capture-interface . "eth0")
              (max-packets . 10000)
              (contract-checking . #t)
              (auto-explain . #t)
              (topology-update-interval . 5000))))
  
  ;; Initialize components
  (println "Initializing components...")
  (init-explanation-templates)
  
  ;; Set state to running
  (reset! *app-state* 'running)
  (println "ClaudeScope ready."))

;; Start packet capture
(spec:contract claudescope-start-capture
  :requires [(eq? @*app-state* 'running)]
  :modifies [*current-mode*]
  :ensures [@*capture-active*])
(define (claudescope-start-capture)
  (let ([config (Config 
                  (hash-ref @*app-config* 'capture-interface)
                  #t  ; promiscuous mode
                  (hash-ref @*app-config* 'max-packets)
                  ""  ; no filter initially
                  1000)])
    (reset! *current-mode* (CaptureMode config))
    (start-capture config)
    (println "Packet capture started...")
    (println "Generating synthetic network traffic for demo...")))

;; Analyze network topology
(spec:contract analyze-network-topology
  :ensures [(hash-table? result)]
  :pure true)
(define (analyze-network-topology)
  (let* ([graph @*topology*]
         [nodes (Graph-nodes graph)]
         [edges (Graph-edges graph)]
         [segments (find-isolated-segments)]
         [bottlenecks (find-bottlenecks)])
    (make-hash-table
      `((total-devices . ,(length nodes))
        (total-connections . ,(length edges))
        (network-segments . ,(length segments))
        (bottlenecks . ,(length bottlenecks))
        (last-updated . ,(Graph-last-updated graph))))))

;; Get security compliance summary
(spec:contract get-compliance-summary
  :ensures [(hash-table? result)]
  :pure true)
(define (get-compliance-summary)
  (let* ([violations (get-recent-violations 3600)]  ; Last hour
         [by-severity (group-violations-by-severity violations)]
         [coverage (contract-coverage-report)])
    (make-hash-table
      `((total-violations . ,(length violations))
        (critical-violations . ,(or (assoc-ref by-severity 'critical) 0))
        (high-violations . ,(or (assoc-ref by-severity 'high) 0))
        (contract-coverage . ,coverage)
        (compliance-score . ,(calculate-compliance-score violations))))))

;; Calculate compliance score (0-100)
(define (calculate-compliance-score violations)
  (let ([stats @*contract-stats*])
    (if (empty? stats)
        100
        (let* ([totals (map (lambda (s) (first (cdr s))) 
                           (hash->list stats))]
               [violations-counts (map (lambda (s) (second (cdr s)))
                                     (hash->list stats))]
               [total-checks (fold + 0 totals)]
               [total-violations (fold + 0 violations-counts)])
          (if (= total-checks 0)
              100
              (truncate (* 100 (- 1 (/ total-violations total-checks)))))))))

;; Optimize firewall configuration
(spec:contract optimize-firewall-config
  :requires [(Firewall? firewall)]
  :ensures [(Firewall? result)]
  :pure true)
(define (optimize-firewall-config firewall)
  (let* ([original-rules (Firewall-rules firewall)]
         [conflicts (analyze-firewall-rules original-rules)]
         [optimized-rules (optimize-firewall-rules original-rules)]
         [report (generate-optimization-report 
                   original-rules optimized-rules conflicts)])
    (println report)
    (Firewall (Firewall-interfaces firewall) optimized-rules)))

;; Create and start replay session
(spec:contract create-traffic-replay
  :requires [(int? duration-seconds)]
  :ensures [(string? result)])
(define (create-traffic-replay duration-seconds)
  (let* ([recent-packets (get-recent-packets 1000)]
         [session-id (create-replay-session recent-packets 1.0 #f "")])
    (println (format "Created replay session: ~a" session-id))
    (println (format "Replaying ~a packets..." (length recent-packets)))
    (start-replay session-id)
    session-id))

;; Interactive violation explanation
(spec:contract explain-recent-violations
  :ensures [(list? result)])
(define (explain-recent-violations)
  (let ([violations (get-recent-violations 300)])  ; Last 5 minutes
    (if (empty? violations)
        (begin
          (println "No recent violations to explain.")
          '())
        (begin
          (println (format "Found ~a recent violations:" (length violations)))
          (println "")
          (map (lambda (v)
                 (let ([explanation (explain-violation-detailed v)])
                   (print-explanation explanation)
                   explanation))
               (take violations 5))))))  ; Show top 5

;; Print explanation to console
(define (print-explanation explanation)
  (match explanation
    [(SimpleExplanation violation summary details severity recommendations)
     (println (format "=== ~a ===" summary))
     (println (format "Severity: ~a" severity))
     (println "")
     (println "Details:")
     (for-each (lambda (d) (println (format "  - ~a" d))) details)
     (println "")
     (println "Recommendations:")
     (for-each (lambda (r) (println (format "  • ~a" r))) recommendations)
     (println "")]
    [(DetailedExplanation violation summary tech-details impact remediation refs)
     (println (format "=== ~a ===" summary))
     (println "")
     (println "Technical Details:")
     (for-each (lambda (d) (println (format "  - ~a" d))) tech-details)
     (println "")
     (println (format "Business Impact: ~a" impact))
     (println "")
     (println "Remediation Steps:")
     (for-each (lambda (r) (println (format "  ~a" r))) remediation)
     (println "")]))

;; Generate comprehensive report
(spec:contract generate-network-report
  :ensures [(string? result)]
  :pure true)
(define (generate-network-report)
  (let* ([topo-analysis (analyze-network-topology)]
         [compliance (get-compliance-summary)]
         [capture-stats (get-capture-stats)])
    (string-append
      "=== ClaudeScope Network Analysis Report ===\n"
      (format "Generated: ~a\n\n" (timestamp-to-string (current-timestamp)))
      
      "Network Topology:\n"
      (format "  Total devices: ~a\n" (hash-ref topo-analysis 'total-devices))
      (format "  Active connections: ~a\n" (hash-ref topo-analysis 'total-connections))
      (format "  Network segments: ~a\n" (hash-ref topo-analysis 'network-segments))
      (format "  Bottlenecks detected: ~a\n\n" (hash-ref topo-analysis 'bottlenecks))
      
      "Security Compliance:\n"
      (format "  Compliance score: ~a%\n" (hash-ref compliance 'compliance-score))
      (format "  Total violations: ~a\n" (hash-ref compliance 'total-violations))
      (format "  Critical violations: ~a\n" (hash-ref compliance 'critical-violations))
      (format "  High severity violations: ~a\n\n" (hash-ref compliance 'high-violations))
      
      "Traffic Statistics:\n"
      (format "  Total packets captured: ~a\n" @*packet-counter*)
      (format "  Protocol breakdown:\n")
      (string-join
        (map (lambda (pair)
               (format "    ~a: ~a packets" (car pair) (cdr pair)))
             (hash->list capture-stats))
        "\n")
      "\n\n"
      
      "Recommendations:\n"
      (generate-recommendations topo-analysis compliance))))

;; Generate actionable recommendations
(define (generate-recommendations topo compliance)
  (let ([score (hash-ref compliance 'compliance-score)]
        [bottlenecks (hash-ref topo 'bottlenecks)]
        [violations (hash-ref compliance 'total-violations)])
    (string-append
      (if (< score 80)
          "  - URGENT: Compliance score below 80%. Review security policies immediately.\n"
          "")
      (if (> bottlenecks 0)
          "  - Network bottlenecks detected. Consider upgrading network infrastructure.\n"
          "")
      (if (> violations 50)
          "  - High violation count. Implement automated remediation workflows.\n"
          "")
      (if (= (hash-ref topo 'network-segments) 1)
          "  - Single network segment detected. Consider network segmentation for security.\n"
          "")
      "  - Enable continuous monitoring and alerting for critical violations.\n"
      "  - Schedule regular security audits and penetration testing.\n")))

;; Demo workflow
(spec:contract run-demo-workflow
  :modifies [*app-state* *current-mode*]
  :ensures [(eq? @*app-state* 'running)])
(define (run-demo-workflow)
  (println "\n=== Running ClaudeScope Demo Workflow ===\n")
  
  ;; Step 1: Initialize
  (println "Step 1: Initializing ClaudeScope...")
  (init-claudescope)
  (sleep 1000)
  
  ;; Step 2: Start capture
  (println "\nStep 2: Starting packet capture...")
  (claudescope-start-capture)
  (sleep 2000)
  
  ;; Step 3: Analyze topology
  (println "\nStep 3: Analyzing network topology...")
  (let ([topo (analyze-network-topology)])
    (println (format "Discovered ~a devices and ~a connections"
                    (hash-ref topo 'total-devices)
                    (hash-ref topo 'total-connections))))
  (sleep 1000)
  
  ;; Step 4: Check compliance
  (println "\nStep 4: Checking security compliance...")
  (let ([compliance (get-compliance-summary)])
    (println (format "Compliance score: ~a%"
                    (hash-ref compliance 'compliance-score)))
    (println (format "Violations detected: ~a"
                    (hash-ref compliance 'total-violations))))
  (sleep 1000)
  
  ;; Step 5: Explain violations
  (println "\nStep 5: Explaining recent violations...")
  (explain-recent-violations)
  (sleep 1000)
  
  ;; Step 6: Generate report
  (println "\nStep 6: Generating comprehensive report...")
  (let ([report (generate-network-report)])
    (println report))
  
  (println "\n=== Demo Complete ===")
  (println "ClaudeScope is ready for interactive use."))

;; Helper functions
(define (println str)
  (display str)
  (newline))

(define (sleep ms)
  ;; Mock sleep function
  #t)

(define (timestamp-to-string ts)
  (format "~a" ts))

(define (assoc-ref alist key)
  (let ([pair (assoc key alist)])
    (if pair (cdr pair) #f)))

(define (empty? lst)
  (null? lst))

(define (truncate x)
  (if (int? x) x (floor x)))

(define (floor x)
  ;; Mock floor function
  (- x (mod x 1)))

;; Export main ClaudeScope functions
(provide init-claudescope
         claudescope-start-capture
         analyze-network-topology
         get-compliance-summary
         optimize-firewall-config
         create-traffic-replay
         explain-recent-violations
         generate-network-report
         run-demo-workflow
         *app-state* *current-mode*)