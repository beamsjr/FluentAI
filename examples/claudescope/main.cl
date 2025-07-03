;;; ClaudeScope Demo Application
;;; Entry point for the self-verifying network analyzer

(require "src/claudescope.cl")

;; Main entry point
(define (main args)
  (println "")
  (println "╔══════════════════════════════════════════════╗")
  (println "║       ClaudeScope Network Analyzer           ║")
  (println "║   Self-Verifying Security with ClaudeLang   ║")
  (println "╚══════════════════════════════════════════════╝")
  (println "")
  
  ;; Check command line arguments
  (cond
    [(null? args)
     ;; No arguments - run interactive demo
     (run-demo-workflow)]
    
    [(equal? (car args) "capture")
     ;; Start packet capture mode
     (init-claudescope)
     (claudescope-start-capture)
     (println "Capture mode active. Press Ctrl+C to stop.")]
    
    [(equal? (car args) "analyze")
     ;; Analyze existing data
     (init-claudescope)
     (println "Analyzing network topology...")
     (let ([topo (analyze-network-topology)])
       (print-analysis-results topo))
     (println "\nChecking security compliance...")
     (let ([compliance (get-compliance-summary)])
       (print-compliance-results compliance))]
    
    [(equal? (car args) "explain")
     ;; Explain recent violations
     (init-claudescope)
     (explain-recent-violations)]
    
    [(equal? (car args) "report")
     ;; Generate full report
     (init-claudescope)
     (let ([report (generate-network-report)])
       (println report))]
    
    [(equal? (car args) "help")
     ;; Show help
     (print-help)]
    
    [else
     (println (format "Unknown command: ~a" (car args)))
     (print-help)]))

;; Print analysis results
(define (print-analysis-results topo)
  (println "\n=== Network Topology Analysis ===")
  (println (format "Total devices discovered: ~a" 
                  (hash-ref topo 'total-devices)))
  (println (format "Active connections: ~a"
                  (hash-ref topo 'total-connections)))
  (println (format "Network segments: ~a"
                  (hash-ref topo 'network-segments)))
  (println (format "Bottlenecks found: ~a"
                  (hash-ref topo 'bottlenecks))))

;; Print compliance results
(define (print-compliance-results compliance)
  (let ([score (hash-ref compliance 'compliance-score)]
        [violations (hash-ref compliance 'total-violations)])
    (println "\n=== Security Compliance Summary ===")
    (println (format "Compliance Score: ~a%" score))
    (print-score-bar score)
    (println (format "\nTotal Violations: ~a" violations))
    (println (format "Critical: ~a" 
                    (hash-ref compliance 'critical-violations)))
    (println (format "High: ~a"
                    (hash-ref compliance 'high-violations)))))

;; Print visual score bar
(define (print-score-bar score)
  (let* ([bar-width 40]
         [filled (truncate (* (/ score 100) bar-width))]
         [empty (- bar-width filled)])
    (display "[")
    (display (string-repeat "█" filled))
    (display (string-repeat "░" empty))
    (display "]")))

;; Print help message
(define (print-help)
  (println "\nUsage: claudelang main.cl [command]")
  (println "\nCommands:")
  (println "  (no command)  Run interactive demo workflow")
  (println "  capture       Start packet capture mode")
  (println "  analyze       Analyze network topology and compliance")
  (println "  explain       Explain recent security violations")
  (println "  report        Generate comprehensive report")
  (println "  help          Show this help message")
  (println "\nExamples:")
  (println "  claudelang main.cl")
  (println "  claudelang main.cl capture")
  (println "  claudelang main.cl analyze"))

;; Helper to repeat string
(define (string-repeat str n)
  (if (<= n 0)
      ""
      (string-append str (string-repeat str (- n 1)))))

;; Run main with command line arguments
(main (command-line-args))