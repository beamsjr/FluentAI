;;; Traffic Replay and Analysis
;;; Replay captured traffic for testing and analysis

(require "types.cl")
(require "capture.cl")
(require "contracts.cl")

;; Replay session state
(define-type ReplaySession
  (Session
    (id string)
    (packets (list Packet))
    (speed-factor float)     ; 1.0 = realtime, 2.0 = 2x speed
    (loop bool)
    (filter string)
    (start-time int)
    (current-index int)))

;; Replay state management
(define *active-sessions* (atom (make-hash-table)))
(define *replay-stats* (atom (make-hash-table)))

;; Contract: Replay speed must be positive
(spec:contract valid-replay-speed?
  :requires [(float? speed)]
  :ensures [(> speed 0.0)]
  :pure true)

;; Create a new replay session
(spec:contract create-replay-session
  :requires [(and (list? packets) 
                  (forall ((p (in packets))) (Packet? p))
                  (valid-replay-speed? speed))]
  :modifies [*active-sessions*]
  :ensures [(string? result)])
(define (create-replay-session packets speed loop filter)
  (let* ([id (generate-session-id)]
         [filtered-packets (if (string-empty? filter)
                              packets
                              (apply-packet-filter filter packets))]
         [session (Session id filtered-packets speed loop filter
                          (current-timestamp) 0)])
    (swap! *active-sessions*
           (lambda (sessions)
             (hash-set sessions id session)))
    id))

;; Start replay session
(spec:contract start-replay
  :requires [(string? session-id)]
  :modifies [*replay-stats*]
  :ensures [(bool? result)])
(define (start-replay session-id)
  (let ([session (hash-ref @*active-sessions* session-id)])
    (if session
        (begin
          (reset-replay-stats session-id)
          (replay-packets session)
          #t)
        #f)))

;; Replay packets from session
(define (replay-packets session)
  (let ([packets (Session-packets session)]
        [speed (Session-speed-factor session)]
        [start-index (Session-current-index session)])
    ;; In real implementation, would use async/timer
    ;; For demo, we'll process synchronously
    (replay-packet-sequence packets start-index speed session)))

;; Replay a sequence of packets
(define (replay-packet-sequence packets index speed session)
  (when (< index (length packets))
    (let* ([packet (nth packets index)]
           [next-index (+ index 1)])
      ;; Process packet
      (process-replay-packet packet (Session-id session))
      ;; Update session index
      (update-session-index (Session-id session) next-index)
      ;; Handle looping
      (when (and (>= next-index (length packets))
                 (Session-loop session))
        (update-session-index (Session-id session) 0)
        (replay-packet-sequence packets 0 speed session)))))

;; Process a replayed packet
(define (process-replay-packet packet session-id)
  ;; Update topology
  (update-topology-from-packet packet)
  ;; Verify contracts
  (let ([results (verify-packet packet)])
    (update-replay-stats session-id packet results)))

;; Update replay statistics
(define (update-replay-stats session-id packet results)
  (swap! *replay-stats*
         (lambda (stats)
           (let* ([session-stats (or (hash-ref stats session-id)
                                    (make-empty-stats))]
                  [updated (update-stats-with-packet session-stats 
                                                    packet results)])
             (hash-set stats session-id updated)))))

;; Create empty statistics record
(define (make-empty-stats)
  (list 0 0 0 0))  ; total, passed, violations, errors

;; Update statistics with packet results
(define (update-stats-with-packet stats packet results)
  (let* ([total (first stats)]
         [passed (second stats)]
         [violations (third stats)]
         [errors (fourth stats)]
         [violation-count (length (filter Violation? results))]
         [pass-count (length (filter Pass? results))])
    (list (+ total 1)
          (+ passed pass-count)
          (+ violations violation-count)
          errors)))

;; Analyze replay results
(spec:contract analyze-replay-session
  :requires [(string? session-id)]
  :ensures [(or (hash-table? result) (bool? result))]
  :pure true)
(define (analyze-replay-session session-id)
  (let* ([session (hash-ref @*active-sessions* session-id)]
         [stats (hash-ref @*replay-stats* session-id)])
    (if (and session stats)
        (create-analysis-report session stats)
        #f)))

;; Create analysis report
(define (create-analysis-report session stats)
  (let* ([total (first stats)]
         [passed (second stats)]
         [violations (third stats)]
         [compliance-rate (if (> total 0)
                             (/ passed total)
                             1.0)])
    (make-hash-table
      `((session-id . ,(Session-id session))
        (total-packets . ,total)
        (passed . ,passed)
        (violations . ,violations)
        (compliance-rate . ,compliance-rate)
        (filter . ,(Session-filter session))
        (speed-factor . ,(Session-speed-factor session))))))

;; Compare replay sessions
(spec:contract compare-replay-sessions
  :requires [(and (string? session1) (string? session2))]
  :ensures [(or (string? result) (bool? result))]
  :pure true)
(define (compare-replay-sessions session1-id session2-id)
  (let ([report1 (analyze-replay-session session1-id)]
        [report2 (analyze-replay-session session2-id)])
    (if (and report1 report2)
        (generate-comparison-report report1 report2)
        #f)))

;; Generate comparison report
(define (generate-comparison-report report1 report2)
  (string-append
    "=== Replay Session Comparison ===\n\n"
    (format "Session 1: ~a\n" (hash-ref report1 'session-id))
    (format "  Total packets: ~a\n" (hash-ref report1 'total-packets))
    (format "  Compliance rate: ~a%\n" 
            (* 100 (hash-ref report1 'compliance-rate)))
    (format "  Violations: ~a\n\n" (hash-ref report1 'violations))
    (format "Session 2: ~a\n" (hash-ref report2 'session-id))
    (format "  Total packets: ~a\n" (hash-ref report2 'total-packets))
    (format "  Compliance rate: ~a%\n"
            (* 100 (hash-ref report2 'compliance-rate)))
    (format "  Violations: ~a\n\n" (hash-ref report2 'violations))
    "=== Analysis ===\n"
    (cond
      [(> (hash-ref report1 'compliance-rate)
          (hash-ref report2 'compliance-rate))
       "Session 1 shows better compliance with security policies."]
      [(< (hash-ref report1 'compliance-rate)
          (hash-ref report2 'compliance-rate))
       "Session 2 shows better compliance with security policies."]
      [else
       "Both sessions show similar compliance rates."])))

;; Export replay session for sharing
(spec:contract export-replay-session
  :requires [(string? session-id)]
  :ensures [(or (string? result) (bool? result))]
  :pure true)
(define (export-replay-session session-id)
  (let ([session (hash-ref @*active-sessions* session-id)])
    (if session
        (session-to-pcap-format session)
        #f)))

;; Convert session to PCAP-like format
(define (session-to-pcap-format session)
  ;; Simplified export format
  (string-append
    "ClaudeScope Replay Session v1.0\n"
    (format "ID: ~a\n" (Session-id session))
    (format "Packets: ~a\n" (length (Session-packets session)))
    (format "Filter: ~a\n" (Session-filter session))
    (format "Speed: ~ax\n" (Session-speed-factor session))
    "\n--- Packet Data ---\n"
    (string-join
      (map packet-to-export-format (Session-packets session))
      "\n")))

;; Convert packet to export format
(define (packet-to-export-format packet)
  (format "~a ~a->~a ~a"
          (packet-protocol packet)
          (ip-to-string (packet-src-ip packet))
          (ip-to-string (packet-dst-ip packet))
          (classify-packet packet)))

;; Helper functions
(define (generate-session-id)
  (format "replay-~a" (current-timestamp)))

(define (reset-replay-stats session-id)
  (swap! *replay-stats*
         (lambda (stats)
           (hash-set stats session-id (make-empty-stats)))))

(define (update-session-index session-id new-index)
  (swap! *active-sessions*
         (lambda (sessions)
           (let ([session (hash-ref sessions session-id)])
             (if session
                 (hash-set sessions session-id
                          (Session (Session-id session)
                                  (Session-packets session)
                                  (Session-speed-factor session)
                                  (Session-loop session)
                                  (Session-filter session)
                                  (Session-start-time session)
                                  new-index))
                 sessions)))))

(define (string-empty? s)
  (= (string-length s) 0))

;; Export replay functions
(provide ReplaySession Session Session?
         create-replay-session start-replay
         analyze-replay-session compare-replay-sessions
         export-replay-session
         *active-sessions* *replay-stats*)