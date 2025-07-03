;;; Network Security Contract Verification
;;; Define and verify network security policies using formal contracts

(require "types.cl")
(require "topology.cl")

;; Network security contract definition
(define-type SecurityContract
  (Contract
    (name string)
    (description string)
    (predicate (-> Packet bool))  ; Function type
    (severity symbol)              ; 'critical 'high 'medium 'low
    (enabled bool)))

;; Contract violation tracking
(define *violations* (atom '()))
(define *contract-stats* (atom (make-hash-table)))

;; Built-in security contracts
(define *security-contracts* (atom '()))

;; Register a new security contract
(spec:contract register-contract
  :requires [(and (string? name) (procedure? predicate))]
  :modifies [*security-contracts*]
  :ensures [(> (length @*security-contracts*)
               (old (length @*security-contracts*)))])
(define (register-contract name desc predicate severity)
  (let ([contract (Contract name desc predicate severity #t)])
    (swap! *security-contracts* (lambda (cs) (cons contract cs)))
    contract))

;; Verify packet against all active contracts
(spec:contract verify-packet
  :requires [(Packet? packet)]
  :modifies [*violations* *contract-stats*]
  :ensures [(list? result)])
(define (verify-packet packet)
  (let ([results '()])
    (for-each 
      (lambda (contract)
        (when (Contract-enabled contract)
          (let* ([name (Contract-name contract)]
                 [pred (Contract-predicate contract)]
                 [passed (pred packet)])
            ;; Update statistics
            (update-contract-stats name passed)
            ;; Record result
            (if passed
                (set! results (cons (Pass name packet (current-timestamp)) 
                                   results))
                (begin
                  (record-violation contract packet)
                  (set! results (cons (Violation name packet 
                                                (Contract-description contract)
                                                (current-timestamp))
                                     results)))))))
      @*security-contracts*)
    results))

;; Update contract statistics
(define (update-contract-stats name passed)
  (swap! *contract-stats*
         (lambda (stats)
           (let* ([current (or (hash-ref stats name) '(0 0))]
                  [total (first current)]
                  [violations (second current)])
             (hash-set stats name 
                      (list (+ total 1)
                            (if passed violations (+ violations 1))))))))

;; Record contract violation
(define (record-violation contract packet)
  (let ([violation (Violation (Contract-name contract)
                             packet
                             (Contract-description contract)
                             (current-timestamp))])
    (swap! *violations* (lambda (vs) (cons violation vs)))))

;;; Pre-defined Security Contracts

;; No traffic from guest VLAN to internal services
(register-contract
  "guest-vlan-isolation"
  "Guest VLAN (192.168.100.0/24) should not access internal services"
  (lambda (packet)
    (let ([src-ip (packet-src-ip packet)]
          [dst-ip (packet-dst-ip packet)]
          [guest-subnet (make-ipv4 192 168 100 0)]
          [guest-mask (make-ipv4 255 255 255 0)]
          [internal-subnet (make-ipv4 10 0 0 0)]
          [internal-mask (make-ipv4 255 0 0 0)])
      (not (and (ip-in-subnet? src-ip guest-subnet guest-mask)
                (ip-in-subnet? dst-ip internal-subnet internal-mask)))))
  'critical)

;; All DNS traffic must go to authorized servers
(register-contract
  "dns-server-whitelist"
  "DNS queries must only go to authorized DNS servers"
  (lambda (packet)
    (match packet
      [(UDPPacket _ dst _ dst-port _)
       (if (= dst-port 53)
           (member? dst (list (make-ipv4 8 8 8 8)
                             (make-ipv4 8 8 4 4)
                             (make-ipv4 10 0 0 53)))
           #t)]
      [(TCPPacket _ dst _ dst-port _ _ _ _)
       (if (= dst-port 53)
           (member? dst (list (make-ipv4 8 8 8 8)
                             (make-ipv4 8 8 4 4)
                             (make-ipv4 10 0 0 53)))
           #t)]
      [_ #t]))
  'high)

;; No SSH from external networks
(register-contract
  "ssh-access-control"
  "SSH access only allowed from management network"
  (lambda (packet)
    (match packet
      [(TCPPacket src _ _ dst-port _ _ flags _)
       (if (and (= dst-port 22) (member? 'syn flags))
           (let ([mgmt-subnet (make-ipv4 10 0 100 0)]
                 [mgmt-mask (make-ipv4 255 255 255 0)])
             (ip-in-subnet? src mgmt-subnet mgmt-mask))
           #t)]
      [_ #t]))
  'high)

;; Detect port scanning
(define *connection-attempts* (atom (make-hash-table)))

(register-contract
  "port-scan-detection"
  "Detect potential port scanning activity"
  (lambda (packet)
    (match packet
      [(TCPPacket src dst _ dst-port _ _ flags _)
       (if (member? 'syn flags)
           (let* ([key (format "~a->~a" (ip-to-string src) (ip-to-string dst))]
                  [attempts (or (hash-ref @*connection-attempts* key) '())]
                  [recent-ports (filter (lambda (p) 
                                         (< (- (current-timestamp) (cdr p)) 60000))
                                       attempts)]
                  [unique-ports (unique (map car recent-ports))])
             ;; Update attempts
             (swap! *connection-attempts* 
                    (lambda (cache)
                      (hash-set cache key 
                               (cons (cons dst-port (current-timestamp))
                                    recent-ports))))
             ;; Flag if more than 10 different ports in 60 seconds
             (< (length unique-ports) 10))
           #t)]
      [_ #t]))
  'medium)

;; Contract for encrypted traffic
(register-contract
  "encryption-required"
  "Sensitive services must use encryption"
  (lambda (packet)
    (match packet
      [(TCPPacket _ _ _ dst-port _ _ _ _)
       (not (member? dst-port '(80 21 23 110)))]  ; HTTP, FTP, Telnet, POP3
      [_ #t]))
  'high)

;;; Contract Analysis Functions

;; Find which contracts are most frequently violated
(spec:contract analyze-violations
  :ensures [(list? result)]
  :pure true)
(define (analyze-violations)
  (let ([stats @*contract-stats*])
    (sort (hash->list stats)
          (lambda (a b)
            (let ([a-ratio (/ (second (cdr a)) (max 1 (first (cdr a))))]
                  [b-ratio (/ (second (cdr b)) (max 1 (first (cdr b))))])
              (> a-ratio b-ratio))))))

;; Get recent violations
(spec:contract get-recent-violations
  :requires [(int? seconds)]
  :ensures [(list? result)]
  :pure true)
(define (get-recent-violations seconds)
  (let ([cutoff (- (current-timestamp) (* seconds 1000))])
    (filter (lambda (v)
              (> (Violation-timestamp v) cutoff))
            @*violations*)))

;; Explain why a packet violates a contract
(spec:contract explain-violation
  :requires [(and (Violation? violation))]
  :ensures [(string? result)]
  :pure true)
(define (explain-violation violation)
  (let* ([packet (Violation-packet violation)]
         [contract-name (Violation-contract violation)]
         [src (ip-to-string (packet-src-ip packet))]
         [dst (ip-to-string (packet-dst-ip packet))])
    (format "Contract '~a' violated: ~a\nPacket: ~a -> ~a (~a)"
            contract-name
            (Violation-reason violation)
            src dst
            (classify-packet packet))))

;; Generate contract coverage report
(spec:contract contract-coverage-report
  :ensures [(list? result)]
  :pure true)
(define (contract-coverage-report)
  (map (lambda (contract)
         (let* ([name (Contract-name contract)]
                [stats (hash-ref @*contract-stats* name '(0 0))]
                [total (first stats)]
                [violations (second stats)])
           (list name total violations 
                 (if (> total 0)
                     (/ violations total)
                     0))))
       @*security-contracts*))

;; Export contract functions
(provide SecurityContract Contract Contract?
         register-contract verify-packet
         analyze-violations get-recent-violations
         explain-violation contract-coverage-report
         *violations* *security-contracts*)