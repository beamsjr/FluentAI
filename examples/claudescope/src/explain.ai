;;; Contract Violation Explainability Engine
;;; Provides human-readable explanations for security violations

(require "types.cl")
(require "contracts.cl")
(require "topology.cl")

;; Explanation types
(define-type Explanation
  (SimpleExplanation
    (violation Violation)
    (summary string)
    (details (list string))
    (severity symbol)
    (recommendations (list string)))
  (DetailedExplanation
    (violation Violation)
    (summary string)
    (technical-details (list string))
    (business-impact string)
    (remediation-steps (list string))
    (references (list string))))

;; Knowledge base for explanations
(define *explanation-templates* (atom (make-hash-table)))
(define *impact-assessments* (atom (make-hash-table)))

;; Initialize explanation templates
(define (init-explanation-templates)
  ;; Guest VLAN isolation violations
  (register-explanation-template
    "guest-vlan-isolation"
    (lambda (violation)
      (let* ([packet (Violation-packet violation)]
             [src-ip (ip-to-string (packet-src-ip packet))]
             [dst-ip (ip-to-string (packet-dst-ip packet))])
        (SimpleExplanation
          violation
          "Guest network attempting to access internal resources"
          (list
            (format "A device from the guest network (~a) attempted to connect to an internal system (~a)" src-ip dst-ip)
            "Guest networks should be isolated from internal corporate resources"
            "This could indicate a misconfigured device or potential security breach")
          'critical
          (list
            "Review guest network firewall rules"
            "Ensure VLAN tagging is properly configured"
            "Check for any unauthorized network bridges"
            "Consider implementing network access control (NAC)")))))
  
  ;; DNS whitelist violations
  (register-explanation-template
    "dns-server-whitelist"
    (lambda (violation)
      (let* ([packet (Violation-packet violation)]
             [dst-ip (ip-to-string (packet-dst-ip packet))])
        (SimpleExplanation
          violation
          "DNS query to unauthorized server detected"
          (list
            (format "A DNS query was sent to ~a, which is not an approved DNS server" dst-ip)
            "Using unauthorized DNS servers can lead to DNS hijacking attacks"
            "This may bypass content filtering and security controls")
          'high
          (list
            "Configure devices to use only approved DNS servers"
            "Block outbound DNS traffic to unauthorized servers"
            "Implement DNS over HTTPS (DoH) with approved providers"
            "Monitor for DNS tunneling attempts")))))
  
  ;; SSH access control violations
  (register-explanation-template
    "ssh-access-control"
    (lambda (violation)
      (let* ([packet (Violation-packet violation)]
             [src-ip (ip-to-string (packet-src-ip packet))])
        (SimpleExplanation
          violation
          "SSH access attempt from unauthorized network"
          (list
            (format "SSH connection attempt from ~a, which is outside the management network" src-ip)
            "SSH access should be restricted to authorized management networks only"
            "Unrestricted SSH access is a common attack vector")
          'high
          (list
            "Implement jump servers/bastion hosts for SSH access"
            "Use VPN for remote administrative access"
            "Enable multi-factor authentication for SSH"
            "Consider using SSH certificates instead of keys")))))
  
  ;; Port scan detection
  (register-explanation-template
    "port-scan-detection"
    (lambda (violation)
      (let* ([packet (Violation-packet violation)]
             [src-ip (ip-to-string (packet-src-ip packet))]
             [dst-ip (ip-to-string (packet-dst-ip packet))])
        (SimpleExplanation
          violation
          "Potential port scanning activity detected"
          (list
            (format "Host ~a is scanning multiple ports on ~a" src-ip dst-ip)
            "Port scanning is often used to identify vulnerable services"
            "This could be reconnaissance for a targeted attack")
          'medium
          (list
            "Block the scanning IP address"
            "Enable rate limiting on firewall"
            "Deploy an intrusion prevention system (IPS)"
            "Investigate the source of the scanning activity")))))
  
  ;; Encryption requirement violations
  (register-explanation-template
    "encryption-required"
    (lambda (violation)
      (let* ([packet (Violation-packet violation)]
             [port (match packet
                     [(TCPPacket _ _ _ dst-port _ _ _ _) dst-port]
                     [_ 0])]
             [service (port-to-service port)])
        (SimpleExplanation
          violation
          "Unencrypted sensitive service detected"
          (list
            (format "Traffic to ~a service on port ~a is not encrypted" service port)
            "Sensitive data transmitted without encryption can be intercepted"
            "This violates security best practices and compliance requirements")
          'high
          (list
            (format "Enable TLS/SSL for ~a service" service)
            "Use VPN for legacy unencrypted protocols"
            "Implement application-layer encryption"
            "Consider decommissioning insecure protocols"))))))

;; Register an explanation template
(define (register-explanation-template contract-name template-fn)
  (swap! *explanation-templates*
         (lambda (templates)
           (hash-set templates contract-name template-fn))))

;; Generate explanation for violation
(spec:contract explain-violation-detailed
  :requires [(Violation? violation)]
  :ensures [(Explanation? result)]
  :pure true)
(define (explain-violation-detailed violation)
  (let* ([contract-name (Violation-contract violation)]
         [template-fn (hash-ref @*explanation-templates* contract-name)])
    (if template-fn
        (template-fn violation)
        (generate-generic-explanation violation))))

;; Generate generic explanation for unknown violations
(define (generate-generic-explanation violation)
  (SimpleExplanation
    violation
    "Security policy violation detected"
    (list
      (format "Contract '~a' was violated" (Violation-contract violation))
      (Violation-reason violation)
      "Please review your security policies")
    'medium
    (list
      "Review the specific security contract"
      "Analyze network traffic patterns"
      "Consult with security team")))

;; Generate business impact assessment
(spec:contract assess-business-impact
  :requires [(Violation? violation)]
  :ensures [(string? result)]
  :pure true)
(define (assess-business-impact violation)
  (let ([contract (Violation-contract violation)])
    (cond
      [(equal? contract "guest-vlan-isolation")
       "HIGH IMPACT: Potential data breach risk. Guest devices accessing internal systems could lead to unauthorized data access, intellectual property theft, or compliance violations."]
      [(equal? contract "dns-server-whitelist")
       "MEDIUM IMPACT: DNS hijacking could redirect users to malicious sites, leading to credential theft or malware infections. May also bypass content filtering controls."]
      [(equal? contract "ssh-access-control")
       "HIGH IMPACT: Unauthorized SSH access could provide attackers with shell access to critical systems, potentially leading to complete system compromise."]
      [(equal? contract "port-scan-detection")
       "LOW-MEDIUM IMPACT: While scanning itself may not cause damage, it often precedes targeted attacks. Early detection prevents escalation."]
      [(equal? contract "encryption-required")
       "HIGH IMPACT: Unencrypted sensitive data transmission violates compliance requirements (PCI-DSS, HIPAA, GDPR) and exposes confidential information to interception."]
      [else
       "UNKNOWN IMPACT: Please consult with security team for impact assessment."])))

;; Generate remediation playbook
(spec:contract generate-remediation-playbook
  :requires [(Violation? violation)]
  :ensures [(list? result)]
  :pure true)
(define (generate-remediation-playbook violation)
  (let ([explanation (explain-violation-detailed violation)])
    (match explanation
      [(SimpleExplanation _ _ _ _ recommendations)
       (create-playbook-steps recommendations)]
      [(DetailedExplanation _ _ _ _ remediation-steps _)
       (create-playbook-steps remediation-steps)])))

;; Create detailed playbook steps
(define (create-playbook-steps recommendations)
  (map-indexed
    (lambda (i rec)
      (format "Step ~a: ~a\n  Priority: ~a\n  Estimated time: ~a\n"
              (+ i 1)
              rec
              (if (< i 2) "High" "Medium")
              (if (< i 2) "15-30 minutes" "1-2 hours")))
    recommendations))

;; Generate learning resources
(spec:contract get-learning-resources
  :requires [(string? contract-name)]
  :ensures [(list? result)]
  :pure true)
(define (get-learning-resources contract-name)
  (cond
    [(equal? contract-name "guest-vlan-isolation")
     (list
       "NIST SP 800-115: Technical Guide to Information Security Testing"
       "SANS: Network Segmentation Cheat Sheet"
       "RFC 5737: IPv4 Address Blocks Reserved for Documentation")]
    [(equal? contract-name "dns-server-whitelist")
     (list
       "DNS Security Best Practices (NIST)"
       "DNSSEC Deployment Guide"
       "Understanding DNS Attacks (SANS)")]
    [(equal? contract-name "ssh-access-control")
     (list
       "SSH Hardening Guide (CIS Benchmarks)"
       "NIST IR 7966: Security of Interactive and Automated Access Management"
       "OpenSSH Security Best Practices")]
    [else
     (list
       "General Network Security Resources"
       "SANS Reading Room"
       "NIST Cybersecurity Framework")]))

;; Analyze violation patterns
(spec:contract analyze-violation-patterns
  :requires [(list? violations)]
  :ensures [(string? result)]
  :pure true)
(define (analyze-violation-patterns violations)
  (let* ([by-contract (group-by Violation-contract violations)]
         [by-severity (group-violations-by-severity violations)]
         [time-patterns (analyze-time-patterns violations)])
    (string-append
      "=== Violation Pattern Analysis ===\n\n"
      "By Contract Type:\n"
      (string-join
        (map (lambda (pair)
               (format "  ~a: ~a violations"
                       (car pair)
                       (length (cdr pair))))
             by-contract)
        "\n")
      "\n\nBy Severity:\n"
      (string-join
        (map (lambda (pair)
               (format "  ~a: ~a violations"
                       (car pair)
                       (cdr pair)))
             by-severity)
        "\n")
      "\n\n"
      time-patterns
      "\n\nRecommendations:\n"
      (generate-pattern-recommendations by-contract))))

;; Helper functions
(define (port-to-service port)
  (cond
    [(= port 80) "HTTP"]
    [(= port 21) "FTP"]
    [(= port 23) "Telnet"]
    [(= port 110) "POP3"]
    [(= port 25) "SMTP"]
    [else (format "Port ~a" port)]))

(define (group-by key-fn lst)
  ;; Group list elements by key function
  (fold (lambda (item groups)
          (let* ([key (key-fn item)]
                 [group (or (assoc key groups) (cons key '()))])
            (cons (cons key (cons item (cdr group)))
                  (remove group groups))))
        '()
        lst))

(define (group-violations-by-severity violations)
  ;; Mock implementation - would extract severity from explanations
  (list (cons 'critical 10)
        (cons 'high 25)
        (cons 'medium 15)
        (cons 'low 5)))

(define (analyze-time-patterns violations)
  ;; Mock time pattern analysis
  "Time Patterns:\n  Peak violations: Business hours (9 AM - 5 PM)\n  Quiet periods: Overnight (12 AM - 6 AM)")

(define (generate-pattern-recommendations by-contract)
  (if (> (length by-contract) 5)
      "- High violation diversity suggests need for comprehensive security review\n- Consider security awareness training\n- Implement automated remediation"
      "- Focus on addressing top violation types\n- Implement targeted controls\n- Regular security audits recommended"))

(define (map-indexed fn lst)
  (let loop ([i 0] [lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (loop (+ i 1) (cdr lst) 
              (cons (fn i (car lst)) acc)))))

(define (remove item lst)
  (filter (lambda (x) (not (equal? x item))) lst))

;; Initialize templates on load
(init-explanation-templates)

;; Export explanation functions
(provide Explanation SimpleExplanation DetailedExplanation
         explain-violation-detailed assess-business-impact
         generate-remediation-playbook get-learning-resources
         analyze-violation-patterns
         *explanation-templates*)