;;; Packet Capture Simulation
;;; Simulates network packet capture for demo purposes

(require "types.cl")
(require "topology.cl")
(require "contracts.cl")

;; Packet capture state
(define *capture-active* (atom #f))
(define *packet-buffer* (atom '()))
(define *capture-stats* (atom (make-hash-table)))
(define *packet-counter* (atom 0))

;; Capture configuration
(define-type CaptureConfig
  (Config
    (interface string)
    (promiscuous bool)
    (buffer-size int)
    (filter-expr string)
    (timeout-ms int)))

;; Default configuration
(define *default-config*
  (Config "eth0" #t 10000 "" 1000))

;; Contract: Buffer never exceeds configured size
(spec:contract buffer-size-invariant
  :invariant [(<= (length @*packet-buffer*) 
                  (Config-buffer-size *default-config*))])

;; Start packet capture
(spec:contract start-capture
  :requires [(CaptureConfig? config)]
  :modifies [*capture-active* *packet-buffer* *capture-stats*]
  :ensures [@*capture-active*])
(define (start-capture config)
  (reset! *capture-active* #t)
  (reset! *packet-buffer* '())
  (reset! *capture-stats* (make-hash-table))
  (reset! *packet-counter* 0)
  ;; In real implementation, would start pcap capture
  ;; For demo, we'll generate synthetic packets
  (spawn-packet-generator config)
  #t)

;; Stop packet capture
(spec:contract stop-capture
  :modifies [*capture-active*]
  :ensures [(not @*capture-active*)])
(define (stop-capture)
  (reset! *capture-active* #f))

;; Generate synthetic network traffic
(define (spawn-packet-generator config)
  ;; Simulate various network scenarios
  (define scenarios
    (list
      ;; Normal web traffic
      (lambda () (generate-http-traffic))
      ;; DNS queries
      (lambda () (generate-dns-traffic))
      ;; SSH connections
      (lambda () (generate-ssh-traffic))
      ;; Suspicious activity
      (lambda () (generate-suspicious-traffic))))
  
  ;; In a real async implementation, this would run in background
  ;; For demo, we'll generate a batch of packets
  (when @*capture-active*
    (for-each (lambda (scenario) (scenario)) scenarios)))

;; Generate HTTP traffic
(define (generate-http-traffic)
  (let* ([client-ip (make-ipv4 192 168 1 100)]
         [server-ip (make-ipv4 93 184 216 34)]  ; example.com
         [client-port (+ 49152 (random 16384))]  ; Ephemeral port
         [server-port 80])
    ;; SYN packet
    (capture-packet
      (TCPPacket client-ip server-ip client-port server-port
                 1000 0 '(syn) '()))
    ;; SYN-ACK packet
    (capture-packet
      (TCPPacket server-ip client-ip server-port client-port
                 2000 1001 '(syn ack) '()))
    ;; ACK packet
    (capture-packet
      (TCPPacket client-ip server-ip client-port server-port
                 1001 2001 '(ack) '()))
    ;; HTTP GET request
    (capture-packet
      (TCPPacket client-ip server-ip client-port server-port
                 1001 2001 '(ack psh)
                 (string->bytes "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")))))

;; Generate DNS traffic
(define (generate-dns-traffic)
  (let* ([client-ip (make-ipv4 192 168 1 100)]
         [dns-server (make-ipv4 8 8 8 8)]
         [client-port (+ 49152 (random 16384))])
    ;; DNS query
    (capture-packet
      (UDPPacket client-ip dns-server client-port 53
                 (string->bytes "example.com A query")))
    ;; DNS response
    (capture-packet
      (UDPPacket dns-server client-ip 53 client-port
                 (string->bytes "example.com A 93.184.216.34")))))

;; Generate SSH traffic
(define (generate-ssh-traffic)
  (let* ([client-ip (make-ipv4 192 168 1 100)]
         [server-ip (make-ipv4 10 0 100 50)]
         [client-port (+ 49152 (random 16384))])
    ;; SSH connection attempt
    (capture-packet
      (TCPPacket client-ip server-ip client-port 22
                 5000 0 '(syn) '()))))

;; Generate suspicious traffic (for testing security contracts)
(define (generate-suspicious-traffic)
  ;; Port scan simulation
  (let ([scanner-ip (make-ipv4 203 0 113 42)]
        [target-ip (make-ipv4 192 168 1 50)])
    (for-each
      (lambda (port)
        (capture-packet
          (TCPPacket scanner-ip target-ip 
                     (+ 49152 (random 16384)) port
                     (random 1000000) 0 '(syn) '())))
      '(21 22 23 25 80 443 3389 8080)))
  
  ;; Guest VLAN violation
  (let ([guest-ip (make-ipv4 192 168 100 50)]
        [internal-ip (make-ipv4 10 0 0 100)])
    (capture-packet
      (TCPPacket guest-ip internal-ip 
                 (+ 49152 (random 16384)) 3306
                 1000 0 '(syn) '())))
  
  ;; Unauthorized DNS query
  (let ([client-ip (make-ipv4 192 168 1 200)]
        [rogue-dns (make-ipv4 1 2 3 4)])
    (capture-packet
      (UDPPacket client-ip rogue-dns 
                 (+ 49152 (random 16384)) 53
                 (string->bytes "internal.corp DNS query")))))

;; Capture a packet and process it
(spec:contract capture-packet
  :requires [(Packet? packet)]
  :modifies [*packet-buffer* *packet-counter* *capture-stats*]
  :ensures [(> @*packet-counter* (old @*packet-counter*))])
(define (capture-packet packet)
  (when @*capture-active*
    ;; Add to buffer
    (swap! *packet-buffer* 
           (lambda (buf)
             (let ([new-buf (cons packet buf)])
               (if (> (length new-buf) (Config-buffer-size *default-config*))
                   (take new-buf (Config-buffer-size *default-config*))
                   new-buf))))
    ;; Update counter
    (swap! *packet-counter* (lambda (n) (+ n 1)))
    ;; Update stats
    (update-capture-stats packet)
    ;; Update topology
    (update-topology-from-packet packet)
    ;; Verify security contracts
    (verify-packet packet)))

;; Update capture statistics
(define (update-capture-stats packet)
  (let* ([proto (packet-protocol packet)]
         [current (or (hash-ref @*capture-stats* proto) 0)])
    (swap! *capture-stats*
           (lambda (stats)
             (hash-set stats proto (+ current 1))))))

;; Get packet protocol
(define (packet-protocol packet)
  (match packet
    [(TCPPacket _ _ _ _ _ _ _ _) 'tcp]
    [(UDPPacket _ _ _ _ _) 'udp]
    [(ICMPPacket _ _ _ _ _) 'icmp]))

;; Get recent packets
(spec:contract get-recent-packets
  :requires [(int? count)]
  :ensures [(and (list? result)
                 (<= (length result) count))]
  :pure true)
(define (get-recent-packets count)
  (take @*packet-buffer* (min count (length @*packet-buffer*))))

;; Get capture statistics
(spec:contract get-capture-stats
  :ensures [(hash-table? result)]
  :pure true)
(define (get-capture-stats)
  @*capture-stats*)

;; Apply packet filter
(spec:contract apply-packet-filter
  :requires [(and (string? filter) (list? packets))]
  :ensures [(list? result)]
  :pure true)
(define (apply-packet-filter filter packets)
  ;; Simple filter implementation
  (cond
    [(equal? filter "") packets]
    [(equal? filter "tcp") 
     (filter TCPPacket? packets)]
    [(equal? filter "udp")
     (filter UDPPacket? packets)]
    [(equal? filter "icmp")
     (filter ICMPPacket? packets)]
    [(string-prefix? filter "port ")
     (let ([port (string->int (substring filter 5))])
       (filter (lambda (p) (packet-has-port? p port)) packets))]
    [else packets]))

;; Check if packet uses specific port
(define (packet-has-port? packet port)
  (match packet
    [(TCPPacket _ _ src-port dst-port _ _ _ _)
     (or (= src-port port) (= dst-port port))]
    [(UDPPacket _ _ src-port dst-port _)
     (or (= src-port port) (= dst-port port))]
    [_ #f]))

;; Convert string to bytes (mock implementation)
(define (string->bytes str)
  (map char->int (string->list str)))

;; Random number generator (mock)
(define (random max)
  (mod (+ @*packet-counter* (* max 7)) max))

;; Helper to take first n elements
(define (take lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Export capture functions
(provide CaptureConfig Config Config?
         start-capture stop-capture
         capture-packet get-recent-packets
         get-capture-stats apply-packet-filter
         *capture-active* *packet-buffer* *capture-stats*)