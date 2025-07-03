;;; ClaudeScope Core Types and Data Structures
;;; Network analyzer type definitions with contracts

;; IP Address representation (workaround for native IP type)
(define-type IPAddr
  (IPv4 (octets (list int)))  ; List of 4 integers [0-255]
  (IPv6 (segments (list int)))) ; List of 8 integers [0-65535]

;; MAC Address (6 octets)
(define-type MACAddr
  (MAC (octets (list int))))

;; Port number with contract
(spec:contract valid-port?
  :requires [(int? port)]
  :ensures [(and (>= port 0) (<= port 65535))]
  :pure true)

;; Network packet structure
(define-type Packet
  (TCPPacket 
    (src-ip IPAddr)
    (dst-ip IPAddr)
    (src-port int)
    (dst-port int)
    (seq-num int)
    (ack-num int)
    (flags (list symbol))  ; 'syn 'ack 'fin 'rst 'psh
    (payload (list int)))  ; Byte array as list
  (UDPPacket
    (src-ip IPAddr)
    (dst-ip IPAddr)
    (src-port int)
    (dst-port int)
    (payload (list int)))
  (ICMPPacket
    (src-ip IPAddr)
    (dst-ip IPAddr)
    (type int)
    (code int)
    (payload (list int))))

;; Network device representation
(define-type Device
  (Host
    (ip IPAddr)
    (mac MACAddr)
    (hostname string)
    (services (list int))     ; Open ports
    (os-hint string))
  (Router
    (interfaces (list IPAddr))
    (routing-table (list Route)))
  (Firewall
    (interfaces (list IPAddr))
    (rules (list FirewallRule))))

;; Routing entry
(define-type Route
  (StaticRoute
    (destination IPAddr)
    (netmask IPAddr)
    (gateway IPAddr)
    (interface string)
    (metric int)))

;; Firewall rule
(define-type FirewallRule
  (Rule
    (action symbol)      ; 'allow 'deny 'log
    (protocol symbol)    ; 'tcp 'udp 'icmp 'any
    (src-addr IPAddr)
    (src-mask IPAddr)
    (dst-addr IPAddr)
    (dst-mask IPAddr)
    (src-ports (list int))
    (dst-ports (list int))
    (priority int)))

;; Network topology edge
(define-type NetworkEdge
  (Connection
    (from Device)
    (to Device)
    (bandwidth int)      ; Mbps
    (latency int)        ; ms
    (packet-loss float)  ; percentage
    (active bool)))

;; Verification contract result
(define-type ContractResult
  (Pass
    (contract string)
    (packet Packet)
    (timestamp int))
  (Violation
    (contract string)
    (packet Packet)
    (reason string)
    (timestamp int)))

;; IP Address validation contracts
(spec:contract valid-ipv4?
  :requires [(IPv4? addr)]
  :ensures [(let ([octets (IPv4-octets addr)])
              (and (= (length octets) 4)
                   (forall ((o (in octets)))
                     (and (>= o 0) (<= o 255)))))]
  :pure true)

(spec:contract valid-ipv6?
  :requires [(IPv6? addr)]
  :ensures [(let ([segs (IPv6-segments addr)])
              (and (= (length segs) 8)
                   (forall ((s (in segs)))
                     (and (>= s 0) (<= s 65535)))))]
  :pure true)

;; MAC Address validation
(spec:contract valid-mac?
  :requires [(MAC? addr)]
  :ensures [(let ([octets (MAC-octets addr)])
              (and (= (length octets) 6)
                   (forall ((o (in octets)))
                     (and (>= o 0) (<= o 255)))))]
  :pure true)

;; Helper functions for IP operations
(define (make-ipv4 a b c d)
  (spec:assume (and (>= a 0) (<= a 255)
                    (>= b 0) (<= b 255)
                    (>= c 0) (<= c 255)
                    (>= d 0) (<= d 255)))
  (IPv4 (list a b c d)))

(define (ip-to-string ip)
  (match ip
    [(IPv4 octets) 
     (string-join (map int->string octets) ".")]
    [(IPv6 segments)
     (string-join (map (lambda (s) (int->hex s)) segments) ":")]))

(define (ip-in-subnet? ip subnet mask)
  (spec:contract ip-in-subnet?
    :requires [(and (valid-ipv4? ip)
                    (valid-ipv4? subnet)
                    (valid-ipv4? mask))]
    :pure true)
  (match (list ip subnet mask)
    [(list (IPv4 ip-octets) (IPv4 subnet-octets) (IPv4 mask-octets))
     (equal? (map (lambda (i s m) (bitwise-and i m))
                  ip-octets subnet-octets mask-octets)
             (map (lambda (s m) (bitwise-and s m))
                  subnet-octets mask-octets))]))

;; Packet classification
(define (classify-packet packet)
  (spec:contract classify-packet
    :ensures [(symbol? result)]
    :pure true)
  (match packet
    [(TCPPacket _ _ _ dst-port _ _ flags _)
     (cond
       [(= dst-port 80) 'http]
       [(= dst-port 443) 'https]
       [(= dst-port 22) 'ssh]
       [(= dst-port 25) 'smtp]
       [(= dst-port 53) 'dns]
       [(member? 'syn flags) 'tcp-handshake]
       [else 'tcp-data])]
    [(UDPPacket _ _ _ dst-port _)
     (cond
       [(= dst-port 53) 'dns]
       [(= dst-port 67) 'dhcp]
       [(= dst-port 123) 'ntp]
       [else 'udp-data])]
    [(ICMPPacket _ _ type _ _)
     (cond
       [(= type 8) 'ping-request]
       [(= type 0) 'ping-reply]
       [(= type 3) 'unreachable]
       [else 'icmp-other])]))

;; Export all types and contracts
(provide IPAddr IPv4 IPv6 IPv4? IPv6?
         MACAddr MAC MAC?
         Packet TCPPacket UDPPacket ICMPPacket
         TCPPacket? UDPPacket? ICMPPacket?
         Device Host Router Firewall
         Host? Router? Firewall?
         Route StaticRoute StaticRoute?
         FirewallRule Rule Rule?
         NetworkEdge Connection Connection?
         ContractResult Pass Violation Pass? Violation?
         valid-ipv4? valid-ipv6? valid-mac? valid-port?
         make-ipv4 ip-to-string ip-in-subnet?
         classify-packet)