;;; Network Topology Discovery and Mapping
;;; Automatically infers network structure from packet observations

(require "types.cl")

;; Network topology graph representation
(define-type NetworkGraph
  (Graph
    (nodes (list Device))
    (edges (list NetworkEdge))
    (last-updated int)))

;; Topology discovery state
(define *topology* (atom (Graph '() '() 0)))
(define *device-cache* (atom (make-hash-table)))  ; IP -> Device mapping
(define *conversation-cache* (atom (make-hash-table))) ; src:dst -> stats

;; Contract: Topology remains consistent
(spec:contract topology-invariant
  :invariant [(let ([g @*topology*])
                (and (Graph? g)
                     ;; No duplicate nodes
                     (unique? (map device-ip (Graph-nodes g)))
                     ;; Edges reference existing nodes
                     (forall ((edge (in (Graph-edges g))))
                       (and (member? (Connection-from edge) (Graph-nodes g))
                            (member? (Connection-to edge) (Graph-nodes g))))))])

;; Extract device IP address
(define (device-ip device)
  (match device
    [(Host ip _ _ _ _) ip]
    [(Router interfaces _ _) (first interfaces)]
    [(Firewall interfaces _) (first interfaces)]))

;; Update topology with observed packet
(spec:contract update-topology-from-packet
  :requires [(Packet? packet)]
  :modifies [*topology* *device-cache*]
  :ensures [(>= (length (Graph-nodes @*topology*))
                (old (length (Graph-nodes @*topology*))))])
(define (update-topology-from-packet packet)
  (let* ([src-ip (packet-src-ip packet)]
         [dst-ip (packet-dst-ip packet)]
         [src-device (ensure-device-exists src-ip)]
         [dst-device (ensure-device-exists dst-ip)])
    ;; Update device information based on packet
    (update-device-info src-device packet 'source)
    (update-device-info dst-device packet 'destination)
    ;; Update or create edge
    (update-edge src-device dst-device packet)
    ;; Update timestamp
    (swap! *topology* 
           (lambda (g) 
             (Graph (Graph-nodes g) 
                    (Graph-edges g) 
                    (current-timestamp))))))

;; Ensure device exists in topology
(define (ensure-device-exists ip)
  (or (hash-ref @*device-cache* ip)
      (let ([new-device (infer-device-type ip)])
        (swap! *device-cache* (lambda (cache) (hash-set cache ip new-device)))
        (swap! *topology* (lambda (g) (add-node g new-device)))
        new-device)))

;; Infer device type from IP and behavior
(define (infer-device-type ip)
  (cond
    ;; Router detection heuristics
    [(is-gateway-ip? ip) 
     (Router (list ip) '())]
    ;; Default to host
    [else 
     (Host ip (MAC '(0 0 0 0 0 0)) "" '() "unknown")]))

;; Check if IP is likely a gateway
(define (is-gateway-ip? ip)
  (match ip
    [(IPv4 octets)
     (or (= (fourth octets) 1)    ; x.x.x.1
         (= (fourth octets) 254))] ; x.x.x.254
    [_ #f]))

;; Update device information from packet
(define (update-device-info device packet role)
  (match device
    [(Host ip mac hostname services os)
     (when (eq? role 'source)
       ;; Source device - update services if new port seen
       (match packet
         [(TCPPacket _ _ src-port _ _ _ flags _)
          (when (and (member? 'syn flags)
                     (member? 'ack flags)
                     (not (member? src-port services)))
            (update-device device 
                          (Host ip mac hostname 
                                (cons src-port services) os)))]
         [_ #f]))]
    [_ #f]))

;; Update or create edge between devices
(spec:contract update-edge
  :requires [(and (Device? src) (Device? dst) (Packet? packet))]
  :modifies [*topology*])
(define (update-edge src dst packet)
  (let* ([edges (Graph-edges @*topology*)]
         [existing (find-edge src dst edges)])
    (if existing
        (update-edge-stats existing packet)
        (add-new-edge src dst packet))))

;; Find existing edge
(define (find-edge src dst edges)
  (find (lambda (edge)
          (match edge
            [(Connection from to _ _ _ _)
             (or (and (equal? from src) (equal? to dst))
                 (and (equal? from dst) (equal? to src)))]))
        edges))

;; Add new edge to topology
(define (add-new-edge src dst packet)
  (let ([new-edge (Connection src dst 
                             1000  ; Default 1Gbps
                             1     ; Default 1ms latency
                             0.0   ; No packet loss
                             #t)]) ; Active
    (swap! *topology* 
           (lambda (g) 
             (Graph (Graph-nodes g)
                    (cons new-edge (Graph-edges g))
                    (Graph-last-updated g))))))

;; Get packet source/destination IPs
(define (packet-src-ip packet)
  (match packet
    [(TCPPacket src _ _ _ _ _ _ _) src]
    [(UDPPacket src _ _ _ _) src]
    [(ICMPPacket src _ _ _ _) src]))

(define (packet-dst-ip packet)
  (match packet
    [(TCPPacket _ dst _ _ _ _ _ _) dst]
    [(UDPPacket _ dst _ _ _) dst]
    [(ICMPPacket _ dst _ _ _) dst]))

;; Topology analysis functions
(spec:contract find-all-paths
  :requires [(and (Device? src) (Device? dst))]
  :ensures [(list? result)]
  :pure true)
(define (find-all-paths src dst)
  (define (dfs current target visited path)
    (if (equal? current target)
        (list (reverse (cons current path)))
        (let ([neighbors (get-neighbors current)])
          (flatten
            (map (lambda (next)
                   (if (member? next visited)
                       '()
                       (dfs next target 
                            (cons current visited)
                            (cons current path))))
                 neighbors)))))
  (dfs src dst '() '()))

;; Get neighboring devices
(define (get-neighbors device)
  (let ([edges (Graph-edges @*topology*)])
    (filter-map
      (lambda (edge)
        (match edge
          [(Connection from to _ _ _ active)
           (cond
             [(and active (equal? from device)) to]
             [(and active (equal? to device)) from]
             [else #f])]))
      edges)))

;; Find potential bottlenecks
(spec:contract find-bottlenecks
  :ensures [(list? result)]
  :pure true)
(define (find-bottlenecks)
  (let* ([g @*topology*]
         [edges (Graph-edges g)])
    (filter (lambda (edge)
              (match edge
                [(Connection _ _ bandwidth latency loss _)
                 (or (< bandwidth 100)      ; Less than 100Mbps
                     (> latency 100)        ; More than 100ms
                     (> loss 1.0))]))       ; More than 1% loss
            edges)))

;; Detect network segmentation
(spec:contract find-isolated-segments
  :ensures [(list? result)]
  :pure true)
(define (find-isolated-segments)
  (let* ([g @*topology*]
         [nodes (Graph-nodes g)]
         [components '()])
    ;; Find connected components using DFS
    (fold (lambda (node components)
            (if (any? (lambda (comp) (member? node comp)) components)
                components
                (cons (find-component node) components)))
          '()
          nodes)))

;; Find all nodes in component containing start node
(define (find-component start)
  (define (dfs node visited)
    (if (member? node visited)
        visited
        (fold dfs 
              (cons node visited)
              (get-neighbors node))))
  (dfs start '()))

;; Export topology functions
(provide NetworkGraph Graph Graph?
         update-topology-from-packet
         find-all-paths find-bottlenecks
         find-isolated-segments
         *topology* topology-invariant)