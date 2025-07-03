# ClaudeScope - Self-Verifying Network Analyzer

A demonstration application showcasing ClaudeLang's design-by-contract capabilities in the context of network security analysis.

## Overview

ClaudeScope is a network analyzer that automatically verifies security policies using formal contracts. It demonstrates how ClaudeLang's contract system can be applied to real-world security problems.

## Features

### 1. **Live Traffic Analysis**
- Simulated packet capture with realistic network traffic
- Real-time topology discovery
- Automatic device classification

### 2. **Security Contract Verification**
- Pre-defined security policies as formal contracts
- Real-time violation detection
- Contract coverage reporting

### 3. **Topology Mapping**
- Automatic network graph construction
- Bottleneck detection
- Network segmentation analysis

### 4. **Firewall Optimization**
- Rule conflict detection
- Redundancy elimination
- Optimization recommendations

### 5. **Traffic Replay**
- Session recording and replay
- Speed control and filtering
- Comparative analysis

### 6. **Explainable Violations**
- Human-readable explanations
- Business impact assessment
- Remediation playbooks

## Architecture

```
claudescope/
├── src/
│   ├── types.cl          # Core data types (IP, MAC, Packet, etc.)
│   ├── topology.cl       # Network topology discovery
│   ├── contracts.cl      # Security contract definitions
│   ├── capture.cl        # Packet capture simulation
│   ├── firewall.cl       # Firewall rule optimization
│   ├── replay.cl         # Traffic replay engine
│   ├── explain.cl        # Violation explainability
│   └── claudescope.cl    # Main application logic
├── main.cl               # Entry point
├── README.md             # This file
└── LanguageRequests.md   # ClaudeLang feature wishlist
```

## Running the Demo

### Interactive Demo (Recommended)
```bash
claudelang main.cl
```

This runs through a complete workflow demonstrating all features.

### Individual Commands
```bash
# Start packet capture
claudelang main.cl capture

# Analyze network
claudelang main.cl analyze

# Explain violations
claudelang main.cl explain

# Generate report
claudelang main.cl report
```

## Example Security Contracts

### Guest VLAN Isolation
```clojure
(register-contract
  "guest-vlan-isolation"
  "Guest VLAN (192.168.100.0/24) should not access internal services"
  (lambda (packet)
    (not (and (ip-in-subnet? src-ip guest-subnet guest-mask)
              (ip-in-subnet? dst-ip internal-subnet internal-mask))))
  'critical)
```

### Port Scan Detection
```clojure
(register-contract
  "port-scan-detection"
  "Detect potential port scanning activity"
  (lambda (packet)
    (< (length unique-ports-in-window) 10))
  'medium)
```

## Contract Verification

ClaudeScope uses ClaudeLang's contract system to ensure:

1. **Data Integrity**: All network data types have validation contracts
2. **State Consistency**: Topology invariants are maintained
3. **Function Correctness**: All analysis functions have pre/post conditions
4. **Pure Functions**: Analysis functions are marked as pure for caching

Example contract:
```clojure
(spec:contract ip-in-subnet?
  :requires [(and (valid-ipv4? ip)
                  (valid-ipv4? subnet)
                  (valid-ipv4? mask))]
  :ensures [(bool? result)]
  :pure true)
```

## Sample Output

```
=== ClaudeScope Network Analysis Report ===
Generated: 2024-01-15 14:30:00

Network Topology:
  Total devices: 42
  Active connections: 156
  Network segments: 3
  Bottlenecks detected: 2

Security Compliance:
  Compliance score: 87%
  Total violations: 23
  Critical violations: 2
  High severity violations: 5

Traffic Statistics:
  Total packets captured: 10,000
  Protocol breakdown:
    tcp: 6,234 packets
    udp: 2,890 packets
    icmp: 876 packets

Recommendations:
  - Network bottlenecks detected. Consider upgrading network infrastructure.
  - Enable continuous monitoring and alerting for critical violations.
  - Schedule regular security audits and penetration testing.
```

## Technical Implementation

### Type Safety
ClaudeScope uses ClaudeLang's algebraic data types for all network entities:
```clojure
(define-type Packet
  (TCPPacket (src-ip IPAddr) (dst-ip IPAddr) ...)
  (UDPPacket (src-ip IPAddr) (dst-ip IPAddr) ...)
  (ICMPPacket (src-ip IPAddr) (dst-ip IPAddr) ...))
```

### Contract Verification
All critical functions include formal contracts:
```clojure
(spec:contract verify-packet
  :requires [(Packet? packet)]
  :modifies [*violations* *contract-stats*]
  :ensures [(list? result)])
```

### Pure Function Analysis
Analysis functions are marked as pure for optimization:
```clojure
(spec:contract analyze-violations
  :ensures [(list? result)]
  :pure true)
```

## Limitations and Workarounds

Due to current ClaudeLang limitations, we implement workarounds for:
- **Network Types**: IP addresses as tagged unions instead of native types
- **Binary Data**: Packet payloads as integer lists instead of byte arrays
- **Async Operations**: Synchronous simulation instead of true async
- **External FFI**: Mock packet capture instead of libpcap integration

See `LanguageRequests.md` for detailed feature requests.

## Future Enhancements

With additional ClaudeLang features:
1. Real packet capture via libpcap FFI
2. Concurrent packet processing with channels
3. Binary pattern matching for deep packet inspection
4. SIMD operations for high-performance analysis
5. Native network types with built-in validation

## Conclusion

ClaudeScope demonstrates how formal contracts can enhance network security tools by:
- Catching configuration errors at compile time
- Providing mathematical guarantees about security properties
- Enabling safe optimizations through purity analysis
- Creating self-documenting, verifiable security policies

The combination of functional programming and design-by-contract creates more reliable and maintainable security software.