#!/bin/bash

# ClaudeScope Demo Runner
# This script demonstrates how to run the ClaudeScope network analyzer

echo "=== ClaudeScope Demo ==="
echo ""
echo "Note: This demo requires a ClaudeLang interpreter to run the .cl files."
echo "The implementation showcases ClaudeLang's contract system applied to"
echo "network security analysis."
echo ""
echo "Demo Structure:"
echo "  - src/types.cl: Network data types with validation contracts"
echo "  - src/topology.cl: Network topology discovery with invariants"
echo "  - src/contracts.cl: Security policy verification"
echo "  - src/capture.cl: Packet capture simulation"
echo "  - src/firewall.cl: Firewall rule optimization"
echo "  - src/replay.cl: Traffic replay and analysis"
echo "  - src/explain.cl: Violation explainability engine"
echo "  - src/claudescope.cl: Main application logic"
echo "  - main.cl: Entry point"
echo ""
echo "Key Features Demonstrated:"
echo "  1. Design-by-contract for network security"
echo "  2. Formal verification of security properties"
echo "  3. Type-safe network data structures"
echo "  4. Pure function analysis for optimization"
echo "  5. Self-verifying security policies"
echo ""
echo "Example Security Contract (from src/contracts.cl):"
echo ""
cat << 'EOF'
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
EOF
echo ""
echo "To run with a ClaudeLang interpreter:"
echo "  claudelang main.cl           # Run interactive demo"
echo "  claudelang main.cl capture   # Start packet capture"
echo "  claudelang main.cl analyze   # Analyze network"
echo "  claudelang main.cl explain   # Explain violations"
echo "  claudelang main.cl report    # Generate report"
echo ""
echo "For Rust integration demo:"
echo "  cd ../../rust/claudelang-contracts"
echo "  cargo run --example claudescope_demo"
echo ""