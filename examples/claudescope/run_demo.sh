#!/bin/bash

# ClaudeScope Demo Runner
# This script demonstrates how to run the ClaudeScope network analyzer

echo "=== ClaudeScope Demo ==="
echo ""
echo "Note: This demo requires a FluentAI interpreter to run the .ai files."
echo "The implementation showcases FluentAI's contract system applied to"
echo "network security analysis."
echo ""
echo "Demo Structure:"
echo "  - src/types.ai: Network data types with validation contracts"
echo "  - src/topology.ai: Network topology discovery with invariants"
echo "  - src/contracts.ai: Security policy verification"
echo "  - src/capture.ai: Packet capture simulation"
echo "  - src/firewall.ai: Firewall rule optimization"
echo "  - src/replay.ai: Traffic replay and analysis"
echo "  - src/explain.ai: Violation explainability engine"
echo "  - src/claudescope.ai: Main application logic"
echo "  - main.ai: Entry point"
echo ""
echo "Key Features Demonstrated:"
echo "  1. Design-by-contract for network security"
echo "  2. Formal verification of security properties"
echo "  3. Type-safe network data structures"
echo "  4. Pure function analysis for optimization"
echo "  5. Self-verifying security policies"
echo ""
echo "Example Security Contract (from src/contracts.ai):"
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
echo "To run with a FluentAI interpreter:"
echo "  fluentai main.ai           # Run interactive demo"
echo "  fluentai main.ai capture   # Start packet capture"
echo "  fluentai main.ai analyze   # Analyze network"
echo "  fluentai main.ai explain   # Explain violations"
echo "  fluentai main.ai report    # Generate report"
echo ""
echo "For Rust integration demo:"
echo "  cd ../../rust/fluentai-contracts"
echo "  cargo run --example claudescope_demo"
echo ""