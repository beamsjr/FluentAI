#!/bin/bash
# Run the learning mode demo with full output
cd /Users/joe/repos/claudelang/rust
export RUSTFLAGS="-A warnings"
cargo run --example learning_mode_live_demo -p fluentai-vm --quiet 2>&1