#!/bin/bash
# Run the learning mode demo cleanly
cd /Users/joe/repos/claudelang/rust
export RUSTFLAGS="-A warnings"
cargo run --example learning_mode_live_demo -p fluentai-vm 2>&1 | \
  grep -E "(🚀|📋|📝|🏃|📊|=====|🎯|💡|✨|📈|🔬|🏁|Strategy:|Execution time:|Testing|Test #)" | \
  head -100