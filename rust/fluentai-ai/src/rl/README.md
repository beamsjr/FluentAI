# FluentAI RL Module

This module implements reinforcement learning for automatic optimization strategy discovery.

## Module Structure

- `mod.rs` - Core types and action definitions
- `agent.rs` - DQN-based RL agent implementation
- `environment.rs` - Optimization environment and state management
- `experience.rs` - Experience replay buffer for stable learning
- `reward.rs` - Multi-objective reward calculation
- `training.rs` - Training loop and model management

## Quick Start

```rust
use fluentai_ai::rl::*;

// Create an agent
let config = AgentConfig::default();
let agent = OptimizationAgent::new(config, device);

// Select an optimization action
let state = OptimizationState::from_graph(&graph);
let action = agent.select_action(&state, false);

// Apply the action
let mut opt_config = OptimizationConfig::default();
opt_config.apply_action(action);
```

## Key Features

- **Deep Q-Learning**: Uses neural networks to approximate Q-values
- **Experience Replay**: Stores past experiences for stable learning
- **Target Network**: Separate network for computing target Q-values
- **Multi-Metric Rewards**: Considers execution time, memory, binary size, etc.
- **Adaptive Exploration**: Epsilon-greedy with decay
- **Prioritized Sampling**: Optional prioritized experience replay

## Testing

Run the RL tests:
```bash
cargo test simple_rl_tests --features "rl"
```

For integration tests:
```bash
cargo test rl_optimization_test --features "ai-analysis,rl"
```