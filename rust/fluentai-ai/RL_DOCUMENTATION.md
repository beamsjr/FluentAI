# FluentAI Reinforcement Learning System Documentation

## Overview

The FluentAI RL system uses Deep Q-Network (DQN) based reinforcement learning to automatically discover optimal optimization strategies for AST graphs. The system learns from runtime performance metrics to suggest the best combination of optimization passes for different code patterns.

## Implementation Status

### Completed Features

1. **Full Action Mapping**: All 16 optimization actions are properly mapped with indices
2. **Action Masking**: Proper one-hot encoding extraction from action tensors
3. **Q-Value Calculation**: Implemented proper Bellman equation for target Q-values
4. **Model Serialization**: Added save/load functionality using Burn's record system
5. **Gradient Updates**: Structured for autodiff backend support
6. **Autodiff Backend**: Enabled in Cargo.toml configuration

### Note on Gradient Updates

The gradient computation is structured to support Burn's autodiff backend. When using a backend with autodiff support (e.g., `burn-autodiff`), the training loop will automatically compute gradients and update network weights. For backends without autodiff, the system logs the loss for monitoring purposes.

### Implementation Details

1. **Q-Value Computation**: Uses a mask-based approach to select max Q-values from the target network
2. **Bellman Equation**: Properly implements `Q(s,a) = r + γ * max_a' Q(s',a') * (1 - done)`
3. **Action Masking**: Extracts action indices from tensors and creates one-hot encodings
4. **Tensor Operations**: All dimension mismatches have been resolved with proper reshaping
5. **Model Persistence**: Uses Burn's `BinFileRecorder` for saving and loading trained models

## Architecture

### Core Components

1. **OptimizationAgent** (`src/rl/agent.rs`)
   - DQN-based agent that learns Q-values for optimization actions
   - Uses epsilon-greedy exploration strategy
   - Maintains target network for stable learning

2. **OptimizationEnvironment** (`src/rl/environment.rs`)
   - Manages the optimization state and AST transformations
   - Calculates rewards based on performance improvements
   - Tracks multiple metrics: execution time, memory, binary size, etc.

3. **ExperienceReplay** (`src/rl/experience.rs`)
   - Stores and samples past experiences for stable learning
   - Implements prioritized experience replay
   - Provides statistics about learned patterns

4. **RewardCalculator** (`src/rl/reward.rs`)
   - Multi-objective reward function
   - Configurable weights for different performance metrics
   - Adaptive target adjustment based on achieved performance

5. **Trainer** (`src/rl/training.rs`)
   - Manages the training loop
   - Handles model checkpointing and statistics
   - Supports both training and evaluation modes

## Action Space

The agent can select from 16 different optimization actions:

```rust
pub enum OptimizationAction {
    NoOp,                              // No optimization
    ConstantFolding,                   // Fold compile-time constants
    DeadCodeElimination,               // Remove unreachable code
    CSE,                               // Common Subexpression Elimination
    Inline(InlineLevel),               // Function inlining (3 levels)
    TailCall,                          // Tail call optimization
    LoopOpt,                           // Loop optimizations
    BetaReduction,                     // Lambda calculus reduction
    PartialEval,                       // Partial evaluation
    StrengthReduction,                 // Replace expensive ops
    AlgebraicSimplification,           // Simplify expressions
    LoopInvariantMotion,               // Hoist loop-invariant code
    FunctionSpecialization,            // Specialize generic functions
    Composite(u8),                     // Combinations of passes
}
```

## State Representation

The agent observes:
- AST graph features (node types, depth, branching factor)
- Current optimization configuration
- Performance history statistics
- Resource usage patterns

## Reward Function

The reward calculation considers:
- **Execution time improvement** (40% weight)
- **Memory usage reduction** (20% weight)
- **Binary size optimization** (10% weight)
- **Instruction count efficiency** (15% weight)
- **Cache performance** (10% weight)
- **Branch prediction accuracy** (5% weight)
- **Compilation time penalty** (-10% weight)

## Usage

### Basic Training

```rust
use fluentai_ai::rl::{Trainer, TrainingConfig, AgentConfig};
use fluentai_core::ast::Graph;

// Configure the agent
let agent_config = AgentConfig::default();
let training_config = TrainingConfig {
    num_episodes: 1000,
    batch_size: 32,
    target_update_frequency: 10,
    ..Default::default()
};

// Create environment with compilation and execution functions
let environment = OptimizationEnvironment::new(
    50,  // max steps per episode
    compile_fn,
    execute_fn,
);

// Create trainer
let mut trainer = Trainer::<Burn>::new(
    agent_config,
    training_config,
    environment,
    device,
);

// Train on a dataset of graphs
let graphs: Vec<Graph> = load_training_data();
let stats = trainer.train(graphs)?;
```

### Using Trained Agent

```rust
// Load trained agent
let agent = OptimizationAgent::load("models/rl_optimizer.bin")?;

// Get optimization recommendations
let state = OptimizationState::from_graph(&graph);
let action = agent.select_action(&state, false);  // false = no exploration

// Apply the recommended optimization
let config = OptimizationConfig::default();
config.apply_action(action);
```

## Configuration

### Agent Configuration

```rust
pub struct AgentConfig {
    pub state_dim: usize,           // State vector dimension (default: 64)
    pub hidden_dims: Vec<usize>,    // Hidden layer sizes (default: [128, 64, 32])
    pub action_dim: usize,          // Number of actions (default: 16)
    pub learning_rate: f32,         // Learning rate (default: 0.001)
    pub gamma: f32,                 // Discount factor (default: 0.95)
    pub epsilon: f32,               // Initial exploration rate (default: 1.0)
    pub epsilon_decay: f32,         // Exploration decay (default: 0.995)
    pub epsilon_min: f32,           // Minimum exploration (default: 0.01)
}
```

### Training Configuration

```rust
pub struct TrainingConfig {
    pub num_episodes: usize,            // Training episodes (default: 1000)
    pub max_steps_per_episode: usize,   // Max steps per episode (default: 50)
    pub batch_size: usize,              // Batch size (default: 32)
    pub target_update_frequency: usize, // Target network update (default: 10)
    pub min_replay_size: usize,         // Min experiences before training (default: 1000)
    pub train_frequency: usize,         // Train every N steps (default: 4)
    pub save_frequency: usize,          // Save every N episodes (default: 100)
    pub model_path: String,             // Model save path
    pub experience_path: String,        // Experience save path
}
```

## Performance Monitoring

The system tracks:
- Episode rewards over time
- Action selection frequencies
- Performance improvement statistics per action type
- Resource usage trends

## Integration with CLI

Enable RL-based optimization:

```bash
# Use RL mode for optimization
fluentai run program.flc --optimize=rl

# Train new RL model
fluentai train-rl --episodes=5000 --dataset=training_data/
```

## Future Enhancements

1. **Multi-objective optimization** - Pareto-optimal solutions
2. **Transfer learning** - Reuse knowledge across different codebases
3. **Online learning** - Continue learning from production workloads
4. **Interpretability** - Explain why certain optimizations are chosen
5. **Hardware-specific models** - Optimize for specific architectures

## Troubleshooting

### Common Issues

1. **Slow convergence**: Increase `num_episodes` or adjust learning rate
2. **Poor generalization**: Ensure diverse training dataset
3. **High variance in rewards**: Adjust reward weights or increase batch size
4. **Memory issues**: Reduce `experience_replay` capacity

### Debug Mode

Enable detailed logging:
```rust
std::env::set_var("FLUENTAI_RL_DEBUG", "1");
```

## Examples

See `tests/rl_optimization_test.rs` for complete examples of:
- Training an agent on synthetic graphs
- Evaluating optimization performance
- Comparing RL vs manual optimization strategies