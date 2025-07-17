//! Reward calculation for RL optimization

use super::environment::PerformanceMetrics;
use serde::{Deserialize, Serialize};

/// Configuration for reward calculation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RewardConfig {
    /// Weight for execution time improvement
    pub exec_time_weight: f32,
    /// Weight for memory usage improvement
    pub memory_weight: f32,
    /// Weight for binary size improvement
    pub binary_size_weight: f32,
    /// Weight for instruction count improvement
    pub instruction_weight: f32,
    /// Weight for cache performance
    pub cache_weight: f32,
    /// Weight for branch prediction
    pub branch_weight: f32,
    /// Penalty weight for compilation time
    pub compile_penalty_weight: f32,
    /// Bonus for achieving all targets
    pub synergy_bonus: f32,
}

impl Default for RewardConfig {
    fn default() -> Self {
        Self {
            exec_time_weight: 0.4,
            memory_weight: 0.2,
            binary_size_weight: 0.1,
            instruction_weight: 0.15,
            cache_weight: 0.1,
            branch_weight: 0.05,
            compile_penalty_weight: 0.1,
            synergy_bonus: 0.2,
        }
    }
}

/// Reward calculator for optimization actions
pub struct RewardCalculator {
    config: RewardConfig,
    /// Target improvements for bonus
    targets: PerformanceTargets,
}

/// Performance improvement targets
#[derive(Debug, Clone)]
pub struct PerformanceTargets {
    /// Target execution time reduction (%)
    pub exec_time_reduction: f32,
    /// Target memory reduction (%)
    pub memory_reduction: f32,
    /// Target binary size reduction (%)
    pub size_reduction: f32,
}

impl Default for PerformanceTargets {
    fn default() -> Self {
        Self {
            exec_time_reduction: 0.2,  // 20% improvement
            memory_reduction: 0.15,     // 15% improvement
            size_reduction: 0.1,        // 10% improvement
        }
    }
}

impl RewardCalculator {
    /// Create new reward calculator
    pub fn new(config: RewardConfig, targets: PerformanceTargets) -> Self {
        Self { config, targets }
    }
    
    /// Calculate reward for performance metrics
    pub fn calculate(
        &self,
        current: PerformanceMetrics,
        baseline: PerformanceMetrics,
        compilation_time_us: u64,
    ) -> RewardDetails {
        // Calculate individual improvements
        let exec_improvement = self.calculate_improvement(
            baseline.execution_time_us,
            current.execution_time_us,
        );
        
        let memory_improvement = self.calculate_improvement(
            baseline.memory_bytes,
            current.memory_bytes,
        );
        
        let instruction_improvement = self.calculate_improvement(
            baseline.instruction_count,
            current.instruction_count,
        );
        
        let cache_improvement = if baseline.cache_misses > 0 {
            self.calculate_improvement(baseline.cache_misses, current.cache_misses)
        } else {
            0.0
        };
        
        let branch_improvement = if baseline.branch_mispredictions > 0 {
            self.calculate_improvement(
                baseline.branch_mispredictions,
                current.branch_mispredictions,
            )
        } else {
            0.0
        };
        
        // Compilation time penalty (normalized to seconds)
        let compile_penalty = (compilation_time_us as f32 / 1_000_000.0).min(1.0);
        
        // Calculate weighted reward
        let base_reward = 
            exec_improvement * self.config.exec_time_weight +
            memory_improvement * self.config.memory_weight +
            instruction_improvement * self.config.instruction_weight +
            cache_improvement * self.config.cache_weight +
            branch_improvement * self.config.branch_weight -
            compile_penalty * self.config.compile_penalty_weight;
        
        // Check if targets are met for synergy bonus
        let targets_met = 
            exec_improvement >= self.targets.exec_time_reduction &&
            memory_improvement >= self.targets.memory_reduction;
        
        let synergy_bonus = if targets_met {
            self.config.synergy_bonus
        } else {
            0.0
        };
        
        let total_reward = (base_reward + synergy_bonus).max(-1.0).min(1.0);
        
        RewardDetails {
            total_reward,
            exec_improvement,
            memory_improvement,
            instruction_improvement,
            cache_improvement,
            branch_improvement,
            compile_penalty,
            synergy_bonus,
            targets_met,
        }
    }
    
    /// Calculate percentage improvement
    fn calculate_improvement(&self, baseline: u64, current: u64) -> f32 {
        if baseline == 0 {
            return 0.0;
        }
        
        ((baseline as f32 - current as f32) / baseline as f32).max(-1.0).min(1.0)
    }
    
    /// Update targets based on achieved performance
    pub fn adaptive_update_targets(&mut self, achieved: &RewardDetails) {
        // If consistently achieving targets, make them more aggressive
        if achieved.targets_met {
            self.targets.exec_time_reduction *= 1.1;
            self.targets.memory_reduction *= 1.1;
            self.targets.size_reduction *= 1.1;
        }
        
        // Cap targets at reasonable levels
        self.targets.exec_time_reduction = self.targets.exec_time_reduction.min(0.5);
        self.targets.memory_reduction = self.targets.memory_reduction.min(0.4);
        self.targets.size_reduction = self.targets.size_reduction.min(0.3);
    }
}

/// Detailed reward breakdown
#[derive(Debug, Clone)]
pub struct RewardDetails {
    pub total_reward: f32,
    pub exec_improvement: f32,
    pub memory_improvement: f32,
    pub instruction_improvement: f32,
    pub cache_improvement: f32,
    pub branch_improvement: f32,
    pub compile_penalty: f32,
    pub synergy_bonus: f32,
    pub targets_met: bool,
}

impl RewardDetails {
    /// Get a summary string
    pub fn summary(&self) -> String {
        format!(
            "Reward: {:.3} (exec: {:.1}%, mem: {:.1}%, bonus: {})",
            self.total_reward,
            self.exec_improvement * 100.0,
            self.memory_improvement * 100.0,
            if self.targets_met { "YES" } else { "NO" }
        )
    }
}

// Re-export PerformanceMetrics from environment
// PerformanceMetrics is already imported at the top