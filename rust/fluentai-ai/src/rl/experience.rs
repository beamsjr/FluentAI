//! Experience replay for RL training

use super::{OptimizationState, OptimizationAction};
use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

/// Single experience tuple
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Experience {
    /// State before action
    pub state: OptimizationState,
    /// Action taken
    pub action: OptimizationAction,
    /// Reward received
    pub reward: f32,
    /// Next state
    pub next_state: OptimizationState,
    /// Whether episode ended
    pub done: bool,
}

/// Experience replay buffer
pub struct ExperienceReplay {
    /// Buffer of experiences
    buffer: VecDeque<Experience>,
    /// Maximum buffer size
    capacity: usize,
}

impl ExperienceReplay {
    /// Create new replay buffer
    pub fn new(capacity: usize) -> Self {
        Self {
            buffer: VecDeque::with_capacity(capacity),
            capacity,
        }
    }
    
    /// Add experience to buffer
    pub fn push(&mut self, experience: Experience) {
        if self.buffer.len() >= self.capacity {
            self.buffer.pop_front();
        }
        self.buffer.push_back(experience);
    }
    
    /// Sample batch of experiences
    pub fn sample(&self, batch_size: usize) -> Vec<Experience> {
        let mut rng = rand::thread_rng();
        let experiences: Vec<_> = self.buffer.iter().cloned().collect();
        
        experiences
            .choose_multiple(&mut rng, batch_size.min(experiences.len()))
            .cloned()
            .collect()
    }
    
    /// Get buffer size
    pub fn len(&self) -> usize {
        self.buffer.len()
    }
    
    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
    
    /// Save experiences to file
    pub fn save(&self, path: &str) -> std::io::Result<()> {
        let data = bincode::serialize(&self.buffer)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        std::fs::write(path, data)
    }
    
    /// Load experiences from file
    pub fn load(&mut self, path: &str) -> std::io::Result<()> {
        let data = std::fs::read(path)?;
        self.buffer = bincode::deserialize(&data)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        Ok(())
    }
    
    /// Get statistics about experiences
    pub fn get_stats(&self) -> ExperienceStats {
        let rewards: Vec<f32> = self.buffer.iter().map(|e| e.reward).collect();
        let mean_reward = rewards.iter().sum::<f32>() / rewards.len().max(1) as f32;
        let max_reward = rewards.iter().cloned().fold(f32::NEG_INFINITY, f32::max);
        let min_reward = rewards.iter().cloned().fold(f32::INFINITY, f32::min);
        
        // Count action distribution
        let mut action_counts = std::collections::HashMap::new();
        for exp in &self.buffer {
            *action_counts.entry(format!("{:?}", exp.action)).or_insert(0) += 1;
        }
        
        ExperienceStats {
            total_experiences: self.buffer.len(),
            mean_reward,
            max_reward,
            min_reward,
            action_distribution: action_counts,
        }
    }
}

/// Statistics about experience replay buffer
#[derive(Debug)]
pub struct ExperienceStats {
    pub total_experiences: usize,
    pub mean_reward: f32,
    pub max_reward: f32,
    pub min_reward: f32,
    pub action_distribution: std::collections::HashMap<String, usize>,
}