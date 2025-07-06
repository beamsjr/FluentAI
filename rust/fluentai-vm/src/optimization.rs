//! Runtime optimizations including instruction fusion and inline caching

use crate::bytecode::{Opcode, Instruction, Value};
use rustc_hash::FxHashMap;
use std::sync::RwLock;

/// Fused instruction patterns for common sequences
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FusedOpcode {
    /// Load local + Load local + Add
    LoadLoadAdd { local1: u32, local2: u32 },
    /// Load local + Push constant + Add
    LoadPushAdd { local: u32, const_idx: u32 },
    /// Load local + Load local + Call
    LoadLoadCall { local1: u32, local2: u32, arity: u32 },
    /// Push + Push + Add (for integer constants)
    PushPushAddInt { val1: i64, val2: i64 },
    /// Load + Store sequence
    LoadStore { load_idx: u32, store_idx: u32 },
    /// Compare and jump
    CompareJump { op: Opcode, jump_offset: u32 },
}

/// Instruction fusion analyzer
pub struct InstructionFusion {
    /// Pattern matching window size
    window_size: usize,
}

impl InstructionFusion {
    pub fn new() -> Self {
        Self { window_size: 4 }
    }
    
    /// Analyze instruction sequence and return fused instructions
    pub fn analyze(&self, instructions: &[Instruction]) -> Vec<FusionOpportunity> {
        let mut opportunities = Vec::new();
        let mut i = 0;
        
        while i < instructions.len() {
            // Try to match patterns starting at position i
            if let Some(fusion) = self.try_match_pattern(&instructions[i..]) {
                opportunities.push(FusionOpportunity {
                    start_idx: i,
                    length: fusion.1,
                    fused_op: fusion.0,
                });
                i += fusion.1;
            } else {
                i += 1;
            }
        }
        
        opportunities
    }
    
    fn try_match_pattern(&self, instructions: &[Instruction]) -> Option<(FusedOpcode, usize)> {
        if instructions.len() < 2 {
            return None;
        }
        
        // Pattern: Load + Load + Add
        if instructions.len() >= 3 {
            if let (
                Instruction { opcode: Opcode::Load, arg: local1 },
                Instruction { opcode: Opcode::Load, arg: local2 },
                Instruction { opcode: Opcode::Add, .. }
            ) = (&instructions[0], &instructions[1], &instructions[2]) {
                return Some((FusedOpcode::LoadLoadAdd { 
                    local1: *local1, 
                    local2: *local2 
                }, 3));
            }
        }
        
        // Pattern: Load + Push + Add
        if instructions.len() >= 3 {
            if let (
                Instruction { opcode: Opcode::Load, arg: local },
                Instruction { opcode: Opcode::Push, arg: const_idx },
                Instruction { opcode: Opcode::Add, .. }
            ) = (&instructions[0], &instructions[1], &instructions[2]) {
                return Some((FusedOpcode::LoadPushAdd { 
                    local: *local, 
                    const_idx: *const_idx 
                }, 3));
            }
        }
        
        // Pattern: Load + Store
        if let (
            Instruction { opcode: Opcode::Load, arg: load_idx },
            Instruction { opcode: Opcode::Store, arg: store_idx }
        ) = (&instructions[0], &instructions[1]) {
            return Some((FusedOpcode::LoadStore { 
                load_idx: *load_idx, 
                store_idx: *store_idx 
            }, 2));
        }
        
        // Pattern: Compare + Jump
        if instructions.len() >= 2 {
            match &instructions[0].opcode {
                Opcode::Eq | Opcode::Ne | Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge => {
                    if let Instruction { opcode: Opcode::JumpIf, arg: offset } = &instructions[1] {
                        return Some((FusedOpcode::CompareJump { 
                            op: instructions[0].opcode, 
                            jump_offset: *offset 
                        }, 2));
                    }
                }
                _ => {}
            }
        }
        
        None
    }
}

#[derive(Debug)]
pub struct FusionOpportunity {
    pub start_idx: usize,
    pub length: usize,
    pub fused_op: FusedOpcode,
}

/// Inline cache for method/property lookups
pub struct InlineCache {
    /// Cache entries indexed by call site
    caches: RwLock<FxHashMap<usize, CacheEntry>>,
    /// Maximum entries per cache
    max_entries: usize,
}

#[derive(Clone)]
struct CacheEntry {
    /// Cached lookups for this call site
    entries: Vec<LookupEntry>,
}

#[derive(Clone)]
struct LookupEntry {
    /// Type or class of the receiver
    receiver_type: String,
    /// Cached value (function, property offset, etc.)
    cached_value: CachedValue,
    /// Hit count for this entry
    hit_count: u64,
}

#[derive(Clone)]
pub enum CachedValue {
    /// Cached function/method
    Function { chunk_id: usize, arity: u32 },
    /// Cached property offset
    PropertyOffset(usize),
    /// Cached constant value
    Constant(Value),
}

impl InlineCache {
    pub fn new(max_entries: usize) -> Self {
        Self {
            caches: RwLock::new(FxHashMap::default()),
            max_entries,
        }
    }
    
    /// Look up a cached value
    pub fn lookup(&self, call_site: usize, receiver_type: &str) -> Option<CachedValue> {
        let caches = self.caches.read().unwrap();
        if let Some(cache_entry) = caches.get(&call_site) {
            for entry in &cache_entry.entries {
                if entry.receiver_type == receiver_type {
                    return Some(entry.cached_value.clone());
                }
            }
        }
        None
    }
    
    /// Update cache with new lookup result
    pub fn update(&self, call_site: usize, receiver_type: String, value: CachedValue) {
        let mut caches = self.caches.write().unwrap();
        let cache_entry = caches.entry(call_site).or_insert_with(|| CacheEntry { 
            entries: Vec::new() 
        });
        
        // Check if we already have this type cached
        for entry in &mut cache_entry.entries {
            if entry.receiver_type == receiver_type {
                entry.cached_value = value;
                entry.hit_count += 1;
                return;
            }
        }
        
        // Add new entry
        if cache_entry.entries.len() < self.max_entries {
            cache_entry.entries.push(LookupEntry {
                receiver_type,
                cached_value: value,
                hit_count: 1,
            });
        } else {
            // Evict least used entry
            if let Some(min_idx) = cache_entry.entries
                .iter()
                .enumerate()
                .min_by_key(|(_, e)| e.hit_count)
                .map(|(i, _)| i) 
            {
                cache_entry.entries[min_idx] = LookupEntry {
                    receiver_type,
                    cached_value: value,
                    hit_count: 1,
                };
            }
        }
    }
    
    /// Clear all caches
    pub fn clear(&self) {
        self.caches.write().unwrap().clear();
    }
    
    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let caches = self.caches.read().unwrap();
        let total_sites = caches.len();
        let total_entries: usize = caches.values().map(|c| c.entries.len()).sum();
        let total_hits: u64 = caches.values()
            .flat_map(|c| &c.entries)
            .map(|e| e.hit_count)
            .sum();
        
        CacheStats {
            total_sites,
            total_entries,
            total_hits,
        }
    }
}

#[derive(Debug)]
pub struct CacheStats {
    pub total_sites: usize,
    pub total_entries: usize,
    pub total_hits: u64,
}

/// Profiling information for hot paths
pub struct ProfileInfo {
    /// Execution counts for each instruction
    pub instruction_counts: FxHashMap<usize, u64>,
    /// Branch taken counts
    pub branch_taken: FxHashMap<usize, u64>,
    /// Branch not taken counts
    pub branch_not_taken: FxHashMap<usize, u64>,
    /// Call target frequencies
    pub call_targets: FxHashMap<usize, FxHashMap<usize, u64>>,
}

impl ProfileInfo {
    pub fn new() -> Self {
        Self {
            instruction_counts: FxHashMap::default(),
            branch_taken: FxHashMap::default(),
            branch_not_taken: FxHashMap::default(),
            call_targets: FxHashMap::default(),
        }
    }
    
    /// Record instruction execution
    pub fn record_instruction(&mut self, pc: usize) {
        *self.instruction_counts.entry(pc).or_insert(0) += 1;
    }
    
    /// Record branch outcome
    pub fn record_branch(&mut self, pc: usize, taken: bool) {
        if taken {
            *self.branch_taken.entry(pc).or_insert(0) += 1;
        } else {
            *self.branch_not_taken.entry(pc).or_insert(0) += 1;
        }
    }
    
    /// Record call target
    pub fn record_call(&mut self, call_site: usize, target: usize) {
        *self.call_targets
            .entry(call_site)
            .or_default()
            .entry(target)
            .or_insert(0) += 1;
    }
    
    /// Get hot instructions (executed more than threshold times)
    pub fn hot_instructions(&self, threshold: u64) -> Vec<usize> {
        self.instruction_counts
            .iter()
            .filter(|(_, &count)| count > threshold)
            .map(|(&pc, _)| pc)
            .collect()
    }
    
    /// Get biased branches (heavily favor one direction)
    pub fn biased_branches(&self, bias_threshold: f64) -> Vec<(usize, bool)> {
        let mut biased = Vec::new();
        let mut checked = std::collections::HashSet::new();
        
        // Check branches that were taken at least once
        for (&pc, &taken_count) in &self.branch_taken {
            let not_taken_count = self.branch_not_taken.get(&pc).copied().unwrap_or(0);
            let total = taken_count + not_taken_count;
            
            if total > 0 {
                let taken_ratio = taken_count as f64 / total as f64;
                if taken_ratio > bias_threshold {
                    biased.push((pc, true));
                } else if taken_ratio < (1.0 - bias_threshold) {
                    biased.push((pc, false));
                }
            }
            checked.insert(pc);
        }
        
        // Also check branches that were never taken (only in branch_not_taken)
        for (&pc, &not_taken_count) in &self.branch_not_taken {
            if checked.contains(&pc) {
                continue; // Already processed
            }
            
            let taken_count = 0; // Never taken
            let total = taken_count + not_taken_count;
            
            if total > 0 {
                let taken_ratio = taken_count as f64 / total as f64;
                // taken_ratio is 0, so it's definitely < (1.0 - bias_threshold)
                if bias_threshold < 1.0 {
                    biased.push((pc, false));
                }
            }
        }
        
        biased
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_instruction_fusion() {
        let fusion = InstructionFusion::new();
        let instructions = vec![
            Instruction::with_arg(Opcode::Load, 0),
            Instruction::with_arg(Opcode::Load, 1),
            Instruction::new(Opcode::Add),
            Instruction::with_arg(Opcode::Store, 2),
        ];
        
        let opportunities = fusion.analyze(&instructions);
        assert_eq!(opportunities.len(), 1);
        assert!(matches!(
            opportunities[0].fused_op,
            FusedOpcode::LoadLoadAdd { local1: 0, local2: 1 }
        ));
    }
    
    #[test]
    fn test_inline_cache() {
        let cache = InlineCache::new(2);
        
        // Add cache entries
        cache.update(100, "String".to_string(), CachedValue::PropertyOffset(10));
        cache.update(100, "Int".to_string(), CachedValue::PropertyOffset(20));
        
        // Lookup
        assert!(matches!(
            cache.lookup(100, "String"),
            Some(CachedValue::PropertyOffset(10))
        ));
        
        // Stats
        let stats = cache.stats();
        assert_eq!(stats.total_sites, 1);
        assert_eq!(stats.total_entries, 2);
    }
    
    #[test]
    fn test_profile_info() {
        let mut profile = ProfileInfo::new();
        
        // Record executions
        for _ in 0..100 {
            profile.record_instruction(10);
        }
        profile.record_instruction(20);
        
        // Record branches
        for _ in 0..90 {
            profile.record_branch(30, true);
        }
        for _ in 0..10 {
            profile.record_branch(30, false);
        }
        
        // Check hot instructions
        let hot = profile.hot_instructions(50);
        assert_eq!(hot, vec![10]);
        
        // Check biased branches
        let biased = profile.biased_branches(0.8);
        assert_eq!(biased, vec![(30, true)]);
    }
}