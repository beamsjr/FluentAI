//! Comprehensive tests for FluentAI VM optimization module

use fluentai_vm::{
    optimization::{
        InstructionFusion, FusedOpcode,
        InlineCache, ProfileInfo, CachedValue,
    },
    bytecode::{Instruction, Opcode},
};
use std::sync::Arc;
use std::thread;
use std::time::Instant;

#[test]
fn test_instruction_fusion_patterns() {
    let fusion = InstructionFusion::new();
    
    // Test Load + Load + Add pattern
    let instructions = vec![
        Instruction::with_arg(Opcode::Load, 0),
        Instruction::with_arg(Opcode::Load, 1),
        Instruction::new(Opcode::Add),
        Instruction::with_arg(Opcode::Store, 2),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 1);
    assert_eq!(opportunities[0].start_idx, 0);
    assert_eq!(opportunities[0].length, 3);
    assert!(matches!(
        opportunities[0].fused_op,
        FusedOpcode::LoadLoadAdd { local1: 0, local2: 1 }
    ));
}

#[test]
fn test_load_push_add_fusion() {
    let fusion = InstructionFusion::new();
    
    let instructions = vec![
        Instruction::with_arg(Opcode::Load, 5),
        Instruction::with_arg(Opcode::Push, 10),
        Instruction::new(Opcode::Add),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 1);
    assert!(matches!(
        opportunities[0].fused_op,
        FusedOpcode::LoadPushAdd { local: 5, const_idx: 10 }
    ));
}

#[test]
fn test_load_store_fusion() {
    let fusion = InstructionFusion::new();
    
    let instructions = vec![
        Instruction::with_arg(Opcode::Load, 3),
        Instruction::with_arg(Opcode::Store, 7),
        Instruction::with_arg(Opcode::Load, 4),
        Instruction::with_arg(Opcode::Store, 8),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 2);
    
    // First Load-Store pair
    assert!(matches!(
        opportunities[0].fused_op,
        FusedOpcode::LoadStore { load_idx: 3, store_idx: 7 }
    ));
    
    // Second Load-Store pair
    assert!(matches!(
        opportunities[1].fused_op,
        FusedOpcode::LoadStore { load_idx: 4, store_idx: 8 }
    ));
}

#[test]
fn test_compare_jump_fusion() {
    let fusion = InstructionFusion::new();
    
    // Test all comparison operators
    let comparisons = vec![
        Opcode::Eq, Opcode::Ne, Opcode::Lt, 
        Opcode::Le, Opcode::Gt, Opcode::Ge
    ];
    
    for comp_op in comparisons {
        let instructions = vec![
            Instruction::new(comp_op),
            Instruction::with_arg(Opcode::JumpIf, 100),
        ];
        
        let opportunities = fusion.analyze(&instructions);
        assert_eq!(opportunities.len(), 1);
        assert!(matches!(
            opportunities[0].fused_op,
            FusedOpcode::CompareJump { op, jump_offset: 100 } if op == comp_op
        ));
    }
}

#[test]
fn test_overlapping_patterns() {
    let fusion = InstructionFusion::new();
    
    // Pattern that could be interpreted multiple ways
    let instructions = vec![
        Instruction::with_arg(Opcode::Load, 0),
        Instruction::with_arg(Opcode::Load, 1),
        Instruction::new(Opcode::Add),
        Instruction::with_arg(Opcode::Store, 2),
        Instruction::with_arg(Opcode::Load, 2),
        Instruction::with_arg(Opcode::Store, 3),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 2);
    
    // Should get LoadLoadAdd and then LoadStore
    assert!(matches!(
        opportunities[0].fused_op,
        FusedOpcode::LoadLoadAdd { .. }
    ));
    assert!(matches!(
        opportunities[1].fused_op,
        FusedOpcode::LoadStore { load_idx: 2, store_idx: 3 }
    ));
}

#[test]
fn test_no_fusion_opportunities() {
    let fusion = InstructionFusion::new();
    
    // Instructions that don't form patterns
    let instructions = vec![
        Instruction::new(Opcode::Add),
        Instruction::new(Opcode::Sub),
        Instruction::new(Opcode::Mul),
        Instruction::new(Opcode::Div),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 0);
}

#[test]
fn test_inline_cache_basic() {
    let cache = InlineCache::new(4);
    
    // Cache a method lookup
    cache.update(
        100, // call site
        "String".to_string(),
        CachedValue::Function { chunk_id: 42, arity: 2 }
    );
    
    // Lookup should succeed
    let result = cache.lookup(100, "String");
    assert!(matches!(
        result,
        Some(CachedValue::Function { chunk_id: 42, arity: 2 })
    ));
    
    // Different type should miss
    assert!(cache.lookup(100, "Integer").is_none());
    
    // Different call site should miss
    assert!(cache.lookup(200, "String").is_none());
}

#[test]
fn test_inline_cache_polymorphic() {
    let cache = InlineCache::new(4);
    
    // Multiple types at same call site
    cache.update(100, "String".to_string(), CachedValue::PropertyOffset(10));
    cache.update(100, "Integer".to_string(), CachedValue::PropertyOffset(20));
    cache.update(100, "Float".to_string(), CachedValue::PropertyOffset(30));
    
    // All should be cached
    assert!(matches!(
        cache.lookup(100, "String"),
        Some(CachedValue::PropertyOffset(10))
    ));
    assert!(matches!(
        cache.lookup(100, "Integer"),
        Some(CachedValue::PropertyOffset(20))
    ));
    assert!(matches!(
        cache.lookup(100, "Float"),
        Some(CachedValue::PropertyOffset(30))
    ));
    
    // Check stats
    let stats = cache.stats();
    assert_eq!(stats.total_sites, 1);
    assert_eq!(stats.total_entries, 3);
}

#[test]
fn test_inline_cache_eviction() {
    let cache = InlineCache::new(2); // Small cache
    
    // Fill cache
    cache.update(100, "Type1".to_string(), CachedValue::PropertyOffset(1));
    cache.update(100, "Type2".to_string(), CachedValue::PropertyOffset(2));
    
    // Hit Type1 multiple times to increase hit count
    for _ in 0..5 {
        cache.lookup(100, "Type1");
        cache.update(100, "Type1".to_string(), CachedValue::PropertyOffset(1));
    }
    
    // Add new type - should evict Type2 (lower hit count)
    cache.update(100, "Type3".to_string(), CachedValue::PropertyOffset(3));
    
    // Type1 and Type3 should be present
    assert!(cache.lookup(100, "Type1").is_some());
    assert!(cache.lookup(100, "Type3").is_some());
}

#[test]
fn test_inline_cache_concurrent() {
    let cache = Arc::new(InlineCache::new(10));
    let num_threads = 4;
    let calls_per_thread = 100;
    
    let mut handles = vec![];
    
    for tid in 0..num_threads {
        let cache_clone = Arc::clone(&cache);
        
        let handle = thread::spawn(move || {
            for i in 0..calls_per_thread {
                let call_site = (tid * 10 + i % 10) as usize;
                let type_name = format!("Type{}", tid);
                
                // Update and lookup
                cache_clone.update(
                    call_site,
                    type_name.clone(),
                    CachedValue::PropertyOffset(tid * 100 + i)
                );
                
                let _ = cache_clone.lookup(call_site, &type_name);
            }
        });
        
        handles.push(handle);
    }
    
    for handle in handles {
        handle.join().unwrap();
    }
    
    // Verify cache is populated
    let stats = cache.stats();
    assert!(stats.total_sites > 0);
    assert!(stats.total_entries > 0);
}

#[test]
fn test_profile_info_instruction_counting() {
    let mut profile = ProfileInfo::new();
    
    // Simulate execution
    for _ in 0..1000 {
        profile.record_instruction(10);
    }
    for _ in 0..500 {
        profile.record_instruction(20);
    }
    for _ in 0..100 {
        profile.record_instruction(30);
    }
    profile.record_instruction(40);
    
    // Check hot instructions
    let hot = profile.hot_instructions(200);
    assert_eq!(hot.len(), 2);
    assert!(hot.contains(&10));
    assert!(hot.contains(&20));
    assert!(!hot.contains(&30));
    assert!(!hot.contains(&40));
}

#[test]
fn test_profile_info_branch_bias() {
    let mut profile = ProfileInfo::new();
    
    // Heavily biased branch (90% taken)
    for _ in 0..900 {
        profile.record_branch(100, true);
    }
    for _ in 0..100 {
        profile.record_branch(100, false);
    }
    
    // Unbiased branch (50/50)
    for _ in 0..500 {
        profile.record_branch(200, true);
    }
    for _ in 0..500 {
        profile.record_branch(200, false);
    }
    
    // Check biased branches
    let biased = profile.biased_branches(0.8);
    assert_eq!(biased.len(), 1);
    assert_eq!(biased[0], (100, true));
}

#[test]
fn test_profile_info_call_targets() {
    let mut profile = ProfileInfo::new();
    
    // Record call patterns
    for _ in 0..100 {
        profile.record_call(1000, 2000); // Monomorphic
    }
    
    for _ in 0..60 {
        profile.record_call(1100, 2100);
    }
    for _ in 0..40 {
        profile.record_call(1100, 2200); // Polymorphic
    }
    
    // Could verify call patterns if ProfileInfo exposed this data
}

#[test]
fn test_fusion_with_empty_instructions() {
    let fusion = InstructionFusion::new();
    let instructions = vec![];
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 0);
}

#[test]
fn test_fusion_with_single_instruction() {
    let fusion = InstructionFusion::new();
    let instructions = vec![
        Instruction::new(Opcode::Add),
    ];
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 0);
}

#[test]
fn test_cache_clear() {
    let cache = InlineCache::new(4);
    
    // Add some entries
    cache.update(100, "Type1".to_string(), CachedValue::PropertyOffset(10));
    cache.update(200, "Type2".to_string(), CachedValue::PropertyOffset(20));
    
    // Verify they're cached
    assert!(cache.lookup(100, "Type1").is_some());
    assert!(cache.lookup(200, "Type2").is_some());
    
    // Clear cache
    cache.clear();
    
    // Verify everything is gone
    assert!(cache.lookup(100, "Type1").is_none());
    assert!(cache.lookup(200, "Type2").is_none());
    
    let stats = cache.stats();
    assert_eq!(stats.total_sites, 0);
    assert_eq!(stats.total_entries, 0);
}

#[test]
fn test_complex_fusion_patterns() {
    let fusion = InstructionFusion::new();
    
    // Complex instruction sequence with multiple opportunities
    let instructions = vec![
        // First pattern: Load-Load-Add
        Instruction::with_arg(Opcode::Load, 0),
        Instruction::with_arg(Opcode::Load, 1),
        Instruction::new(Opcode::Add),
        
        // Some unrelated instructions
        Instruction::new(Opcode::Dup),
        Instruction::new(Opcode::Pop),
        
        // Second pattern: Load-Store
        Instruction::with_arg(Opcode::Load, 5),
        Instruction::with_arg(Opcode::Store, 6),
        
        // Third pattern: Compare-Jump
        Instruction::new(Opcode::Lt),
        Instruction::with_arg(Opcode::JumpIf, 50),
        
        // Fourth pattern: Load-Push-Add
        Instruction::with_arg(Opcode::Load, 10),
        Instruction::with_arg(Opcode::Push, 20),
        Instruction::new(Opcode::Add),
    ];
    
    let opportunities = fusion.analyze(&instructions);
    assert_eq!(opportunities.len(), 4);
    
    // Verify each opportunity
    assert_eq!(opportunities[0].start_idx, 0);
    assert_eq!(opportunities[1].start_idx, 5);
    assert_eq!(opportunities[2].start_idx, 7);
    assert_eq!(opportunities[3].start_idx, 9);
}

#[test]
fn test_profile_edge_cases() {
    let mut profile = ProfileInfo::new();
    
    // Branch with only taken
    for _ in 0..100 {
        profile.record_branch(300, true);
    }
    
    // Branch with only not taken
    for _ in 0..100 {
        profile.record_branch(400, false);
    }
    
    // Now check both branches
    let biased = profile.biased_branches(0.9);
    assert_eq!(biased.len(), 2);
    assert!(biased.contains(&(300, true)));
    assert!(biased.contains(&(400, false)));
}

#[test]
#[ignore = "Performance test - run manually"]
fn test_optimization_performance() {
    let fusion = InstructionFusion::new();
    let cache = InlineCache::new(100);
    
    // Generate large instruction sequence
    let mut instructions = Vec::new();
    for i in 0..10000 {
        instructions.push(Instruction::with_arg(Opcode::Load, i % 10));
        if i % 3 == 0 {
            instructions.push(Instruction::with_arg(Opcode::Load, (i + 1) % 10));
            instructions.push(Instruction::new(Opcode::Add));
        }
        if i % 5 == 0 {
            instructions.push(Instruction::with_arg(Opcode::Store, i % 10));
        }
    }
    
    // Measure fusion analysis time
    let start = Instant::now();
    let opportunities = fusion.analyze(&instructions);
    let fusion_time = start.elapsed();
    
    println!("Fusion analysis time: {:?}", fusion_time);
    println!("Found {} fusion opportunities", opportunities.len());
    
    // Measure cache performance
    let start = Instant::now();
    for i in 0..10000 {
        let call_site = i % 100;
        let type_name = format!("Type{}", i % 10);
        
        cache.update(
            call_site,
            type_name.clone(),
            CachedValue::PropertyOffset(i)
        );
        
        let _ = cache.lookup(call_site, &type_name);
    }
    let cache_time = start.elapsed();
    
    println!("Cache operations time: {:?}", cache_time);
    
    let stats = cache.stats();
    println!("Cache stats: {:?}", stats);
}