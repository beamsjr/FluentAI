//! Tests for the security manager

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::security::{SecurityManager, SecurityPolicy, SecurityContext, Capability, TaintLevel, TaintTracker};
    use crate::bytecode::Value;
    use std::sync::Arc;
    use std::time::Duration;
    use std::sync::atomic::Ordering;
    
    #[test]
    fn test_security_policy_default() {
        let policy = SecurityPolicy::default();
        
        // Check default limits
        assert_eq!(policy.max_memory, 100 * 1024 * 1024); // 100MB
        assert_eq!(policy.max_allocations, 100_000);
        assert_eq!(policy.max_instructions, 10_000_000);
        assert_eq!(policy.max_file_handles, 10);
        assert_eq!(policy.max_network_connections, 10);
        assert_eq!(policy.max_threads, 1);
        assert!(!policy.allow_eval);
        assert!(policy.strict_mode);
    }
    
    #[test]
    fn test_security_policy_sandbox() {
        let policy = SecurityPolicy::sandbox();
        
        // Check stricter limits
        assert_eq!(policy.max_memory, 10 * 1024 * 1024); // 10MB
        assert_eq!(policy.max_allocations, 10_000);
        assert_eq!(policy.max_instructions, 1_000_000);
        assert_eq!(policy.max_file_handles, 0);
        assert_eq!(policy.max_network_connections, 0);
        assert_eq!(policy.max_threads, 0);
        assert!(!policy.allow_eval);
        assert!(policy.strict_mode);
    }
    
    #[test]
    fn test_security_policy_trusted() {
        let policy = SecurityPolicy::trusted();
        
        // More permissive limits
        assert_eq!(policy.max_memory, u64::MAX);
        assert_eq!(policy.max_allocations, u64::MAX);
        assert_eq!(policy.max_instructions, u64::MAX);
        assert_eq!(policy.max_file_handles, 1000);
        assert_eq!(policy.max_network_connections, 1000);
        assert_eq!(policy.max_threads, 100);
        assert!(policy.allow_eval); // Trusted allows eval
        assert!(!policy.strict_mode);
    }
    
    #[test]
    fn test_security_context_capabilities() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Add a capability
        let file_cap = Capability::FileSystem { paths: vec!["/tmp".to_string()] };
        assert!(!context.has_capability(&file_cap));
        context.grant_capability(file_cap.clone());
        assert!(context.has_capability(&file_cap));
        
        // Check non-existent capability
        let net_cap = Capability::Network { hosts: vec!["localhost".to_string()] };
        assert!(!context.has_capability(&net_cap));
        
        // Revoke capability
        context.revoke_capability(&file_cap);
        assert!(!context.has_capability(&file_cap));
    }
    
    #[test]
    fn test_resource_tracking() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Track memory
        assert!(context.track_allocation(1024).is_ok());
        assert!(context.track_allocation(1024).is_ok());
        
        // Track instructions
        for _ in 0..100 {
            assert!(context.track_instruction().is_ok());
        }
        
        // Track deallocation
        context.track_deallocation(512);
    }
    
    #[test]
    fn test_resource_limits() {
        // Test memory limits
        let policy1 = SecurityPolicy {
            max_memory: 1024, // 1KB
            ..SecurityPolicy::default()
        };
        
        let context1 = SecurityContext::new(policy1);
        
        // Memory limit
        assert!(context1.track_allocation(512).is_ok());
        assert!(context1.track_allocation(600).is_err()); // Would exceed memory limit
        
        // Test instruction limits separately
        let policy2 = SecurityPolicy {
            max_instructions: 10,
            ..SecurityPolicy::default()
        };
        
        let context2 = SecurityContext::new(policy2);
        
        // Instruction limit
        for i in 0..10 {
            assert!(context2.track_instruction().is_ok(), "Failed at instruction {}", i);
        }
        assert!(context2.track_instruction().is_err()); // 11th instruction should fail
    }
    
    #[test]
    fn test_time_limits() {
        let context = SecurityContext::new(SecurityPolicy::default())
            .with_time_limit(Duration::from_millis(10));
        
        // Initially should pass
        assert!(context.check_limits().is_ok());
        
        // Sleep past the limit
        std::thread::sleep(Duration::from_millis(15));
        
        // Should now fail
        assert!(context.check_limits().is_err());
    }
    
    #[test]
    fn test_taint_tracking() {
        let tracker = TaintTracker::new();
        
        // Create taint IDs
        let id1 = tracker.create_taint_id();
        let id2 = tracker.create_taint_id();
        let id3 = tracker.create_taint_id();
        
        // Set taints
        tracker.set_taint(id1, TaintLevel::Sensitive);
        tracker.set_taint(id2, TaintLevel::Untrusted);
        
        // Check taints
        assert_eq!(tracker.get_taint(id1), TaintLevel::Sensitive);
        assert_eq!(tracker.get_taint(id2), TaintLevel::Untrusted);
        assert_eq!(tracker.get_taint(id3), TaintLevel::Clean); // Default
        
        // Propagate taint
        tracker.propagate_taint(id1, id3);
        assert_eq!(tracker.get_taint(id3), TaintLevel::Sensitive);
    }
    
    #[test]
    fn test_taint_flow_restrictions() {
        let tracker = TaintTracker::new();
        
        let sensitive_id = tracker.create_taint_id();
        let untrusted_id = tracker.create_taint_id();
        
        tracker.set_taint(sensitive_id, TaintLevel::Sensitive);
        tracker.set_taint(untrusted_id, TaintLevel::Untrusted);
        
        // Sensitive data cannot flow to certain contexts
        assert!(tracker.check_taint_flow(sensitive_id, "network").is_err());
        assert!(tracker.check_taint_flow(sensitive_id, "file").is_err());
        
        // Untrusted data cannot flow to eval/exec
        assert!(tracker.check_taint_flow(untrusted_id, "eval").is_err());
        assert!(tracker.check_taint_flow(untrusted_id, "exec").is_err());
        assert!(tracker.check_taint_flow(untrusted_id, "file").is_ok());
    }
    
    #[test]
    fn test_module_isolation() {
        let manager = SecurityManager::new(SecurityPolicy::default());
        let isolation = &manager.module_isolation;
        
        // Create module contexts
        let module1_policy = SecurityPolicy::sandbox();
        let module2_policy = SecurityPolicy::default();
        
        isolation.create_module_context("module1", module1_policy);
        isolation.create_module_context("module2", module2_policy);
        
        // Set module-specific globals
        isolation.set_module_global("module1", "x".to_string(), Value::Int(42));
        isolation.set_module_global("module2", "x".to_string(), Value::Int(100));
        
        // Check isolation
        assert_eq!(isolation.get_module_global("module1", "x"), Some(Value::Int(42)));
        assert_eq!(isolation.get_module_global("module2", "x"), Some(Value::Int(100)));
        assert_eq!(isolation.get_module_global("module1", "y"), None);
    }
    
    #[test]
    fn test_module_capabilities() {
        let manager = SecurityManager::new(SecurityPolicy::default());
        let isolation = &manager.module_isolation;
        
        isolation.create_module_context("trusted_module", SecurityPolicy::trusted());
        isolation.create_module_context("sandbox_module", SecurityPolicy::sandbox());
        
        // Note: Cannot directly add module capabilities without public API
        // This test would need the module isolation API to be extended
        // to support adding capabilities to modules
    }
    
    #[test]
    fn test_custom_capabilities() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Add custom capability
        let custom_cap = Capability::Custom("gpu_access".to_string());
        context.grant_capability(custom_cap.clone());
        
        assert!(context.has_capability(&custom_cap));
        assert!(!context.has_capability(&Capability::Custom("cpu_affinity".to_string())));
    }
    
    #[test]
    fn test_module_import_restrictions() {
        // Test with allowed list
        let mut policy = SecurityPolicy::default();
        policy.allowed_modules.insert("math".to_string());
        policy.allowed_modules.insert("string".to_string());
        
        let context = SecurityContext::new(policy);
        
        // Need to grant the capability first - grant them individually
        context.grant_capability(Capability::ModuleImport { modules: vec!["math".to_string()] });
        context.grant_capability(Capability::ModuleImport { modules: vec!["string".to_string()] });
        
        // Allowed modules
        assert!(context.check_module_import("math").is_ok());
        assert!(context.check_module_import("string").is_ok());
        
        // Not in allowed list (when allowed list is not empty, only those modules are allowed)
        assert!(context.check_module_import("network").is_err());
        
        // Test with denied list
        let mut policy2 = SecurityPolicy::default();
        policy2.denied_modules.insert("fs".to_string());
        policy2.denied_modules.insert("network".to_string());
        
        let context2 = SecurityContext::new(policy2);
        
        // Grant wildcard capability
        let wildcard = Capability::ModuleImport { modules: vec!["*".to_string()] };
        context2.grant_capability(wildcard);
        
        // Denied modules should still be denied
        assert!(context2.check_module_import("fs").is_err());
        assert!(context2.check_module_import("network").is_err());
        
        // Not denied modules should work with wildcard capability
        assert!(context2.check_module_import("math").is_ok());
    }
    
    #[test]
    fn test_thread_safety() {
        use std::thread;
        
        let context = Arc::new(SecurityContext::new(SecurityPolicy::default()));
        let mut handles = vec![];
        
        // Spawn multiple threads tracking resources
        for _ in 0..4 {
            let context_clone = Arc::clone(&context);
            let handle = thread::spawn(move || {
                for _ in 0..100 {
                    let _ = context_clone.track_instruction();
                    let _ = context_clone.track_allocation(100);
                }
            });
            handles.push(handle);
        }
        
        // Wait for all threads
        for handle in handles {
            handle.join().unwrap();
        }
    }
    
    #[test]
    fn test_termination() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Initially should pass
        assert!(context.check_limits().is_ok());
        
        // Terminate
        context.terminate();
        
        // Should fail after termination
        assert!(context.check_limits().is_err());
    }
    
    #[test]
    fn test_sanitized_taint() {
        let tracker = TaintTracker::new();
        
        let untrusted_id = tracker.create_taint_id();
        let sanitized_id = tracker.create_taint_id();
        
        tracker.set_taint(untrusted_id, TaintLevel::Untrusted);
        
        // Simulate sanitization
        tracker.set_taint(sanitized_id, TaintLevel::Sanitized);
        
        // Sanitized data has fewer restrictions
        assert!(tracker.check_taint_flow(sanitized_id, "eval").is_ok());
        assert!(tracker.check_taint_flow(untrusted_id, "eval").is_err());
    }
    
    #[test]
    fn test_capability_subsets() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Grant filesystem capability for specific paths
        let fs_tmp = Capability::FileSystem { paths: vec!["/tmp".to_string()] };
        let fs_home = Capability::FileSystem { paths: vec!["/home".to_string()] };
        
        context.grant_capability(fs_tmp.clone());
        
        assert!(context.has_capability(&fs_tmp));
        assert!(!context.has_capability(&fs_home));
    }
    
    #[test]
    fn test_capability_wildcards() {
        let context = SecurityContext::new(SecurityPolicy::default());
        
        // Grant wildcard module import
        let wildcard = Capability::ModuleImport { modules: vec!["*".to_string()] };
        context.grant_capability(wildcard.clone());
        
        assert!(context.has_capability(&wildcard));
        
        // Should allow any module when checking through check_module_import
        // (Note: actual wildcard logic is in check_module_import method)
    }
    
    #[test]
    fn test_security_manager_creation() {
        let sandbox = SecurityManager::sandbox();
        let trusted = SecurityManager::trusted();
        
        // Verify manager components are initialized
        assert_eq!(sandbox.taint_tracker.create_taint_id(), 1);
        assert_eq!(trusted.taint_tracker.create_taint_id(), 1);
        
        // Check that contexts have appropriate policies
        assert!(sandbox.context.check_limits().is_ok());
        assert!(trusted.context.check_limits().is_ok());
    }
}