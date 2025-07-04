//! Graph-based dependency injection for AI-first design
//! 
//! This module provides a graph-based DI system that integrates with
//! FluentAi's AST structure, making dependency relationships explicit
//! and analyzable by AI systems.

use fluentai_core::ast::{Graph, Node, NodeId};
use std::collections::HashMap;
use std::sync::Arc;

/// Service definition as an AST node
#[derive(Debug, Clone)]
pub struct ServiceNode {
    /// Unique identifier for this service
    pub id: NodeId,
    /// Service interface type (as a symbol node)
    pub interface: NodeId,
    /// Implementation type (as a symbol node)
    pub implementation: NodeId,
    /// Service lifetime metadata
    pub lifetime: ServiceLifetime,
    /// Dependencies (edges to other service nodes)
    pub dependencies: Vec<DependencyEdge>,
    /// AI-friendly metadata
    pub metadata: ServiceMetadata,
}

/// Dependency edge in the service graph
#[derive(Debug, Clone)]
pub struct DependencyEdge {
    /// Target service node
    pub target: NodeId,
    /// Dependency kind (constructor, property, method)
    pub kind: DependencyKind,
    /// Optional: parameter position for constructor dependencies
    pub position: Option<usize>,
    /// AI hints about this dependency
    pub hints: DependencyHints,
}

/// AI-friendly service metadata
#[derive(Debug, Clone, Default)]
pub struct ServiceMetadata {
    /// Embedding vector for semantic similarity
    pub embedding: Option<Vec<f32>>,
    /// Performance characteristics
    pub performance_hints: PerformanceHints,
    /// Usage statistics for optimization
    pub usage_stats: UsageStats,
    /// Semantic tags for categorization
    pub tags: Vec<String>,
    /// Version for semantic versioning
    pub semantic_version: SemanticVersion,
}

/// Performance hints for AI optimization
#[derive(Debug, Clone, Default)]
pub struct PerformanceHints {
    /// Estimated instantiation cost (microseconds)
    pub instantiation_cost: Option<f64>,
    /// Memory footprint (bytes)
    pub memory_footprint: Option<usize>,
    /// Whether this service is thread-safe
    pub thread_safe: bool,
    /// Whether this service performs I/O
    pub performs_io: bool,
    /// Cache-friendliness score (0-1)
    pub cache_friendly: Option<f32>,
}

/// Usage statistics for learning patterns
#[derive(Debug, Clone, Default)]
pub struct UsageStats {
    /// Number of times resolved
    pub resolution_count: u64,
    /// Average resolution time (microseconds)
    pub avg_resolution_time: f64,
    /// Common resolution patterns
    pub resolution_patterns: Vec<ResolutionPattern>,
}

/// Pattern of service resolution
#[derive(Debug, Clone)]
pub struct ResolutionPattern {
    /// Context in which this service is resolved
    pub context: String,
    /// Frequency of this pattern
    pub frequency: u32,
    /// Co-resolved services
    pub co_resolved: Vec<NodeId>,
}

/// Semantic version based on behavior
#[derive(Debug, Clone, Default)]
pub struct SemanticVersion {
    /// Major version (breaking changes)
    pub major: u32,
    /// Minor version (new features)
    pub minor: u32,
    /// Patch version (bug fixes)
    pub patch: u32,
    /// Behavioral hash for AI comparison
    pub behavior_hash: Option<u64>,
}

/// Dependency injection hints
#[derive(Debug, Clone, Default)]
pub struct DependencyHints {
    /// Whether this dependency is optional
    pub optional: bool,
    /// Whether to use lazy initialization
    pub lazy: bool,
    /// Preferred resolution strategy
    pub resolution_strategy: ResolutionStrategy,
    /// Fallback service if primary fails
    pub fallback: Option<NodeId>,
}

/// Resolution strategy for dependencies
#[derive(Debug, Clone, PartialEq)]
pub enum ResolutionStrategy {
    /// Always create new instance
    AlwaysNew,
    /// Prefer cached instance
    PreferCached,
    /// Use factory pattern
    Factory,
    /// Custom strategy (described by string)
    Custom(String),
}

impl Default for ResolutionStrategy {
    fn default() -> Self {
        ResolutionStrategy::PreferCached
    }
}

/// Service lifetime in graph representation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServiceLifetime {
    /// Transient - new instance each time
    Transient,
    /// Scoped - one instance per scope
    Scoped,
    /// Singleton - one instance globally
    Singleton,
    /// Custom lifetime with AI-defined semantics
    Custom(u32),
}

/// Dependency kind for better analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DependencyKind {
    /// Constructor injection
    Constructor,
    /// Property/setter injection
    Property,
    /// Method injection
    Method,
    /// Factory injection
    Factory,
}

/// Graph-based service container
pub struct GraphContainer {
    /// The service dependency graph
    pub graph: Graph,
    /// Service nodes indexed by ID
    pub services: HashMap<NodeId, ServiceNode>,
    /// Cached instances for singletons
    pub singletons: HashMap<NodeId, Arc<dyn std::any::Any + Send + Sync>>,
    /// Scoped instances
    pub scoped: HashMap<NodeId, Arc<dyn std::any::Any + Send + Sync>>,
    /// Factory functions indexed by service ID
    pub factories: HashMap<NodeId, Arc<dyn Fn(&GraphContainer) -> Box<dyn std::any::Any + Send + Sync> + Send + Sync>>,
    /// AI optimization cache
    pub optimization_cache: OptimizationCache,
}

/// Cache for AI-driven optimizations
#[derive(Default)]
pub struct OptimizationCache {
    /// Precomputed resolution paths
    pub resolution_paths: HashMap<NodeId, Vec<NodeId>>,
    /// Cycle detection results
    pub cycle_free_verified: HashMap<NodeId, bool>,
    /// Parallel resolution opportunities
    pub parallel_groups: Vec<Vec<NodeId>>,
    /// Hot path optimizations
    pub hot_paths: HashMap<NodeId, OptimizedPath>,
}

/// Optimized resolution path
#[derive(Debug, Clone)]
pub struct OptimizedPath {
    /// Pre-ordered resolution sequence
    pub sequence: Vec<NodeId>,
    /// Estimated total cost
    pub estimated_cost: f64,
    /// Parallelization opportunities
    pub parallel_points: Vec<usize>,
}

impl GraphContainer {
    /// Create a new graph-based container
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            services: HashMap::new(),
            singletons: HashMap::new(),
            scoped: HashMap::new(),
            factories: HashMap::new(),
            optimization_cache: OptimizationCache::default(),
        }
    }
    
    /// Register a service in the graph
    pub fn register_service(&mut self, service: ServiceNode) -> NodeId {
        let id = service.id;
        
        // Add to graph as a variable node (representing the service)
        let node = Node::Variable {
            name: format!("service_{}", id.get()),
        };
        let actual_id = self.graph.add_node(node);
        
        // Update service with actual ID from graph
        let mut service = service;
        service.id = actual_id;
        
        // Store service metadata
        self.services.insert(actual_id, service);
        
        // Invalidate optimization cache for this path
        self.optimization_cache.resolution_paths.remove(&actual_id);
        
        actual_id
    }
    
    /// Analyze service dependencies using graph algorithms
    pub fn analyze_dependencies(&self, service_id: NodeId) -> DependencyAnalysis {
        let mut analysis = DependencyAnalysis::default();
        
        // Detect cycles first
        analysis.has_cycles = self.has_circular_dependencies(service_id);
        
        // If there are cycles, we can't calculate depth or other metrics
        if analysis.has_cycles {
            return analysis;
        }
        
        // Calculate depth
        analysis.dependency_depth = self.calculate_dependency_depth(service_id);
        
        // Find all transitive dependencies
        analysis.transitive_deps = self.get_transitive_dependencies(service_id);
        
        // Identify parallel resolution opportunities
        analysis.parallel_groups = self.find_parallel_groups(service_id);
        
        // Calculate total instantiation cost
        analysis.total_cost = self.estimate_total_cost(service_id);
        
        analysis
    }
    
    /// Resolve a service using graph traversal
    pub fn resolve<T: 'static>(&mut self, service_id: NodeId) -> Option<Arc<T>> {
        // Check if we have an optimized path
        if let Some(path) = self.optimization_cache.resolution_paths.get(&service_id) {
            return self.resolve_optimized(service_id, path.clone());
        }
        
        // Otherwise, do standard graph-based resolution
        self.resolve_with_graph_traversal(service_id)
    }
    
    /// Check for circular dependencies
    fn has_circular_dependencies(&self, start: NodeId) -> bool {
        // Early return if start node doesn't exist
        if !self.services.contains_key(&start) {
            return false;
        }
        
        // Use DFS with visited set and recursion stack
        let mut visited = std::collections::HashSet::new();
        let mut rec_stack = std::collections::HashSet::new();
        
        self.dfs_cycle_check(start, &mut visited, &mut rec_stack)
    }
    
    fn dfs_cycle_check(
        &self, 
        node: NodeId, 
        visited: &mut std::collections::HashSet<NodeId>,
        rec_stack: &mut std::collections::HashSet<NodeId>
    ) -> bool {
        // If we're already in the recursion stack, we found a cycle
        if rec_stack.contains(&node) {
            return true;
        }
        
        // If already visited and not in recursion stack, no cycle through this path
        if visited.contains(&node) {
            return false;
        }
        
        // Mark as visited and add to recursion stack
        visited.insert(node);
        rec_stack.insert(node);
        
        // Check all dependencies
        if let Some(service) = self.services.get(&node) {
            for dep in &service.dependencies {
                // Only check dependencies that exist in our services
                if self.services.contains_key(&dep.target) {
                    if self.dfs_cycle_check(dep.target, visited, rec_stack) {
                        // Remove from recursion stack before returning
                        rec_stack.remove(&node);
                        return true;
                    }
                }
            }
        }
        
        // Remove from recursion stack before returning
        rec_stack.remove(&node);
        false
    }
    
    /// Calculate maximum dependency depth
    fn calculate_dependency_depth(&self, node: NodeId) -> usize {
        self.calculate_dependency_depth_with_visited(node, &mut HashMap::new())
    }
    
    fn calculate_dependency_depth_with_visited(
        &self, 
        node: NodeId, 
        visited: &mut HashMap<NodeId, usize>
    ) -> usize {
        if let Some(&depth) = visited.get(&node) {
            return depth;
        }
        
        let depth = if let Some(service) = self.services.get(&node) {
            if service.dependencies.is_empty() {
                0
            } else {
                service.dependencies.iter()
                    .filter(|dep| self.services.contains_key(&dep.target))
                    .map(|dep| self.calculate_dependency_depth_with_visited(dep.target, visited))
                    .max()
                    .unwrap_or(0) + 1
            }
        } else {
            0
        };
        
        visited.insert(node, depth);
        depth
    }
    
    /// Get all transitive dependencies
    fn get_transitive_dependencies(&self, node: NodeId) -> Vec<NodeId> {
        let mut deps = Vec::new();
        let mut visited = HashMap::new();
        self.collect_transitive_deps(node, &mut deps, &mut visited);
        deps
    }
    
    fn collect_transitive_deps(
        &self,
        node: NodeId,
        deps: &mut Vec<NodeId>,
        visited: &mut HashMap<NodeId, bool>
    ) {
        if *visited.get(&node).unwrap_or(&false) {
            return;
        }
        
        visited.insert(node, true);
        
        if let Some(service) = self.services.get(&node) {
            for dep in &service.dependencies {
                // Only add and recurse if the dependency exists as a service
                if self.services.contains_key(&dep.target) {
                    deps.push(dep.target);
                    self.collect_transitive_deps(dep.target, deps, visited);
                }
            }
        }
    }
    
    /// Find groups of dependencies that can be resolved in parallel
    fn find_parallel_groups(&self, node: NodeId) -> Vec<Vec<NodeId>> {
        // This would use topological sorting and level-based grouping
        // For now, return a simple implementation
        vec![self.get_transitive_dependencies(node)]
    }
    
    /// Estimate total instantiation cost
    fn estimate_total_cost(&self, node: NodeId) -> f64 {
        let mut total = 0.0;
        let mut visited = HashMap::new();
        self.calculate_cost_recursive(node, &mut total, &mut visited);
        total
    }
    
    fn calculate_cost_recursive(
        &self,
        node: NodeId,
        total: &mut f64,
        visited: &mut HashMap<NodeId, bool>
    ) {
        if *visited.get(&node).unwrap_or(&false) {
            return;
        }
        
        visited.insert(node, true);
        
        if let Some(service) = self.services.get(&node) {
            if let Some(cost) = service.metadata.performance_hints.instantiation_cost {
                *total += cost;
            }
            
            for dep in &service.dependencies {
                self.calculate_cost_recursive(dep.target, total, visited);
            }
        }
    }
    
    fn resolve_optimized<T: 'static>(&mut self, _service_id: NodeId, _path: Vec<NodeId>) -> Option<Arc<T>> {
        // Optimized resolution implementation
        None
    }
    
    fn resolve_with_graph_traversal<T: 'static>(&mut self, _service_id: NodeId) -> Option<Arc<T>> {
        // Standard graph traversal resolution
        None
    }
}

/// Result of dependency analysis
#[derive(Debug, Default)]
pub struct DependencyAnalysis {
    /// Whether there are circular dependencies
    pub has_cycles: bool,
    /// Maximum dependency depth
    pub dependency_depth: usize,
    /// All transitive dependencies
    pub transitive_deps: Vec<NodeId>,
    /// Groups that can be resolved in parallel
    pub parallel_groups: Vec<Vec<NodeId>>,
    /// Estimated total instantiation cost
    pub total_cost: f64,
}

/// Builder for graph-based services
pub struct ServiceGraphBuilder {
    container: GraphContainer,
}

impl ServiceGraphBuilder {
    pub fn new() -> Self {
        Self {
            container: GraphContainer::new(),
        }
    }
    
    /// Start building a new service
    pub fn service(&mut self, interface: &str) -> ServiceNodeBuilder {
        let interface_id = self.create_symbol_node(interface);
        ServiceNodeBuilder {
            builder: self,
            interface_id,
            implementation_id: None,
            lifetime: ServiceLifetime::Transient,
            dependencies: Vec::new(),
            metadata: ServiceMetadata::default(),
        }
    }
    
    /// Create a symbol node for type names
    fn create_symbol_node(&mut self, name: &str) -> NodeId {
        let node = Node::Variable {
            name: name.to_string(),
        };
        
        self.container.graph.add_node(node)
    }
    
    /// Build the final container
    pub fn build(self) -> GraphContainer {
        self.container
    }
}

#[cfg(test)]
mod tests;

/// Builder for individual service nodes
pub struct ServiceNodeBuilder<'a> {
    builder: &'a mut ServiceGraphBuilder,
    interface_id: NodeId,
    implementation_id: Option<NodeId>,
    lifetime: ServiceLifetime,
    dependencies: Vec<DependencyEdge>,
    metadata: ServiceMetadata,
}

impl<'a> ServiceNodeBuilder<'a> {
    /// Set the implementation type
    pub fn implementation(mut self, impl_name: &str) -> Self {
        self.implementation_id = Some(self.builder.create_symbol_node(impl_name));
        self
    }
    
    /// Set the service lifetime
    pub fn lifetime(mut self, lifetime: ServiceLifetime) -> Self {
        self.lifetime = lifetime;
        self
    }
    
    /// Add a dependency
    pub fn depends_on(mut self, service: &str, kind: DependencyKind) -> Self {
        let target = self.builder.create_symbol_node(service);
        self.dependencies.push(DependencyEdge {
            target,
            kind,
            position: None,
            hints: DependencyHints::default(),
        });
        self
    }
    
    /// Add AI metadata
    pub fn with_metadata(mut self, metadata: ServiceMetadata) -> Self {
        self.metadata = metadata;
        self
    }
    
    /// Register the service
    pub fn register(self) -> NodeId {
        // Create a temporary ID that will be replaced by graph.add_node
        let temp_id = NodeId::new(1).unwrap(); // Will be replaced
        
        let service = ServiceNode {
            id: temp_id,
            interface: self.interface_id,
            implementation: self.implementation_id.unwrap_or(self.interface_id),
            lifetime: self.lifetime,
            dependencies: self.dependencies,
            metadata: self.metadata,
        };
        
        self.builder.container.register_service(service)
    }
}