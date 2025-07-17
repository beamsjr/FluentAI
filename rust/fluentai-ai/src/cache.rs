//! Caching layer for AI analysis results

use crate::error::Result;
use crate::analysis::AiAnalysisResult;
use dashmap::DashMap;
use fluentai_core::ast::{Graph, NodeId};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tracing::{debug, warn};

/// Cache for AI analysis results
pub struct AiCache {
    /// Analysis results cache
    analysis_cache: DashMap<GraphHash, CachedResult<AiAnalysisResult>>,
    /// Embedding cache
    embedding_cache: DashMap<GraphHash, CachedResult<HashMap<NodeId, Vec<f32>>>>,
    /// Cache configuration
    config: CacheConfig,
    /// Cache statistics
    stats: Arc<RwLock<CacheStats>>,
}

/// Configuration for the cache
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Maximum number of cached analysis results
    pub max_analysis_entries: usize,
    /// Maximum number of cached embeddings
    pub max_embedding_entries: usize,
    /// Time to live for cache entries
    pub ttl: Duration,
    /// Enable disk persistence
    pub enable_persistence: bool,
    /// Cache directory for persistence
    pub cache_dir: PathBuf,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            max_analysis_entries: 1000,
            max_embedding_entries: 5000,
            ttl: Duration::from_secs(3600), // 1 hour
            enable_persistence: false,
            cache_dir: PathBuf::from(".fluentai_cache"),
        }
    }
}

/// Cached result with metadata
#[derive(Debug, Clone)]
struct CachedResult<T> {
    /// The cached value
    value: T,
    /// When the result was cached
    timestamp: Instant,
    /// Number of times accessed
    access_count: u64,
    /// Graph size when cached
    graph_size: usize,
}

/// Hash of a graph for caching
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GraphHash(u64);

/// Cache statistics
#[derive(Debug, Default)]
pub struct CacheStats {
    /// Total hits
    pub hits: u64,
    /// Total misses
    pub misses: u64,
    /// Total evictions
    pub evictions: u64,
    /// Average access time
    pub avg_access_time: Duration,
    /// Total access time
    total_access_time: Duration,
    /// Number of accesses
    access_count: u64,
}

impl AiCache {
    /// Create a new cache
    pub fn new(config: CacheConfig) -> Self {
        // Create cache directory if persistence is enabled
        if config.enable_persistence {
            if let Err(e) = std::fs::create_dir_all(&config.cache_dir) {
                warn!("Failed to create cache directory: {}", e);
            }
        }
        
        Self {
            analysis_cache: DashMap::new(),
            embedding_cache: DashMap::new(),
            config,
            stats: Arc::new(RwLock::new(CacheStats::default())),
        }
    }
    
    /// Get or compute analysis result
    pub fn get_or_compute_analysis<F>(
        &self,
        graph: &Graph,
        compute: F,
    ) -> Result<AiAnalysisResult>
    where
        F: FnOnce() -> Result<AiAnalysisResult>,
    {
        let start = Instant::now();
        let hash = self.compute_graph_hash(graph);
        
        // Check cache
        if let Some(entry) = self.analysis_cache.get(&hash) {
            if self.is_valid_entry(&entry) {
                self.record_hit(start);
                return Ok(entry.value.clone());
            }
        }
        
        // Compute new result
        self.record_miss(start);
        let result = compute()?;
        
        // Cache the result
        self.cache_analysis(hash, result.clone(), graph.node_ids().count());
        
        Ok(result)
    }
    
    /// Get or compute embeddings
    pub fn get_or_compute_embeddings<F>(
        &self,
        graph: &Graph,
        compute: F,
    ) -> Result<HashMap<NodeId, Vec<f32>>>
    where
        F: FnOnce() -> Result<HashMap<NodeId, Vec<f32>>>,
    {
        let start = Instant::now();
        let hash = self.compute_graph_hash(graph);
        
        // Check cache
        if let Some(entry) = self.embedding_cache.get(&hash) {
            if self.is_valid_entry(&entry) {
                self.record_hit(start);
                return Ok(entry.value.clone());
            }
        }
        
        // Compute new result
        self.record_miss(start);
        let result = compute()?;
        
        // Cache the result
        self.cache_embeddings(hash, result.clone(), graph.node_ids().count());
        
        Ok(result)
    }
    
    /// Clear the cache
    pub fn clear(&self) {
        self.analysis_cache.clear();
        self.embedding_cache.clear();
        debug!("Cache cleared");
    }
    
    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let stats = self.stats.read();
        CacheStats {
            hits: stats.hits,
            misses: stats.misses,
            evictions: stats.evictions,
            avg_access_time: stats.avg_access_time,
            total_access_time: stats.total_access_time,
            access_count: stats.access_count,
        }
    }
    
    /// Evict expired entries
    pub fn evict_expired(&self) {
        let now = Instant::now();
        let mut evicted = 0;
        
        // Evict from analysis cache
        self.analysis_cache.retain(|_, entry| {
            let keep = now.duration_since(entry.timestamp) < self.config.ttl;
            if !keep {
                evicted += 1;
            }
            keep
        });
        
        // Evict from embedding cache
        self.embedding_cache.retain(|_, entry| {
            let keep = now.duration_since(entry.timestamp) < self.config.ttl;
            if !keep {
                evicted += 1;
            }
            keep
        });
        
        if evicted > 0 {
            let mut stats = self.stats.write();
            stats.evictions += evicted;
            debug!("Evicted {} expired entries", evicted);
        }
    }
    
    /// Save cache to disk
    pub fn save_to_disk(&self) -> Result<()> {
        if !self.config.enable_persistence {
            return Ok(());
        }
        
        // TODO: Implement serialization
        warn!("Cache persistence not yet implemented");
        Ok(())
    }
    
    /// Load cache from disk
    pub fn load_from_disk(&self) -> Result<()> {
        if !self.config.enable_persistence {
            return Ok(());
        }
        
        // TODO: Implement deserialization
        warn!("Cache persistence not yet implemented");
        Ok(())
    }
    
    /// Compute hash of a graph
    fn compute_graph_hash(&self, graph: &Graph) -> GraphHash {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        
        // Hash graph structure
        for node_id in graph.node_ids() {
            node_id.hash(&mut hasher);
            
            if let Some(node) = graph.get_node(node_id) {
                // Hash node type
                std::mem::discriminant(node).hash(&mut hasher);
                
                // Hash node-specific data
                match node {
                    fluentai_core::ast::Node::Variable { name } => {
                        name.hash(&mut hasher);
                    }
                    fluentai_core::ast::Node::Literal(lit) => {
                        match lit {
                            fluentai_core::ast::Literal::Integer(n) => n.hash(&mut hasher),
                            fluentai_core::ast::Literal::Float(f) => {
                                // Convert float to bits for hashing
                                f.to_bits().hash(&mut hasher);
                            }
                            fluentai_core::ast::Literal::String(s) => s.hash(&mut hasher),
                            fluentai_core::ast::Literal::Boolean(b) => b.hash(&mut hasher),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            
            // Hash edges
            for child in graph.children(node_id) {
                child.hash(&mut hasher);
            }
        }
        
        GraphHash(hasher.finish())
    }
    
    /// Check if a cache entry is still valid
    fn is_valid_entry<T>(&self, entry: &CachedResult<T>) -> bool {
        let age = Instant::now().duration_since(entry.timestamp);
        age < self.config.ttl
    }
    
    /// Cache analysis result
    fn cache_analysis(&self, hash: GraphHash, result: AiAnalysisResult, graph_size: usize) {
        // Check cache size and evict if necessary
        if self.analysis_cache.len() >= self.config.max_analysis_entries {
            self.evict_least_used_analysis();
        }
        
        let entry = CachedResult {
            value: result,
            timestamp: Instant::now(),
            access_count: 1,
            graph_size,
        };
        
        self.analysis_cache.insert(hash, entry);
    }
    
    /// Cache embeddings
    fn cache_embeddings(&self, hash: GraphHash, embeddings: HashMap<NodeId, Vec<f32>>, graph_size: usize) {
        // Check cache size and evict if necessary
        if self.embedding_cache.len() >= self.config.max_embedding_entries {
            self.evict_least_used_embeddings();
        }
        
        let entry = CachedResult {
            value: embeddings,
            timestamp: Instant::now(),
            access_count: 1,
            graph_size,
        };
        
        self.embedding_cache.insert(hash, entry);
    }
    
    /// Evict least recently used analysis entry
    fn evict_least_used_analysis(&self) {
        if let Some((hash, _)) = self.analysis_cache
            .iter()
            .min_by_key(|entry| entry.value().access_count)
            .map(|entry| (*entry.key(), entry.value().clone()))
        {
            self.analysis_cache.remove(&hash);
            let mut stats = self.stats.write();
            stats.evictions += 1;
        }
    }
    
    /// Evict least recently used embedding entry
    fn evict_least_used_embeddings(&self) {
        if let Some((hash, _)) = self.embedding_cache
            .iter()
            .min_by_key(|entry| entry.value().access_count)
            .map(|entry| (*entry.key(), entry.value().clone()))
        {
            self.embedding_cache.remove(&hash);
            let mut stats = self.stats.write();
            stats.evictions += 1;
        }
    }
    
    /// Record a cache hit
    fn record_hit(&self, start: Instant) {
        let duration = start.elapsed();
        let mut stats = self.stats.write();
        stats.hits += 1;
        stats.access_count += 1;
        stats.total_access_time += duration;
        stats.avg_access_time = stats.total_access_time / stats.access_count as u32;
    }
    
    /// Record a cache miss
    fn record_miss(&self, start: Instant) {
        let duration = start.elapsed();
        let mut stats = self.stats.write();
        stats.misses += 1;
        stats.access_count += 1;
        stats.total_access_time += duration;
        stats.avg_access_time = stats.total_access_time / stats.access_count as u32;
    }
}

/// Background cache manager
pub struct CacheManager {
    cache: Arc<AiCache>,
    #[cfg(feature = "parallel")]
    eviction_handle: Option<std::thread::JoinHandle<()>>,
}

impl CacheManager {
    /// Create a new cache manager
    pub fn new(cache: Arc<AiCache>) -> Self {
        Self {
            cache,
            #[cfg(feature = "parallel")]
            eviction_handle: None,
        }
    }
    
    /// Start background eviction thread
    #[cfg(feature = "parallel")]
    pub fn start_background_eviction(&mut self, interval: Duration) {
        let cache = self.cache.clone();
        
        let handle = std::thread::spawn(move || {
            loop {
                std::thread::sleep(interval);
                cache.evict_expired();
                
                // Also save to disk periodically if enabled
                if let Err(e) = cache.save_to_disk() {
                    warn!("Failed to save cache to disk: {}", e);
                }
            }
        });
        
        self.eviction_handle = Some(handle);
    }
    
    /// Stop background eviction
    #[cfg(feature = "parallel")]
    pub fn stop_background_eviction(&mut self) {
        if let Some(handle) = self.eviction_handle.take() {
            // Note: In production, you'd want a proper shutdown mechanism
            drop(handle);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{BaseAnalyzer, AnalysisConfig, AstAnalyzer};
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_cache_hit_miss() {
        let cache = AiCache::new(CacheConfig::default());
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let analyzer = BaseAnalyzer::new(AnalysisConfig::default());
        
        // First call - should be a miss
        let result1 = cache.get_or_compute_analysis(&graph, || {
            analyzer.analyze(&graph)
        }).unwrap();
        
        // Second call - should be a hit
        let result2 = cache.get_or_compute_analysis(&graph, || {
            panic!("Should not be called - should hit cache");
        }).unwrap();
        
        assert_eq!(result1.performance_score, result2.performance_score);
        
        let stats = cache.stats();
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }
    
    #[test]
    fn test_cache_eviction() {
        let mut config = CacheConfig::default();
        config.max_analysis_entries = 2;
        let cache = AiCache::new(config);
        
        let analyzer = BaseAnalyzer::new(AnalysisConfig::default());
        
        // Add 3 different graphs
        for i in 0..3 {
            let mut graph = Graph::new();
            graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
            
            cache.get_or_compute_analysis(&graph, || {
                analyzer.analyze(&graph)
            }).unwrap();
        }
        
        // Cache should have evicted one entry
        let stats = cache.stats();
        assert_eq!(stats.evictions, 1);
    }
    
    #[test]
    fn test_cache_expiration() {
        let mut config = CacheConfig::default();
        config.ttl = Duration::from_millis(100);
        let cache = AiCache::new(config);
        
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let analyzer = BaseAnalyzer::new(AnalysisConfig::default());
        
        // Cache a result
        cache.get_or_compute_analysis(&graph, || {
            analyzer.analyze(&graph)
        }).unwrap();
        
        // Wait for expiration
        std::thread::sleep(Duration::from_millis(150));
        
        // Should recompute due to expiration
        let mut computed = false;
        cache.get_or_compute_analysis(&graph, || {
            computed = true;
            analyzer.analyze(&graph)
        }).unwrap();
        
        assert!(computed);
    }
}