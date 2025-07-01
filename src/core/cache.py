"""
ClaudeLang AST Cache

Caches parsed ASTs to eliminate parsing overhead for repeated execution.
"""

import hashlib
from typing import Dict, Optional, Tuple
import time


class ASTCache:
    """LRU cache for parsed AST graphs"""
    
    def __init__(self, max_size: int = 1000):
        self.max_size = max_size
        self.cache: Dict[str, Tuple['Graph', float]] = {}
        self.stats = {
            'hits': 0,
            'misses': 0,
            'evictions': 0
        }
    
    def _hash_code(self, code: str) -> str:
        """Generate hash for code string"""
        return hashlib.sha256(code.encode()).hexdigest()[:16]
    
    def get(self, code: str) -> Optional['Graph']:
        """Get cached graph if available"""
        key = self._hash_code(code)
        
        if key in self.cache:
            graph, timestamp = self.cache[key]
            self.stats['hits'] += 1
            # Update timestamp for LRU
            self.cache[key] = (graph, time.time())
            return graph
        
        self.stats['misses'] += 1
        return None
    
    def put(self, code: str, graph: 'Graph'):
        """Cache a parsed graph"""
        key = self._hash_code(code)
        
        # Evict oldest if at capacity
        if len(self.cache) >= self.max_size:
            oldest_key = min(self.cache.keys(), 
                           key=lambda k: self.cache[k][1])
            del self.cache[oldest_key]
            self.stats['evictions'] += 1
        
        self.cache[key] = (graph, time.time())
    
    def clear(self):
        """Clear the cache"""
        self.cache.clear()
        self.stats = {
            'hits': 0,
            'misses': 0,
            'evictions': 0
        }
    
    def get_stats(self) -> Dict[str, any]:
        """Get cache statistics"""
        total = self.stats['hits'] + self.stats['misses']
        hit_rate = self.stats['hits'] / total if total > 0 else 0
        
        return {
            **self.stats,
            'size': len(self.cache),
            'hit_rate': hit_rate
        }


# Global cache instance
_ast_cache = ASTCache()


def cached_parse(source: str) -> 'Graph':
    """Parse with caching"""
    # Check cache first
    graph = _ast_cache.get(source)
    if graph is not None:
        return graph
    
    # Parse and cache
    from ..parser.sexpr_parser import parse
    graph = parse(source)
    _ast_cache.put(source, graph)
    
    return graph


def get_cache_stats() -> Dict[str, any]:
    """Get global cache statistics"""
    return _ast_cache.get_stats()


def clear_cache():
    """Clear the global cache"""
    _ast_cache.clear()