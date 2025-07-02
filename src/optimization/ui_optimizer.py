"""
UI Optimization Learning for ClaudeLang

This module implements machine learning-based optimization for UI rendering.
It learns from component update patterns to optimize virtual DOM diffing,
component re-renders, and state update batching.
"""

import json
import time
from typing import Dict, List, Any, Optional, Tuple, Set
from dataclasses import dataclass, field
from collections import defaultdict, deque
import os

# Make ML dependencies optional
try:
    import numpy as np
    from sklearn.ensemble import RandomForestRegressor
    from sklearn.preprocessing import StandardScaler
    import joblib
    HAS_ML = True
except ImportError:
    HAS_ML = False
    # Provide dummy implementations
    class RandomForestRegressor:
        def __init__(self, **kwargs):
            pass
        def fit(self, X, y):
            pass
        def predict(self, X):
            return [10.0]  # Default prediction
    
    class StandardScaler:
        def __init__(self):
            pass
        def fit_transform(self, X):
            return X
        def transform(self, X):
            return X

from ..core.ast import UIComponent, Effect, EffectType
from ..core.graph import Graph


@dataclass
class RenderMetrics:
    """Metrics for a component render"""
    component_name: str
    render_time: float
    props_changed: Set[str]
    state_accessed: Set[str]
    dom_operations: int
    child_renders: int
    timestamp: float = field(default_factory=time.time)


@dataclass
class UpdatePattern:
    """Pattern of component updates"""
    component_name: str
    prop_updates: List[Set[str]]  # History of prop changes
    state_updates: List[Set[str]]  # History of state changes
    render_times: List[float]
    update_intervals: List[float]


class UIOptimizer:
    """Learn and optimize UI rendering patterns"""
    
    def __init__(self, model_path: Optional[str] = None):
        self.render_history: Dict[str, deque] = defaultdict(lambda: deque(maxlen=1000))
        self.update_patterns: Dict[str, UpdatePattern] = {}
        self.optimization_rules: Dict[str, Dict[str, Any]] = {}
        
        # ML models for prediction
        self.render_time_model = RandomForestRegressor(n_estimators=100, random_state=42)
        self.scaler = StandardScaler()
        self.is_trained = False
        
        # Optimization thresholds
        self.batch_threshold = 16  # ms - batch updates within this window
        self.memo_threshold = 0.8  # memoize if prop change probability < this
        self.split_threshold = 100  # ms - split component if render time > this
        
        if model_path and os.path.exists(model_path):
            self.load_model(model_path)
    
    def record_render(self, metrics: RenderMetrics) -> None:
        """Record a component render for learning"""
        self.render_history[metrics.component_name].append(metrics)
        
        # Update patterns
        if metrics.component_name not in self.update_patterns:
            self.update_patterns[metrics.component_name] = UpdatePattern(
                component_name=metrics.component_name,
                prop_updates=[],
                state_updates=[],
                render_times=[],
                update_intervals=[]
            )
        
        pattern = self.update_patterns[metrics.component_name]
        pattern.prop_updates.append(metrics.props_changed)
        pattern.state_updates.append(metrics.state_accessed)
        pattern.render_times.append(metrics.render_time)
        
        # Calculate update interval
        if len(self.render_history[metrics.component_name]) > 1:
            prev_render = self.render_history[metrics.component_name][-2]
            interval = metrics.timestamp - prev_render.timestamp
            pattern.update_intervals.append(interval)
    
    def analyze_patterns(self, component_name: str) -> Dict[str, Any]:
        """Analyze update patterns for a component"""
        if component_name not in self.update_patterns:
            return {}
        
        pattern = self.update_patterns[component_name]
        history = list(self.render_history[component_name])
        
        if len(history) < 10:
            return {}  # Not enough data
        
        # Analyze prop change frequency
        prop_frequencies = defaultdict(int)
        for props in pattern.prop_updates:
            for prop in props:
                prop_frequencies[prop] += 1
        
        total_updates = len(pattern.prop_updates)
        prop_change_rates = {
            prop: count / total_updates 
            for prop, count in prop_frequencies.items()
        }
        
        # Analyze render time statistics
        if HAS_ML:
            render_times = np.array(pattern.render_times)
            render_stats = {
                'mean': np.mean(render_times),
                'std': np.std(render_times),
                'p95': np.percentile(render_times, 95),
                'max': np.max(render_times)
            }
        else:
            # Basic statistics without numpy
            render_times = pattern.render_times
            mean = sum(render_times) / len(render_times) if render_times else 0
            render_stats = {
                'mean': mean,
                'std': 0,  # Skip std calculation
                'p95': sorted(render_times)[int(len(render_times) * 0.95)] if render_times else 0,
                'max': max(render_times) if render_times else 0
            }
        
        # Analyze update intervals
        if pattern.update_intervals:
            if HAS_ML:
                intervals = np.array(pattern.update_intervals)
                interval_stats = {
                    'mean': np.mean(intervals),
                    'std': np.std(intervals),
                    'min': np.min(intervals)
                }
            else:
                intervals = pattern.update_intervals
                mean = sum(intervals) / len(intervals)
                interval_stats = {
                    'mean': mean,
                    'std': 0,
                    'min': min(intervals)
                }
        else:
            interval_stats = {}
        
        # Detect update bursts
        burst_threshold = 50  # ms
        bursts = sum(1 for i in pattern.update_intervals if i < burst_threshold)
        burst_rate = bursts / len(pattern.update_intervals) if pattern.update_intervals else 0
        
        return {
            'prop_change_rates': prop_change_rates,
            'render_stats': render_stats,
            'interval_stats': interval_stats,
            'burst_rate': burst_rate,
            'total_renders': total_updates
        }
    
    def generate_optimizations(self, component_name: str, 
                              component_ast: UIComponent) -> Dict[str, Any]:
        """Generate optimization recommendations for a component"""
        analysis = self.analyze_patterns(component_name)
        if not analysis:
            return {}
        
        optimizations = {
            'component': component_name,
            'strategies': []
        }
        
        # 1. Memoization recommendations
        if 'prop_change_rates' in analysis:
            # Get all component props
            all_props = set(component_ast.props.keys()) if hasattr(component_ast, 'props') else set()
            changed_props = set(analysis['prop_change_rates'].keys())
            
            # Props that never changed
            never_changed = all_props - changed_props
            
            # Props that rarely change
            rarely_changed = [
                prop for prop, rate in analysis['prop_change_rates'].items()
                if rate < self.memo_threshold
            ]
            
            stable_props = list(never_changed) + rarely_changed
            
            if stable_props:
                optimizations['strategies'].append({
                    'type': 'memoize',
                    'props': stable_props,
                    'reason': f'Props {stable_props} change infrequently or never'
                })
        
        # 2. Update batching recommendations
        if analysis.get('burst_rate', 0) > 0.3:
            optimizations['strategies'].append({
                'type': 'batch_updates',
                'threshold': self.batch_threshold,
                'reason': f'{analysis["burst_rate"]:.0%} of updates happen in bursts'
            })
        
        # 3. Component splitting recommendations
        render_stats = analysis.get('render_stats', {})
        if render_stats.get('p95', 0) > self.split_threshold:
            # Analyze which props/state cause slow renders
            slow_renders = [
                m for m in self.render_history[component_name]
                if m.render_time > self.split_threshold
            ]
            
            # Find common props in slow renders
            common_props = set()
            if slow_renders:
                common_props = set.intersection(*[r.props_changed for r in slow_renders])
            
            if common_props:
                optimizations['strategies'].append({
                    'type': 'split_component',
                    'extract_props': list(common_props),
                    'reason': f'95th percentile render time is {render_stats["p95"]:.1f}ms'
                })
        
        # 4. Lazy loading recommendations
        if render_stats.get('mean', 0) > 50 and len(component_ast.props) > 5:
            optimizations['strategies'].append({
                'type': 'lazy_load',
                'reason': 'Heavy component with many props'
            })
        
        # 5. State coalescing recommendations
        pattern = self.update_patterns[component_name]
        if len(pattern.state_updates) > 0:
            # Find frequently co-occurring state updates
            state_pairs = defaultdict(int)
            for states in pattern.state_updates:
                state_list = list(states)
                for i in range(len(state_list)):
                    for j in range(i + 1, len(state_list)):
                        pair = tuple(sorted([state_list[i], state_list[j]]))
                        state_pairs[pair] += 1
            
            # Recommend coalescing frequently co-occurring states
            total_updates = len(pattern.state_updates)
            correlated_states = [
                pair for pair, count in state_pairs.items()
                if count / total_updates > 0.5
            ]
            
            if correlated_states:
                optimizations['strategies'].append({
                    'type': 'coalesce_state',
                    'state_groups': correlated_states,
                    'reason': 'These states frequently update together'
                })
        
        return optimizations
    
    def train_render_predictor(self) -> None:
        """Train ML model to predict render times"""
        # Collect training data
        X = []
        y = []
        
        for component_name, history in self.render_history.items():
            for metrics in history:
                features = self._extract_features(metrics)
                X.append(features)
                y.append(metrics.render_time)
        
        if len(X) < 100 or not HAS_ML:
            return  # Not enough data or ML not available
        
        # Train model
        X = np.array(X)
        y = np.array(y)
        
        X_scaled = self.scaler.fit_transform(X)
        self.render_time_model.fit(X_scaled, y)
        self.is_trained = True
    
    def predict_render_time(self, component_name: str, 
                          props_changed: Set[str],
                          state_accessed: Set[str]) -> Optional[float]:
        """Predict render time for given changes"""
        if not self.is_trained:
            return None
        
        # Create dummy metrics for feature extraction
        metrics = RenderMetrics(
            component_name=component_name,
            render_time=0,
            props_changed=props_changed,
            state_accessed=state_accessed,
            dom_operations=0,
            child_renders=0
        )
        
        features = self._extract_features(metrics)
        features_scaled = self.scaler.transform([features])
        
        return self.render_time_model.predict(features_scaled)[0]
    
    def _extract_features(self, metrics: RenderMetrics) -> List[float]:
        """Extract ML features from render metrics"""
        # Get historical statistics
        history = list(self.render_history[metrics.component_name])
        
        if HAS_ML and history:
            recent_times = [m.render_time for m in history[-10:]]
            mean_time = np.mean(recent_times)
            std_time = np.std(recent_times)
        else:
            recent_times = [m.render_time for m in history[-10:]] if history else []
            mean_time = sum(recent_times) / len(recent_times) if recent_times else 0
            std_time = 0
        
        features = [
            len(metrics.props_changed),
            len(metrics.state_accessed),
            metrics.dom_operations,
            metrics.child_renders,
            len(history),  # Component render count
            mean_time,
            std_time,
        ]
        
        # Add prop-specific features (top 10 most common props)
        pattern = self.update_patterns.get(metrics.component_name)
        if pattern:
            prop_frequencies = defaultdict(int)
            for props in pattern.prop_updates:
                for prop in props:
                    prop_frequencies[prop] += 1
            
            top_props = sorted(prop_frequencies.items(), 
                             key=lambda x: x[1], reverse=True)[:10]
            
            for prop, _ in top_props:
                features.append(1.0 if prop in metrics.props_changed else 0.0)
            
            # Pad if fewer than 10 props
            features.extend([0.0] * (10 - len(top_props)))
        else:
            features.extend([0.0] * 10)
        
        return features
    
    def compile_optimized_component(self, component: UIComponent, 
                                  graph: Graph) -> str:
        """Generate optimized component code based on learned patterns"""
        optimizations = self.generate_optimizations(component.name, component)
        
        if not optimizations.get('strategies'):
            return ""  # No optimizations needed
        
        code_snippets = []
        
        for strategy in optimizations['strategies']:
            if strategy['type'] == 'memoize':
                # Generate memoization wrapper
                props = strategy['props']
                code_snippets.append(f"""
// Memoize stable props: {', '.join(props)}
const memoized{component.name} = ClaudeLang.memoize(
  {component.name},
  [{', '.join(f'"{p}"' for p in props)}]
);""")
            
            elif strategy['type'] == 'batch_updates':
                # Generate update batching code
                threshold = strategy['threshold']
                code_snippets.append(f"""
// Batch updates within {threshold}ms window
ClaudeLang.configureBatching('{component.name}', {{
  threshold: {threshold},
  maxBatchSize: 10
}});""")
            
            elif strategy['type'] == 'lazy_load':
                # Generate lazy loading wrapper
                code_snippets.append(f"""
// Lazy load heavy component
const Lazy{component.name} = ClaudeLang.lazy(() => 
  Promise.resolve({component.name})
);""")
            
            elif strategy['type'] == 'coalesce_state':
                # Generate state coalescing helpers
                for state_group in strategy['state_groups']:
                    code_snippets.append(f"""
// Coalesce correlated states: {', '.join(state_group)}
const use{component.name}State = () => {{
  const [combinedState, setCombinedState] = ClaudeLang.useState({{
    {', '.join(f'{s}: null' for s in state_group)}
  }});
  
  const updateStates = (updates) => {{
    setCombinedState(prev => ({{...prev, ...updates}}));
  }};
  
  return [combinedState, updateStates];
}};""")
        
        return '\n'.join(code_snippets)
    
    def get_runtime_optimizations(self) -> Dict[str, Any]:
        """Get runtime optimization configuration"""
        config = {
            'components': {}
        }
        
        for component_name, pattern in self.update_patterns.items():
            analysis = self.analyze_patterns(component_name)
            if not analysis:
                continue
            
            component_config = {}
            
            # Configure render throttling
            render_stats = analysis.get('render_stats', {})
            if render_stats.get('mean', 0) > 20:
                component_config['throttle'] = max(16, render_stats['mean'] * 0.8)
            
            # Configure prop comparison
            prop_rates = analysis.get('prop_change_rates', {})
            if prop_rates:
                # Deep compare only frequently changing props
                component_config['shallowCompareProps'] = [
                    prop for prop, rate in prop_rates.items()
                    if rate < 0.3
                ]
            
            # Configure update batching
            if analysis.get('burst_rate', 0) > 0.3:
                component_config['batchUpdates'] = True
                component_config['batchWindow'] = self.batch_threshold
            
            if component_config:
                config['components'][component_name] = component_config
        
        return config
    
    def save_model(self, path: str) -> None:
        """Save trained model and optimization data"""
        # Convert render_history deques to lists for JSON serialization
        history_data = {}
        for component, deque_data in self.render_history.items():
            history_data[component] = [
                {
                    'component_name': m.component_name,
                    'render_time': m.render_time,
                    'props_changed': list(m.props_changed),
                    'state_accessed': list(m.state_accessed),
                    'dom_operations': m.dom_operations,
                    'child_renders': m.child_renders,
                    'timestamp': m.timestamp
                }
                for m in deque_data
            ]
        
        # Convert update_patterns to serializable format
        patterns_data = {}
        for component, pattern in self.update_patterns.items():
            patterns_data[component] = {
                'component_name': pattern.component_name,
                'prop_updates': [list(s) for s in pattern.prop_updates],
                'state_updates': [list(s) for s in pattern.state_updates],
                'render_times': pattern.render_times,
                'update_intervals': pattern.update_intervals
            }
        
        data = {
            'render_history': history_data,
            'update_patterns': patterns_data,
            'optimization_rules': self.optimization_rules,
            'is_trained': self.is_trained
        }
        
        with open(path + '.json', 'w') as f:
            json.dump(data, f, default=str)
        
        if self.is_trained and HAS_ML:
            joblib.dump(self.render_time_model, path + '_model.pkl')
            joblib.dump(self.scaler, path + '_scaler.pkl')
    
    def load_model(self, path: str) -> None:
        """Load trained model and optimization data"""
        with open(path + '.json', 'r') as f:
            data = json.load(f)
        
        # Restore render history with deques
        self.render_history = defaultdict(lambda: deque(maxlen=1000))
        for component, history in data['render_history'].items():
            for metrics_dict in history:
                metrics = RenderMetrics(**metrics_dict)
                self.render_history[component].append(metrics)
        
        self.update_patterns = {
            k: UpdatePattern(**v) for k, v in data['update_patterns'].items()
        }
        self.optimization_rules = data['optimization_rules']
        self.is_trained = data['is_trained']
        
        if self.is_trained and os.path.exists(path + '_model.pkl'):
            self.render_time_model = joblib.load(path + '_model.pkl')
            self.scaler = joblib.load(path + '_scaler.pkl')


def create_ui_optimizer() -> UIOptimizer:
    """Create and configure a UI optimizer instance"""
    model_path = os.path.join(os.path.dirname(__file__), 'ui_optimizer')
    return UIOptimizer(model_path)