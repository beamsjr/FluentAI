"""
Tests for UI optimization learning system
"""

import unittest
import time
from unittest.mock import Mock, patch

from src.optimization.ui_optimizer import (
    UIOptimizer, RenderMetrics, create_ui_optimizer
)
from src.core.ast import UIComponent, PropDefinition


class TestUIOptimizer(unittest.TestCase):
    """Test UI optimization functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.optimizer = UIOptimizer()
    
    def test_record_render_metrics(self):
        """Test recording render metrics"""
        metrics = RenderMetrics(
            component_name="TestButton",
            render_time=12.5,
            props_changed={"text", "onClick"},
            state_accessed={"isHovered"},
            dom_operations=3,
            child_renders=1
        )
        
        self.optimizer.record_render(metrics)
        
        # Check history is recorded
        self.assertEqual(len(self.optimizer.render_history["TestButton"]), 1)
        self.assertEqual(
            self.optimizer.render_history["TestButton"][0].render_time, 
            12.5
        )
    
    def test_analyze_patterns(self):
        """Test pattern analysis"""
        # Record multiple renders
        for i in range(20):
            metrics = RenderMetrics(
                component_name="TestComponent",
                render_time=10 + (i % 3) * 5,  # Varying render times
                props_changed={"text"} if i % 2 == 0 else {"text", "style"},
                state_accessed={"count"},
                dom_operations=2,
                child_renders=0
            )
            self.optimizer.record_render(metrics)
            time.sleep(0.01)  # Small delay for interval tracking
        
        analysis = self.optimizer.analyze_patterns("TestComponent")
        
        # Check analysis results
        self.assertIn("prop_change_rates", analysis)
        self.assertIn("text", analysis["prop_change_rates"])
        self.assertIn("style", analysis["prop_change_rates"])
        
        # Text changes every time, style changes 50% of the time
        self.assertEqual(analysis["prop_change_rates"]["text"], 1.0)
        self.assertEqual(analysis["prop_change_rates"]["style"], 0.5)
        
        # Check render stats
        self.assertIn("render_stats", analysis)
        self.assertIn("mean", analysis["render_stats"])
        self.assertIn("p95", analysis["render_stats"])
    
    def test_generate_memoization_optimization(self):
        """Test memoization optimization generation"""
        # Create component AST
        component = UIComponent(
            name="StableComponent",
            props={
                "title": PropDefinition("string", True, None),
                "theme": PropDefinition("string", False, "light"),
                "onClick": PropDefinition("function", False, None)
            }
        )
        
        # Record renders with stable props
        for i in range(20):
            metrics = RenderMetrics(
                component_name="StableComponent",
                render_time=15,
                props_changed={"onClick"} if i % 10 == 0 else {"onClick", "title"},
                state_accessed=set(),
                dom_operations=5,
                child_renders=2
            )
            self.optimizer.record_render(metrics)
        
        optimizations = self.optimizer.generate_optimizations("StableComponent", component)
        
        # Should recommend memoizing theme (never changes)
        memo_strategies = [s for s in optimizations["strategies"] if s["type"] == "memoize"]
        self.assertEqual(len(memo_strategies), 1)
        self.assertIn("theme", memo_strategies[0]["props"])
    
    def test_generate_batching_optimization(self):
        """Test update batching optimization"""
        component = UIComponent(name="BurstyComponent")
        
        # Simulate burst updates
        for burst in range(5):
            # Quick burst of updates
            for i in range(5):
                metrics = RenderMetrics(
                    component_name="BurstyComponent",
                    render_time=8,
                    props_changed={"data"},
                    state_accessed={"items"},
                    dom_operations=10,
                    child_renders=5,
                    timestamp=time.time()
                )
                self.optimizer.record_render(metrics)
                time.sleep(0.005)  # 5ms between updates in burst
            
            time.sleep(0.5)  # Long pause between bursts
        
        optimizations = self.optimizer.generate_optimizations("BurstyComponent", component)
        
        # Should recommend batching
        batch_strategies = [s for s in optimizations["strategies"] if s["type"] == "batch_updates"]
        self.assertTrue(len(batch_strategies) > 0)
    
    def test_generate_splitting_optimization(self):
        """Test component splitting optimization"""
        component = UIComponent(
            name="HeavyComponent",
            props={
                "data": PropDefinition("object", True, None),
                "view": PropDefinition("string", False, "grid")
            }
        )
        
        # Record slow renders when data changes
        for i in range(15):
            props_changed = {"data"} if i % 3 == 0 else {"view"}
            render_time = 150 if "data" in props_changed else 20
            metrics = RenderMetrics(
                component_name="HeavyComponent",
                render_time=render_time,
                props_changed=props_changed,
                state_accessed=set(),
                dom_operations=50,
                child_renders=10
            )
            self.optimizer.record_render(metrics)
        
        optimizations = self.optimizer.generate_optimizations("HeavyComponent", component)
        
        # Should recommend splitting
        split_strategies = [s for s in optimizations["strategies"] if s["type"] == "split_component"]
        self.assertTrue(len(split_strategies) > 0)
    
    def test_compile_optimized_component(self):
        """Test optimized component code generation"""
        component = UIComponent(
            name="OptimizedButton",
            props={
                "text": PropDefinition("string", True, None),
                "theme": PropDefinition("string", False, "default"),
                "disabled": PropDefinition("bool", False, False)
            }
        )
        
        # Record patterns that suggest memoization
        for i in range(20):
            # Text changes occasionally, theme never changes, disabled rarely
            props_changed = set()
            if i % 5 == 0:
                props_changed.add("text")
            if i == 10:  # Only once
                props_changed.add("disabled")
            # theme never changes
                
            metrics = RenderMetrics(
                component_name="OptimizedButton",
                render_time=10,
                props_changed=props_changed,
                state_accessed=set(),
                dom_operations=2,
                child_renders=0
            )
            self.optimizer.record_render(metrics)
        
        from src.core.graph import Graph
        graph = Graph()
        
        optimized_code = self.optimizer.compile_optimized_component(component, graph)
        
        # Should generate memoization code
        self.assertIn("memoize", optimized_code)
        self.assertIn("OptimizedButton", optimized_code)
        # Should include stable props (theme never changes, disabled rarely changes)
        self.assertTrue("theme" in optimized_code or "disabled" in optimized_code)
    
    def test_runtime_optimization_config(self):
        """Test runtime optimization configuration generation"""
        # Add render data for multiple components
        components = ["FastComponent", "SlowComponent", "BurstyComponent"]
        
        for comp in components:
            for i in range(30):
                if comp == "SlowComponent":
                    render_time = 50 + (i % 2) * 30
                elif comp == "BurstyComponent":
                    render_time = 15
                else:
                    render_time = 5
                
                metrics = RenderMetrics(
                    component_name=comp,
                    render_time=render_time,
                    props_changed={"data"},
                    state_accessed={"state"},
                    dom_operations=5,
                    child_renders=2,
                    timestamp=time.time() + i * 0.01 if comp == "BurstyComponent" else time.time() + i
                )
                self.optimizer.record_render(metrics)
        
        config = self.optimizer.get_runtime_optimizations()
        
        # Should have configuration for slow component
        self.assertIn("components", config)
        self.assertIn("SlowComponent", config["components"])
        
        # Slow component should have throttling
        slow_config = config["components"]["SlowComponent"]
        self.assertIn("throttle", slow_config)
        self.assertGreater(slow_config["throttle"], 20)
    
    def test_model_persistence(self):
        """Test saving and loading optimizer model"""
        # Check if ML libraries are available
        from src.optimization.ui_optimizer import HAS_ML
        
        # Add some data
        for i in range(10):
            metrics = RenderMetrics(
                component_name="TestComp",
                render_time=10 + i,
                props_changed={"prop1"},
                state_accessed=set(),
                dom_operations=3,
                child_renders=1
            )
            self.optimizer.record_render(metrics)
        
        # Save model (should work even without ML libraries for basic data)
        import tempfile
        with tempfile.NamedTemporaryFile(delete=False) as tmp:
            temp_path = tmp.name
        
        try:
            self.optimizer.save_model(temp_path)
            
            # Verify data was saved
            self.assertEqual(len(self.optimizer.render_history["TestComp"]), 10)
            
            # Check that JSON file was created
            import os
            self.assertTrue(os.path.exists(temp_path + '.json'))
        finally:
            # Cleanup
            import os
            for ext in ['.json', '_model.pkl', '_scaler.pkl']:
                if os.path.exists(temp_path + ext):
                    os.unlink(temp_path + ext)
    
    def test_render_time_prediction(self):
        """Test render time prediction (requires sklearn)"""
        # Check if ML libraries are available
        from src.optimization.ui_optimizer import HAS_ML
        if not HAS_ML:
            self.skipTest("ML libraries (sklearn/numpy) not available")
            
        # Generate training data
        for i in range(200):
            props_count = i % 5
            state_count = i % 3
            render_time = 5 + props_count * 3 + state_count * 2 + (i % 10)
            
            metrics = RenderMetrics(
                component_name="PredictableComponent",
                render_time=render_time,
                props_changed={f"prop{j}" for j in range(props_count)},
                state_accessed={f"state{j}" for j in range(state_count)},
                dom_operations=props_count * 2,
                child_renders=state_count
            )
            self.optimizer.record_render(metrics)
        
        # Train model
        self.optimizer.train_render_predictor()
        
        # Test prediction
        predicted_time = self.optimizer.predict_render_time(
            "PredictableComponent",
            {"prop0", "prop1"},  # 2 props
            {"state0"}  # 1 state
        )
        
        # Should predict around 5 + 2*3 + 1*2 = 13ms (plus some variance)
        self.assertIsNotNone(predicted_time)
        self.assertGreater(predicted_time, 5)
        self.assertLess(predicted_time, 25)


if __name__ == '__main__':
    unittest.main()