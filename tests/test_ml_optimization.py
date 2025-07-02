"""
Test ML Optimization Learning
"""

import unittest
import tempfile
import shutil
import os
from unittest.mock import Mock, patch

# Check if numpy is available
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    # Mock numpy for tests
    class MockArray:
        def __init__(self, data):
            self.data = list(data) if hasattr(data, '__iter__') else [data]
        def __len__(self):
            return len(self.data)
        def __getitem__(self, idx):
            return self.data[idx]
    np = type('np', (), {'ndarray': MockArray, 'array': MockArray})()

from src.core.ast import Graph, ASTNode, NodeType, Variable, Literal, Application, Lambda
from src.semantic.ml_optimization_hints import (
    OptimizationHintType, ProgramFeatures, OptimizationHint,
    PerformanceProfile, OptimizationLearner, FeatureExtractor
)
from src.ml.model_trainer import (
    MLModelTrainer, TrainingExample, OptimizationModel, ModelMetrics
)
from src.ml.optimization_learner import (
    MLOptimizationEngine, OptimizationExperiment, create_ml_optimization_engine
)


class TestFeatureExtractor(unittest.TestCase):
    """Test feature extraction from programs"""
    
    def setUp(self):
        self.extractor = FeatureExtractor()
    
    def test_extract_simple_program(self):
        """Test feature extraction from simple program"""
        # Create simple graph: (+ 1 2)
        graph = Graph()
        
        plus = Variable(name="+")
        plus.id = graph.add_node(plus)
        
        one = Literal(value=1, literal_type="int")
        one.id = graph.add_node(one)
        
        two = Literal(value=2, literal_type="int")
        two.id = graph.add_node(two)
        
        app = Application(function_id=plus.id, argument_ids=[one.id, two.id])
        app.id = graph.add_node(app)
        graph.root_id = app.id
        
        features = self.extractor.extract(graph)
        
        self.assertEqual(features.node_count, 4)
        self.assertEqual(features.depth, 2)
        self.assertEqual(features.arithmetic_ops, 1)
        self.assertEqual(features.function_calls, 1)
        self.assertTrue(features.uses_integers)
        self.assertFalse(features.has_loops)
    
    def test_feature_vector_conversion(self):
        """Test conversion to ML feature vector"""
        features = ProgramFeatures(
            node_count=10,
            depth=3,
            branching_factor=2.0,
            cycle_count=0,
            arithmetic_ops=5,
            memory_ops=2,
            control_flow_ops=1,
            function_calls=3,
            data_dependencies=8,
            live_variables=4,
            register_pressure=0.25,
            has_recursion=False,
            has_loops=True,
            has_map_pattern=False,
            has_reduce_pattern=True,
            uses_integers=True,
            uses_floats=False,
            uses_lists=True,
            uses_higher_order=True,
            estimated_iterations=100,
            data_size_hint=1000,
            hotness_score=0.8
        )
        
        vector = features.to_vector()
        self.assertIsInstance(vector, np.ndarray)
        self.assertEqual(len(vector), 22)
        self.assertEqual(vector[0], 10)  # node_count
        self.assertEqual(vector[-1], 0.8)  # hotness_score


class TestOptimizationHints(unittest.TestCase):
    """Test optimization hint generation and application"""
    
    def test_hint_creation(self):
        """Test creating optimization hints"""
        hint = OptimizationHint(
            hint_type=OptimizationHintType.INLINE,
            node_id="node_123",
            confidence=0.85,
            parameters={"max_size": 50}
        )
        
        self.assertEqual(hint.hint_type, OptimizationHintType.INLINE)
        self.assertEqual(hint.node_id, "node_123")
        self.assertEqual(hint.confidence, 0.85)
        self.assertEqual(hint.parameters["max_size"], 50)
    
    def test_apply_threshold(self):
        """Test threshold checking for hints"""
        # High confidence inline hint
        hint1 = OptimizationHint(
            hint_type=OptimizationHintType.INLINE,
            node_id="node_1",
            confidence=0.8
        )
        self.assertTrue(hint1.apply_threshold())
        
        # Low confidence inline hint
        hint2 = OptimizationHint(
            hint_type=OptimizationHintType.INLINE,
            node_id="node_2",
            confidence=0.5
        )
        self.assertFalse(hint2.apply_threshold())
        
        # High confidence parallelize hint (higher threshold)
        hint3 = OptimizationHint(
            hint_type=OptimizationHintType.PARALLELIZE,
            node_id="node_3",
            confidence=0.95
        )
        self.assertTrue(hint3.apply_threshold())


class TestMLModelTrainer(unittest.TestCase):
    """Test ML model training"""
    
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.trainer = MLModelTrainer(models_dir=self.temp_dir)
    
    def tearDown(self):
        shutil.rmtree(self.temp_dir)
    
    def test_model_initialization(self):
        """Test that models are initialized for all hint types"""
        for hint_type in OptimizationHintType:
            self.assertIn(hint_type, self.trainer.models)
            model = self.trainer.models[hint_type]
            self.assertIsInstance(model, OptimizationModel)
            self.assertEqual(model.hint_type, hint_type)
            self.assertFalse(model.is_trained)
    
    def test_collect_training_example(self):
        """Test collecting training examples"""
        # Create simple graph
        graph = Graph()
        node = Literal(value=42, literal_type="int")
        node.id = graph.add_node(node)
        graph.root_id = node.id
        
        # Create mock trace
        trace = Mock()
        trace.get_node_trace.return_value = None
        
        # Create performance profile
        profile = PerformanceProfile(
            execution_time=0.1,
            memory_usage=1000,
            cache_misses=10,
            branch_mispredicts=5,
            optimizations_applied=["inline:node_1"],
            speedup=1.5
        )
        
        self.trainer.collect_training_example(
            graph, "test_program", trace, profile
        )
        
        self.assertEqual(len(self.trainer.training_data), 1)
        example = self.trainer.training_data[0]
        self.assertEqual(example.program_id, "test_program")
        self.assertEqual(example.performance.speedup, 1.5)
    
    def test_model_training_insufficient_data(self):
        """Test that models don't train with insufficient data"""
        # Try to train with no examples
        self.trainer.train_all_models(min_examples=10)
        
        # Models should remain untrained
        for model in self.trainer.models.values():
            self.assertFalse(model.is_trained)
    
    def test_model_saving_and_loading(self):
        """Test saving and loading trained models"""
        # Create a mock trained model
        model = self.trainer.models[OptimizationHintType.INLINE]
        model.is_trained = True
        model.metrics = ModelMetrics(
            accuracy=0.85,
            precision=0.8,
            recall=0.9,
            f1_score=0.85,
            cross_validation_score=0.83
        )
        
        # Save models
        self.trainer.save_models()
        
        # Create new trainer and load models
        new_trainer = MLModelTrainer(models_dir=self.temp_dir)
        new_trainer.load_models()
        
        # Check that inline model was loaded
        loaded_model = new_trainer.models[OptimizationHintType.INLINE]
        self.assertTrue(loaded_model.is_trained)
        self.assertAlmostEqual(loaded_model.metrics.accuracy, 0.85)


class TestOptimizationModel(unittest.TestCase):
    """Test individual optimization models"""
    
    def test_feature_preparation(self):
        """Test feature preparation for ML"""
        model = OptimizationModel(OptimizationHintType.VECTORIZE)
        
        prog_features = ProgramFeatures(
            node_count=10,
            depth=3,
            branching_factor=2.0,
            cycle_count=0,
            arithmetic_ops=5,
            memory_ops=2,
            control_flow_ops=1,
            function_calls=3,
            data_dependencies=8,
            live_variables=4,
            register_pressure=0.25,
            has_recursion=False,
            has_loops=True,
            has_map_pattern=True,
            has_reduce_pattern=False,
            uses_integers=True,
            uses_floats=False,
            uses_lists=True,
            uses_higher_order=True
        )
        
        node_features = {
            "child_count": 3,
            "depth": 2,
            "call_count": 5,
            "node_type": "APPLICATION"
        }
        
        features = model.prepare_features(prog_features, node_features)
        
        self.assertIsInstance(features, np.ndarray)
        self.assertEqual(len(features), 32)  # 22 program + 10 node features
    
    def test_untrained_prediction(self):
        """Test prediction from untrained model"""
        model = OptimizationModel(OptimizationHintType.MEMOIZE)
        
        prog_features = ProgramFeatures(
            node_count=5,
            depth=2,
            branching_factor=1.5,
            cycle_count=0,
            arithmetic_ops=2,
            memory_ops=0,
            control_flow_ops=0,
            function_calls=1,
            data_dependencies=3,
            live_variables=2,
            register_pressure=0.125,
            has_recursion=True,
            has_loops=False,
            has_map_pattern=False,
            has_reduce_pattern=False,
            uses_integers=True,
            uses_floats=False,
            uses_lists=False,
            uses_higher_order=False
        )
        
        node_features = {"node_type": "LAMBDA", "is_recursive": True}
        
        should_apply, confidence, speedup = model.predict(prog_features, node_features)
        
        # Untrained model should return default values
        self.assertIsInstance(should_apply, bool)
        self.assertIsInstance(confidence, float)
        self.assertIsInstance(speedup, float)
        # Heuristic model may predict speedup based on recursion for memoize
        if prog_features.has_recursion and model.hint_type == OptimizationHintType.MEMOIZE:
            self.assertGreater(speedup, 1.0)
        else:
            self.assertGreaterEqual(speedup, 1.0)


class TestMLOptimizationEngine(unittest.TestCase):
    """Test ML optimization engine"""
    
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.engine = MLOptimizationEngine(models_dir=self.temp_dir)
    
    def tearDown(self):
        shutil.rmtree(self.temp_dir)
    
    @patch('src.ml.optimization_learner.BytecodeCompiler')
    @patch('src.ml.optimization_learner.VM')
    def test_baseline_measurement(self, mock_vm_class, mock_compiler_class):
        """Test baseline performance measurement"""
        # Setup mocks
        mock_compiler = Mock()
        mock_compiler_class.return_value = mock_compiler
        mock_compiler.compile.return_value = Mock()  # bytecode
        
        mock_vm = Mock()
        mock_vm_class.return_value = mock_vm
        
        # Create simple graph
        graph = Graph()
        node = Literal(value=42, literal_type="int")
        node.id = graph.add_node(node)
        graph.root_id = node.id
        
        # Measure baseline
        baseline_time = self.engine._measure_baseline(graph, None)
        
        # Check that VM was used correctly
        self.assertEqual(mock_vm.reset.call_count, 4)  # 3 warmup + 1 actual
        self.assertEqual(mock_vm.run.call_count, 4)
        mock_compiler.compile.assert_called_once()
    
    def test_hint_combination(self):
        """Test combining ML and heuristic hints"""
        ml_hints = [
            OptimizationHint(
                hint_type=OptimizationHintType.INLINE,
                node_id="node_1",
                confidence=0.8
            ),
            OptimizationHint(
                hint_type=OptimizationHintType.VECTORIZE,
                node_id="node_2",
                confidence=0.9
            )
        ]
        
        heuristic_hints = [
            OptimizationHint(
                hint_type=OptimizationHintType.INLINE,
                node_id="node_1",
                confidence=0.6
            ),
            OptimizationHint(
                hint_type=OptimizationHintType.MEMOIZE,
                node_id="node_3",
                confidence=0.7
            )
        ]
        
        combined = self.engine._combine_hints(ml_hints, heuristic_hints)
        
        # Should have 3 unique hints
        self.assertEqual(len(combined), 3)
        
        # Should be sorted by confidence - inline hint may be boosted
        self.assertIn(combined[0].hint_type, [OptimizationHintType.VECTORIZE, OptimizationHintType.INLINE])
        
        # Find specific hints
        inline_hint = next(h for h in combined if h.hint_type == OptimizationHintType.INLINE)
        vectorize_hint = next(h for h in combined if h.hint_type == OptimizationHintType.VECTORIZE)
        memoize_hint = next(h for h in combined if h.hint_type == OptimizationHintType.MEMOIZE)
        
        # Inline hint should have boosted confidence
        self.assertGreater(inline_hint.confidence, 0.8)
        self.assertEqual(vectorize_hint.confidence, 0.9)
        self.assertEqual(memoize_hint.confidence, 0.7)
    
    def test_statistics_update(self):
        """Test updating optimization statistics"""
        experiment = OptimizationExperiment(
            program_id="test",
            baseline_time=1.0,
            optimized_time=0.5,
            speedup=2.0,
            applied_hints=[
                OptimizationHint(
                    hint_type=OptimizationHintType.INLINE,
                    node_id="node_1",
                    confidence=0.8
                )
            ],
            successful_optimizations=["inline:node_1"]
        )
        
        self.engine._update_statistics(experiment)
        
        self.assertEqual(self.engine.stats["programs_optimized"], 1)
        self.assertEqual(self.engine.stats["total_speedup"], 2.0)
        self.assertEqual(self.engine.stats["successful_hints"]["inline"], 1)
    
    def test_optimization_report_generation(self):
        """Test report generation"""
        # Add some fake statistics
        self.engine.stats["programs_optimized"] = 5
        self.engine.stats["total_speedup"] = 7.5
        self.engine.stats["successful_hints"]["inline"] = 3
        self.engine.stats["successful_hints"]["vectorize"] = 2
        self.engine.stats["failed_hints"]["parallelize"] = 1
        
        report = self.engine.generate_optimization_report()
        
        self.assertIn("Programs optimized: 5", report)
        self.assertIn("Average speedup: 1.50x", report)
        self.assertIn("inline:", report)
        self.assertIn("vectorize:", report)


class TestOnlineLearning(unittest.TestCase):
    """Test online learning system"""
    
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.trainer = MLModelTrainer(models_dir=self.temp_dir)
        from src.ml.model_trainer import OnlineLearner
        self.online_learner = OnlineLearner(self.trainer)
    
    def tearDown(self):
        shutil.rmtree(self.temp_dir)
    
    def test_observe_execution(self):
        """Test observing execution for learning"""
        graph = Graph()
        node = Literal(value=42, literal_type="int")
        node.id = graph.add_node(node)
        graph.root_id = node.id
        
        trace = Mock()
        trace.peak_memory = 1000
        trace.total_cache_misses = 10
        
        self.online_learner.observe_execution(
            graph,
            "test_prog",
            baseline_time=1.0,
            optimized_time=0.8,  # 20% improvement
            optimizations=["inline:node_1"],
            trace=trace
        )
        
        # Should have collected training example
        self.assertEqual(len(self.trainer.training_data), 1)
        example = self.trainer.training_data[0]
        self.assertEqual(example.program_id, "test_prog")
        self.assertAlmostEqual(example.performance.speedup, 1.25)


if __name__ == "__main__":
    unittest.main()