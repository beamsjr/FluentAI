"""
ClaudeLang ML Model Trainer

This module implements the training system for learning optimization patterns
from program execution traces and performance data.
"""

import os
import json
import pickle
from typing import Dict, List, Tuple, Optional, Any, Set
from dataclasses import dataclass, field
from datetime import datetime
from collections import defaultdict

# Try to import numpy - basic array operations
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    print("Warning: numpy not available. ML features will use Python lists.")
    # Basic fallback for np.mean
    def np_mean(lst):
        return sum(lst) / len(lst) if lst else 0
    np = type('np', (), {'mean': np_mean, 'array': list, 'concatenate': lambda x: sum(x, [])})()

# Try to import sklearn - ML features will be disabled if not available
try:
    import sklearn
    from sklearn.ensemble import RandomForestClassifier, GradientBoostingRegressor
    from sklearn.neural_network import MLPClassifier
    from sklearn.preprocessing import StandardScaler
    from sklearn.model_selection import train_test_split, cross_val_score
    from sklearn.metrics import precision_recall_fscore_support
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False
    print("Warning: scikit-learn not available. ML optimization features will be limited.")
    print("To enable full ML optimization, install scikit-learn: pip install scikit-learn")

from ..semantic.ml_optimization_hints import (
    OptimizationHintType, ProgramFeatures, OptimizationHint,
    PerformanceProfile, FeatureExtractor
)
from ..core.ast import Graph
from ..performance.execution_trace import ExecutionTrace


@dataclass
class TrainingExample:
    """A single training example"""
    program_id: str
    features: ProgramFeatures
    node_features: Dict[str, Dict[str, Any]]  # node_id -> features
    applied_optimizations: List[Tuple[str, OptimizationHintType]]  # (node_id, hint_type)
    performance: PerformanceProfile
    execution_trace: Optional[ExecutionTrace] = None


@dataclass
class ModelMetrics:
    """Metrics for a trained model"""
    accuracy: float
    precision: float
    recall: float
    f1_score: float
    cross_validation_score: float
    feature_importance: Dict[str, float] = field(default_factory=dict)
    confusion_matrix: Optional[np.ndarray] = None


class OptimizationModel:
    """ML model for a specific optimization type"""
    
    def __init__(self, hint_type: OptimizationHintType):
        self.hint_type = hint_type
        self.is_trained = False
        self.metrics: Optional[ModelMetrics] = None
        self.feature_names: List[str] = []
        
        if SKLEARN_AVAILABLE:
            self.classifier = RandomForestClassifier(
                n_estimators=100,
                max_depth=10,
                random_state=42
            )
            self.regressor = GradientBoostingRegressor(
                n_estimators=100,
                max_depth=5,
                random_state=42
            )
            self.scaler = StandardScaler()
        else:
            # Use simple heuristic-based model
            self.classifier = None
            self.regressor = None
            self.scaler = None
            self.heuristic_weights = self._initialize_heuristic_weights()
    
    def prepare_features(self, program_features: ProgramFeatures,
                        node_features: Dict[str, Any]) -> np.ndarray:
        """Prepare feature vector for ML"""
        # Combine program and node features
        prog_vec = program_features.to_vector()
        
        # Convert node features to vector
        node_vec = np.array([
            node_features.get("child_count", 0),
            node_features.get("depth", 0),
            node_features.get("call_count", 0),
            float(node_features.get("node_type", "") == "APPLICATION"),
            float(node_features.get("node_type", "") == "LAMBDA"),
            float(node_features.get("node_type", "") == "IF"),
            float(node_features.get("is_recursive", False)),
            float(node_features.get("is_pure", True)),
            node_features.get("input_size", 0),
            node_features.get("output_size", 0)
        ])
        
        return np.concatenate([prog_vec, node_vec])
    
    def _initialize_heuristic_weights(self) -> Dict[str, float]:
        """Initialize heuristic weights for non-ML model"""
        # Different weights for different optimization types
        if self.hint_type == OptimizationHintType.INLINE:
            return {
                "call_count": 0.3,
                "node_size": -0.2,
                "depth": -0.1,
                "is_hot": 0.4
            }
        elif self.hint_type == OptimizationHintType.VECTORIZE:
            return {
                "arithmetic_ops": 0.4,
                "has_map_pattern": 0.3,
                "data_parallelism": 0.3
            }
        elif self.hint_type == OptimizationHintType.MEMOIZE:
            return {
                "has_recursion": 0.5,
                "is_pure": 0.3,
                "call_count": 0.2
            }
        else:
            return {}
    
    def train(self, examples: List[TrainingExample], validation_split: float = 0.2):
        """Train the model on examples"""
        if not examples:
            return
        
        if not SKLEARN_AVAILABLE:
            # Simple heuristic learning - adjust weights based on outcomes
            self._train_heuristic(examples)
            return
        
        # Prepare training data
        X = []
        y_classification = []  # Should apply optimization?
        y_regression = []      # Expected speedup
        
        for example in examples:
            # For each node in the program
            for node_id, node_feats in example.node_features.items():
                features = self.prepare_features(example.features, node_feats)
                X.append(features)
                
                # Check if this optimization was applied to this node
                applied = any(
                    nid == node_id and ht == self.hint_type
                    for nid, ht in example.applied_optimizations
                )
                
                y_classification.append(1 if applied else 0)
                y_regression.append(
                    example.performance.speedup if applied else 1.0
                )
        
        X = np.array(X)
        y_classification = np.array(y_classification)
        y_regression = np.array(y_regression)
        
        # Scale features
        X_scaled = self.scaler.fit_transform(X)
        
        # Split data
        X_train, X_val, y_c_train, y_c_val, y_r_train, y_r_val = train_test_split(
            X_scaled, y_classification, y_regression,
            test_size=validation_split, random_state=42
        )
        
        # Train classification model
        self.classifier.fit(X_train, y_c_train)
        
        # Train regression model on positive examples only
        positive_mask = y_c_train == 1
        if np.any(positive_mask):
            self.regressor.fit(
                X_train[positive_mask],
                y_r_train[positive_mask]
            )
        
        # Compute metrics
        y_pred = self.classifier.predict(X_val)
        precision, recall, f1, _ = precision_recall_fscore_support(
            y_c_val, y_pred, average='binary'
        )
        
        # Cross-validation
        cv_scores = cross_val_score(
            self.classifier, X_scaled, y_classification, cv=5
        )
        
        # Feature importance
        feature_importance = {}
        if hasattr(self.classifier, 'feature_importances_'):
            for i, importance in enumerate(self.classifier.feature_importances_):
                if i < len(self.feature_names):
                    feature_importance[self.feature_names[i]] = float(importance)
        
        self.metrics = ModelMetrics(
            accuracy=self.classifier.score(X_val, y_c_val),
            precision=precision,
            recall=recall,
            f1_score=f1,
            cross_validation_score=cv_scores.mean(),
            feature_importance=feature_importance
        )
        
        self.is_trained = True
    
    def _train_heuristic(self, examples: List[TrainingExample]):
        """Train heuristic model by adjusting weights"""
        positive_features = []
        negative_features = []
        
        for example in examples:
            for node_id, node_feats in example.node_features.items():
                applied = any(
                    nid == node_id and ht == self.hint_type
                    for nid, ht in example.applied_optimizations
                )
                
                if applied and example.performance.speedup > 1.1:
                    positive_features.append(node_feats)
                elif not applied:
                    negative_features.append(node_feats)
        
        # Simple weight adjustment based on feature frequency in positive examples
        if positive_features and hasattr(self, 'heuristic_weights'):
            for key in self.heuristic_weights:
                positive_avg = np.mean([f.get(key, 0) for f in positive_features])
                negative_avg = np.mean([f.get(key, 0) for f in negative_features]) if negative_features else 0
                
                # Adjust weight based on difference
                if positive_avg > negative_avg:
                    self.heuristic_weights[key] *= 1.1
                else:
                    self.heuristic_weights[key] *= 0.9
        
        self.is_trained = True
        self.metrics = ModelMetrics(
            accuracy=0.7,  # Heuristic estimate
            precision=0.7,
            recall=0.7,
            f1_score=0.7,
            cross_validation_score=0.7
        )
    
    def predict(self, program_features: ProgramFeatures,
                node_features: Dict[str, Any]) -> Tuple[bool, float, float]:
        """Predict optimization application and expected speedup"""
        if not self.is_trained and not hasattr(self, 'heuristic_weights'):
            return False, 0.5, 1.0
        
        if not SKLEARN_AVAILABLE:
            # Use heuristic prediction
            return self._predict_heuristic(program_features, node_features)
        
        features = self.prepare_features(program_features, node_features)
        features_scaled = self.scaler.transform(features.reshape(1, -1))
        
        # Should apply?
        should_apply = bool(self.classifier.predict(features_scaled)[0])
        confidence = float(self.classifier.predict_proba(features_scaled)[0, 1])
        
        # Expected speedup
        expected_speedup = 1.0
        if should_apply and hasattr(self, 'regressor'):
            try:
                expected_speedup = float(self.regressor.predict(features_scaled)[0])
            except:
                expected_speedup = 1.0
        
        return should_apply, confidence, expected_speedup
    
    def _predict_heuristic(self, program_features: ProgramFeatures,
                          node_features: Dict[str, Any]) -> Tuple[bool, float, float]:
        """Heuristic-based prediction without ML"""
        score = 0.5  # Base score
        
        if self.hint_type == OptimizationHintType.INLINE:
            # Inline small, frequently called functions
            if node_features.get("call_count", 0) > 5:
                score += 0.2
            if node_features.get("child_count", 0) < 10:
                score += 0.2
            if node_features.get("is_hot", False):
                score += 0.3
                
        elif self.hint_type == OptimizationHintType.VECTORIZE:
            # Vectorize arithmetic-heavy code
            if program_features.arithmetic_ops > 10:
                score += 0.3
            if program_features.has_map_pattern:
                score += 0.3
            if node_features.get("node_type") == "APPLICATION":
                score += 0.1
                
        elif self.hint_type == OptimizationHintType.MEMOIZE:
            # Memoize pure recursive functions
            if program_features.has_recursion:
                score += 0.4
            if node_features.get("is_pure", True):
                score += 0.2
            if node_features.get("call_count", 0) > 10:
                score += 0.2
                
        # Apply learned weights if available
        if hasattr(self, 'heuristic_weights'):
            for feature, weight in self.heuristic_weights.items():
                value = node_features.get(feature, 0)
                if isinstance(value, bool):
                    value = float(value)
                score += weight * value * 0.1
        
        # Clamp score
        confidence = max(0.0, min(1.0, score))
        should_apply = confidence > 0.6
        
        # Estimate speedup based on optimization type and confidence
        speedup_estimates = {
            OptimizationHintType.INLINE: 1.1 + confidence * 0.3,
            OptimizationHintType.VECTORIZE: 1.2 + confidence * 0.8,
            OptimizationHintType.MEMOIZE: 1.0 + confidence * 1.5,
            OptimizationHintType.PARALLELIZE: 1.5 + confidence * 1.5,
            OptimizationHintType.UNROLL: 1.1 + confidence * 0.2
        }
        
        expected_speedup = speedup_estimates.get(self.hint_type, 1.0 + confidence * 0.2)
        
        return should_apply, confidence, expected_speedup


class MLModelTrainer:
    """Main trainer for all optimization models"""
    
    def __init__(self, models_dir: str = "models/optimization"):
        self.models_dir = models_dir
        self.models: Dict[OptimizationHintType, OptimizationModel] = {}
        self.training_data: List[TrainingExample] = []
        self.feature_extractor = FeatureExtractor()
        
        # Initialize models
        for hint_type in OptimizationHintType:
            self.models[hint_type] = OptimizationModel(hint_type)
        
        # Create models directory
        os.makedirs(models_dir, exist_ok=True)
    
    def collect_training_example(self, graph: Graph, program_id: str,
                                trace: ExecutionTrace,
                                profile: PerformanceProfile):
        """Collect a training example from execution"""
        # Extract features
        program_features = self.feature_extractor.extract(graph)
        
        # Extract per-node features
        node_features = {}
        for node_id in graph.nodes:
            node_features[node_id] = self._extract_node_features(
                graph, node_id, trace
            )
        
        # Parse applied optimizations
        applied_opts = []
        for opt_name in profile.optimizations_applied:
            # Parse optimization string (e.g., "inline:node_123")
            if ':' in opt_name:
                hint_str, node_id = opt_name.split(':', 1)
                try:
                    hint_type = OptimizationHintType(hint_str)
                    applied_opts.append((node_id, hint_type))
                except ValueError:
                    pass
        
        example = TrainingExample(
            program_id=program_id,
            features=program_features,
            node_features=node_features,
            applied_optimizations=applied_opts,
            performance=profile,
            execution_trace=trace
        )
        
        self.training_data.append(example)
    
    def _extract_node_features(self, graph: Graph, node_id: str,
                              trace: ExecutionTrace) -> Dict[str, Any]:
        """Extract features for a specific node including runtime info"""
        static_features = self.feature_extractor.extract_node_features(
            graph, node_id
        )
        
        # Add runtime features from trace
        runtime_features = {}
        if trace:
            node_trace = trace.get_node_trace(node_id)
            if node_trace:
                runtime_features.update({
                    "execution_count": node_trace.execution_count,
                    "total_time": node_trace.total_time,
                    "average_time": node_trace.average_time,
                    "is_hot": node_trace.is_hot_spot(),
                    "memory_allocated": node_trace.memory_allocated,
                    "cache_misses": node_trace.cache_misses
                })
        
        return {**static_features, **runtime_features}
    
    def train_all_models(self, min_examples: int = 10):
        """Train all optimization models"""
        if len(self.training_data) < min_examples:
            print(f"Not enough training examples: {len(self.training_data)} < {min_examples}")
            return
        
        # Train each model
        for hint_type, model in self.models.items():
            print(f"Training model for {hint_type.value}...")
            model.train(self.training_data)
            
            if model.metrics:
                print(f"  Accuracy: {model.metrics.accuracy:.3f}")
                print(f"  F1 Score: {model.metrics.f1_score:.3f}")
                print(f"  Cross-validation: {model.metrics.cross_validation_score:.3f}")
    
    def predict_optimizations(self, graph: Graph, 
                            trace: Optional[ExecutionTrace] = None) -> List[OptimizationHint]:
        """Predict optimizations for a program"""
        hints = []
        program_features = self.feature_extractor.extract(graph)
        
        for node_id in graph.nodes:
            node_features = self._extract_node_features(graph, node_id, trace)
            
            for hint_type, model in self.models.items():
                should_apply, confidence, speedup = model.predict(
                    program_features, node_features
                )
                
                if should_apply and confidence > 0.6:
                    hint = OptimizationHint(
                        hint_type=hint_type,
                        node_id=node_id,
                        confidence=confidence,
                        parameters={
                            "expected_speedup": speedup,
                            "model_version": self._get_model_version()
                        }
                    )
                    hints.append(hint)
        
        return hints
    
    def save_models(self):
        """Save all trained models"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        for hint_type, model in self.models.items():
            if model.is_trained:
                model_path = os.path.join(
                    self.models_dir,
                    f"{hint_type.value}_model_{timestamp}.pkl"
                )
                
                with open(model_path, 'wb') as f:
                    pickle.dump({
                        'model': model,
                        'metrics': model.metrics,
                        'training_examples': len(self.training_data),
                        'timestamp': timestamp
                    }, f)
                
                # Also save metrics as JSON
                metrics_path = os.path.join(
                    self.models_dir,
                    f"{hint_type.value}_metrics_{timestamp}.json"
                )
                
                if model.metrics:
                    with open(metrics_path, 'w') as f:
                        json.dump({
                            'accuracy': model.metrics.accuracy,
                            'precision': model.metrics.precision,
                            'recall': model.metrics.recall,
                            'f1_score': model.metrics.f1_score,
                            'cross_validation': model.metrics.cross_validation_score,
                            'feature_importance': model.metrics.feature_importance
                        }, f, indent=2)
    
    def load_models(self, version: Optional[str] = None):
        """Load previously trained models"""
        # Find latest models if version not specified
        if not version:
            model_files = [f for f in os.listdir(self.models_dir) 
                          if f.endswith('.pkl')]
            if not model_files:
                return
            
            # Group by hint type and find latest
            latest_models = {}
            for filename in model_files:
                parts = filename.split('_')
                if len(parts) >= 3:
                    hint_type = parts[0]
                    timestamp = '_'.join(parts[-2:]).replace('.pkl', '')
                    
                    if hint_type not in latest_models or timestamp > latest_models[hint_type][1]:
                        latest_models[hint_type] = (filename, timestamp)
            
            # Load latest models
            for hint_type_str, (filename, _) in latest_models.items():
                try:
                    hint_type = OptimizationHintType(hint_type_str)
                    model_path = os.path.join(self.models_dir, filename)
                    
                    with open(model_path, 'rb') as f:
                        data = pickle.load(f)
                        self.models[hint_type] = data['model']
                        print(f"Loaded model for {hint_type.value} from {filename}")
                except Exception as e:
                    print(f"Failed to load model {filename}: {e}")
    
    def generate_report(self) -> str:
        """Generate a report on model performance"""
        report = ["ML Optimization Models Report", "=" * 40, ""]
        
        report.append(f"Training examples: {len(self.training_data)}")
        report.append("")
        
        for hint_type, model in self.models.items():
            report.append(f"{hint_type.value} Model:")
            
            if model.is_trained and model.metrics:
                report.append(f"  Trained: Yes")
                report.append(f"  Accuracy: {model.metrics.accuracy:.3f}")
                report.append(f"  Precision: {model.metrics.precision:.3f}")
                report.append(f"  Recall: {model.metrics.recall:.3f}")
                report.append(f"  F1 Score: {model.metrics.f1_score:.3f}")
                report.append(f"  Cross-validation: {model.metrics.cross_validation_score:.3f}")
                
                # Top features
                if model.metrics.feature_importance:
                    report.append("  Top features:")
                    sorted_features = sorted(
                        model.metrics.feature_importance.items(),
                        key=lambda x: x[1],
                        reverse=True
                    )[:5]
                    for feat, importance in sorted_features:
                        report.append(f"    - {feat}: {importance:.3f}")
            else:
                report.append(f"  Trained: No")
            
            report.append("")
        
        return "\n".join(report)
    
    def _get_model_version(self) -> str:
        """Get current model version"""
        return datetime.now().strftime("%Y%m%d")


class OnlineLearner:
    """Online learning system that updates models during execution"""
    
    def __init__(self, trainer: MLModelTrainer):
        self.trainer = trainer
        self.pending_examples: List[TrainingExample] = []
        self.update_frequency = 100  # Update after N examples
        self.min_improvement = 0.05  # Minimum speedup to consider
    
    def observe_execution(self, graph: Graph, program_id: str,
                         baseline_time: float, optimized_time: float,
                         optimizations: List[str], trace: ExecutionTrace):
        """Observe an execution and learn from it"""
        speedup = baseline_time / max(optimized_time, 0.001)
        
        # Only learn from significant improvements
        if speedup > 1.0 + self.min_improvement:
            profile = PerformanceProfile(
                execution_time=optimized_time,
                memory_usage=trace.peak_memory if trace else 0,
                cache_misses=trace.total_cache_misses if trace else 0,
                branch_mispredicts=0,
                optimizations_applied=optimizations,
                speedup=speedup
            )
            
            self.trainer.collect_training_example(
                graph, program_id, trace, profile
            )
            
            # Check if we should update models
            if len(self.trainer.training_data) % self.update_frequency == 0:
                self.update_models()
    
    def update_models(self):
        """Incrementally update models"""
        print(f"Updating models with {len(self.trainer.training_data)} examples...")
        self.trainer.train_all_models()
        self.trainer.save_models()


def create_ml_optimization_pipeline(models_dir: str = "models/optimization") -> MLModelTrainer:
    """Create and initialize the ML optimization pipeline"""
    trainer = MLModelTrainer(models_dir)
    
    # Try to load existing models
    trainer.load_models()
    
    return trainer