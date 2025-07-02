"""
ClaudeLang Machine Learning Module
"""

from .model_trainer import (
    MLModelTrainer,
    OnlineLearner,
    TrainingExample,
    ModelMetrics,
    create_ml_optimization_pipeline
)

__all__ = [
    'MLModelTrainer',
    'OnlineLearner', 
    'TrainingExample',
    'ModelMetrics',
    'create_ml_optimization_pipeline'
]