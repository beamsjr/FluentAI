"""
ClaudeLang Semantic Analysis and AI-First Features
"""

from .behavior_versioning import (
    BehaviorAnalyzer,
    BehaviorSignature,
    SemanticVersion,
    BehaviorChange,
    generate_behavior_documentation
)

from .proof_generation import (
    ProofGenerator,
    ProofStep,
    Theorem,
    ProofObligation,
    ProofTactic
)

from .ml_optimization_hints import (
    OptimizationLearner,
    OptimizationHint,
    OptimizationHintType,
    ProgramFeatures,
    PerformanceProfile
)

from .formal_specs import (
    SpecificationEngine,
    FormalSpec,
    SpecType,
    ContractedFunction,
    requires,
    ensures,
    invariant
)

from .trace_documentation import (
    TraceAnalyzer,
    ExecutionTrace,
    TraceEvent,
    TraceEventType,
    TracingInterpreter,
    FunctionProfile,
    BehaviorPattern
)

__all__ = [
    # Behavior versioning
    'BehaviorAnalyzer',
    'BehaviorSignature',
    'SemanticVersion',
    'BehaviorChange',
    'generate_behavior_documentation',
    
    # Proof generation
    'ProofGenerator',
    'ProofStep',
    'Theorem',
    'ProofObligation',
    'ProofTactic',
    
    # ML optimization hints
    'OptimizationLearner',
    'OptimizationHint',
    'OptimizationHintType',
    'ProgramFeatures',
    'PerformanceProfile',
    
    # Formal specifications
    'SpecificationEngine',
    'FormalSpec',
    'SpecType',
    'ContractedFunction',
    'requires',
    'ensures',
    'invariant',
    
    # Trace documentation
    'TraceAnalyzer',
    'ExecutionTrace',
    'TraceEvent',
    'TraceEventType',
    'TracingInterpreter',
    'FunctionProfile',
    'BehaviorPattern'
]