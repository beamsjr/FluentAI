"""
ClaudeLang ML-Driven Optimization Integration

This module integrates the ML optimization learning system with the
ClaudeLang compiler and runtime.
"""

import os
import time
import json
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from collections import defaultdict

from ..core.ast import Graph
from ..optimizer.graph_optimizer import GraphOptimizer
from ..vm.compiler import BytecodeCompiler
from ..vm import VM
from ..performance.execution_trace import ExecutionTrace, ExecutionTracer
from ..semantic.ml_optimization_hints import (
    OptimizationHint, OptimizationHintType, OptimizationLearner
)
from .model_trainer import MLModelTrainer, OnlineLearner


@dataclass
class OptimizationExperiment:
    """Results from an optimization experiment"""
    program_id: str
    baseline_time: float
    optimized_time: float
    speedup: float
    applied_hints: List[OptimizationHint]
    successful_optimizations: List[str]
    trace: Optional[ExecutionTrace] = None


class MLOptimizationEngine:
    """Main engine for ML-driven optimization"""
    
    def __init__(self, models_dir: str = "models/optimization"):
        self.models_dir = models_dir
        self.ml_trainer = MLModelTrainer(models_dir)
        self.hint_learner = OptimizationLearner()
        self.online_learner = OnlineLearner(self.ml_trainer)
        self.optimizer = GraphOptimizer()
        self.experiments: List[OptimizationExperiment] = []
        
        # Statistics
        self.stats = {
            "programs_optimized": 0,
            "total_speedup": 0.0,
            "successful_hints": defaultdict(int),
            "failed_hints": defaultdict(int)
        }
    
    def optimize_with_ml(self, graph: Graph, program_id: str,
                        input_data: Optional[Any] = None) -> Tuple[Graph, OptimizationExperiment]:
        """Optimize a program using ML predictions"""
        # Get baseline performance
        baseline_time = self._measure_baseline(graph, input_data)
        
        # Get ML predictions
        ml_hints = self.ml_trainer.predict_optimizations(graph)
        
        # Also get heuristic hints
        heuristic_hints = self.hint_learner.extract_hints(graph, program_id)
        
        # Combine and prioritize hints
        all_hints = self._combine_hints(ml_hints, heuristic_hints)
        
        # Apply optimizations
        optimized_graph, applied_hints = self._apply_optimizations(graph, all_hints)
        
        # Measure optimized performance with trace
        optimized_time, trace = self._measure_optimized(optimized_graph, input_data)
        
        # Calculate speedup
        speedup = baseline_time / max(optimized_time, 0.001)
        
        # Record experiment
        experiment = OptimizationExperiment(
            program_id=program_id,
            baseline_time=baseline_time,
            optimized_time=optimized_time,
            speedup=speedup,
            applied_hints=applied_hints,
            successful_optimizations=[
                f"{h.hint_type.value}:{h.node_id}" for h in applied_hints
            ],
            trace=trace
        )
        
        self.experiments.append(experiment)
        
        # Learn from results
        self._learn_from_experiment(graph, experiment)
        
        # Update statistics
        self._update_statistics(experiment)
        
        return optimized_graph, experiment
    
    def _measure_baseline(self, graph: Graph, input_data: Optional[Any]) -> float:
        """Measure baseline performance"""
        compiler = BytecodeCompiler()
        bytecode = compiler.compile(graph)
        
        vm = VM()
        
        # Warm up
        for _ in range(3):
            vm.reset()
            vm.load_bytecode(bytecode)
            if input_data is not None:
                vm.push(input_data)
            vm.run()
        
        # Actual measurement
        start_time = time.perf_counter()
        vm.reset()
        vm.load_bytecode(bytecode)
        if input_data is not None:
            vm.push(input_data)
        vm.run()
        end_time = time.perf_counter()
        
        return end_time - start_time
    
    def _measure_optimized(self, graph: Graph, 
                          input_data: Optional[Any]) -> Tuple[float, ExecutionTrace]:
        """Measure optimized performance with tracing"""
        compiler = BytecodeCompiler()
        bytecode = compiler.compile(graph)
        
        # Create tracer
        tracer = ExecutionTracer()
        
        # Create VM with tracer
        vm = VM()
        vm.set_tracer(tracer)
        
        # Warm up
        for _ in range(3):
            vm.reset()
            vm.load_bytecode(bytecode)
            if input_data is not None:
                vm.push(input_data)
            vm.run()
        
        # Actual measurement with tracing
        tracer.start_trace()
        start_time = time.perf_counter()
        
        vm.reset()
        vm.load_bytecode(bytecode)
        if input_data is not None:
            vm.push(input_data)
        vm.run()
        
        end_time = time.perf_counter()
        trace = tracer.end_trace()
        
        return end_time - start_time, trace
    
    def _combine_hints(self, ml_hints: List[OptimizationHint],
                      heuristic_hints: List[OptimizationHint]) -> List[OptimizationHint]:
        """Combine ML and heuristic hints, prioritizing by confidence"""
        # Index hints by (node_id, hint_type)
        hint_map = {}
        
        # Add heuristic hints first
        for hint in heuristic_hints:
            key = (hint.node_id, hint.hint_type)
            hint_map[key] = hint
        
        # Override or boost with ML hints
        for hint in ml_hints:
            key = (hint.node_id, hint.hint_type)
            if key in hint_map:
                # Boost confidence if both agree
                existing = hint_map[key]
                hint.confidence = min(1.0, (hint.confidence + existing.confidence) / 1.5)
            hint_map[key] = hint
        
        # Sort by confidence
        all_hints = list(hint_map.values())
        all_hints.sort(key=lambda h: h.confidence, reverse=True)
        
        return all_hints
    
    def _apply_optimizations(self, graph: Graph,
                           hints: List[OptimizationHint]) -> Tuple[Graph, List[OptimizationHint]]:
        """Apply optimization hints to graph"""
        optimized_graph = graph
        applied_hints = []
        
        for hint in hints:
            if hint.apply_threshold():
                try:
                    # Apply optimization based on hint type
                    if hint.hint_type == OptimizationHintType.INLINE:
                        result = self.optimizer.inline_function(
                            optimized_graph, hint.node_id
                        )
                    elif hint.hint_type == OptimizationHintType.UNROLL:
                        result = self.optimizer.unroll_loop(
                            optimized_graph, hint.node_id,
                            factor=hint.parameters.get("factor", 4)
                        )
                    elif hint.hint_type == OptimizationHintType.VECTORIZE:
                        result = self.optimizer.vectorize_operation(
                            optimized_graph, hint.node_id,
                            width=hint.parameters.get("vector_width", 4)
                        )
                    elif hint.hint_type == OptimizationHintType.MEMOIZE:
                        result = self.optimizer.add_memoization(
                            optimized_graph, hint.node_id,
                            cache_size=hint.parameters.get("cache_size", 256)
                        )
                    else:
                        # Other optimizations not yet implemented
                        result = optimized_graph
                    
                    if result != optimized_graph:
                        optimized_graph = result
                        applied_hints.append(hint)
                        
                except Exception as e:
                    # Log optimization failure
                    print(f"Failed to apply {hint.hint_type.value} to {hint.node_id}: {e}")
        
        return optimized_graph, applied_hints
    
    def _learn_from_experiment(self, graph: Graph, experiment: OptimizationExperiment):
        """Learn from optimization results"""
        # Update online learner
        self.online_learner.observe_execution(
            graph,
            experiment.program_id,
            experiment.baseline_time,
            experiment.optimized_time,
            experiment.successful_optimizations,
            experiment.trace
        )
        
        # Update hint learner
        from ..semantic.ml_optimization_hints import PerformanceProfile
        profile = PerformanceProfile(
            execution_time=experiment.optimized_time,
            memory_usage=experiment.trace.peak_memory if experiment.trace else 0,
            cache_misses=experiment.trace.total_cache_misses if experiment.trace else 0,
            branch_mispredicts=0,
            optimizations_applied=experiment.successful_optimizations,
            speedup=experiment.speedup
        )
        
        self.hint_learner.learn_from_execution(experiment.program_id, profile)
    
    def _update_statistics(self, experiment: OptimizationExperiment):
        """Update optimization statistics"""
        self.stats["programs_optimized"] += 1
        self.stats["total_speedup"] += experiment.speedup
        
        for hint in experiment.applied_hints:
            if experiment.speedup > 1.05:  # 5% improvement threshold
                self.stats["successful_hints"][hint.hint_type.value] += 1
            else:
                self.stats["failed_hints"][hint.hint_type.value] += 1
    
    def train_from_benchmark_suite(self, benchmark_dir: str):
        """Train models using a benchmark suite"""
        import glob
        
        # Find all benchmark files
        benchmark_files = glob.glob(os.path.join(benchmark_dir, "*.cl"))
        
        print(f"Training on {len(benchmark_files)} benchmarks...")
        
        for i, bench_file in enumerate(benchmark_files):
            print(f"Processing {i+1}/{len(benchmark_files)}: {bench_file}")
            
            # Parse program
            from ..parser.sexpr_parser import parse
            with open(bench_file, 'r') as f:
                source = f.read()
            
            try:
                graph = parse(source)
                program_id = os.path.basename(bench_file)
                
                # Run optimization experiment
                self.optimize_with_ml(graph, program_id)
                
            except Exception as e:
                print(f"  Failed: {e}")
        
        # Save trained models
        self.ml_trainer.save_models()
        self.hint_learner.save_learned_patterns("models/hint_patterns.json")
    
    def generate_optimization_report(self) -> str:
        """Generate a comprehensive optimization report"""
        report = ["ML Optimization Report", "=" * 50, ""]
        
        # Overall statistics
        avg_speedup = (self.stats["total_speedup"] / 
                      max(self.stats["programs_optimized"], 1))
        
        report.append(f"Programs optimized: {self.stats['programs_optimized']}")
        report.append(f"Average speedup: {avg_speedup:.2f}x")
        report.append("")
        
        # Hint effectiveness
        report.append("Optimization effectiveness:")
        for hint_type in OptimizationHintType:
            successful = self.stats["successful_hints"][hint_type.value]
            failed = self.stats["failed_hints"][hint_type.value]
            total = successful + failed
            
            if total > 0:
                success_rate = successful / total * 100
                report.append(f"  {hint_type.value}: {success_rate:.1f}% success "
                            f"({successful}/{total})")
        
        report.append("")
        
        # Top experiments
        report.append("Top optimization results:")
        sorted_experiments = sorted(
            self.experiments,
            key=lambda e: e.speedup,
            reverse=True
        )[:10]
        
        for exp in sorted_experiments:
            report.append(f"  {exp.program_id}: {exp.speedup:.2f}x speedup")
            for opt in exp.successful_optimizations[:3]:
                report.append(f"    - {opt}")
        
        report.append("")
        
        # ML model report
        report.append(self.ml_trainer.generate_report())
        
        return "\n".join(report)
    
    def export_training_data(self, output_file: str):
        """Export training data for analysis"""
        data = {
            "experiments": [
                {
                    "program_id": exp.program_id,
                    "baseline_time": exp.baseline_time,
                    "optimized_time": exp.optimized_time,
                    "speedup": exp.speedup,
                    "optimizations": exp.successful_optimizations
                }
                for exp in self.experiments
            ],
            "statistics": dict(self.stats),
            "model_metrics": {
                hint_type.value: (
                    self.ml_trainer.models[hint_type].metrics.__dict__
                    if self.ml_trainer.models[hint_type].metrics else {}
                )
                for hint_type in OptimizationHintType
            }
        }
        
        with open(output_file, 'w') as f:
            json.dump(data, f, indent=2)


def create_ml_optimization_engine() -> MLOptimizationEngine:
    """Create and initialize ML optimization engine"""
    engine = MLOptimizationEngine()
    
    # Load any existing models
    engine.ml_trainer.load_models()
    
    # Load learned patterns
    patterns_file = "models/hint_patterns.json"
    if os.path.exists(patterns_file):
        engine.hint_learner.load_learned_patterns(patterns_file)
    
    return engine