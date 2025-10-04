#!/usr/bin/env python3.13
"""
RHEA Framework - Python 3.13 Implementation
Racket Hypothesis-Experiment-Analysis
A scientific method framework for programming language experiments
"""

from dataclasses import dataclass, field
from typing import Callable, Any, List, Dict, Optional
from datetime import datetime
from statistics import mean, variance, stdev
from math import sqrt
import random
from enum import Enum


# ============================================================================
# Core Data Structures
# ============================================================================

class Status(Enum):
    """Experiment status enum."""
    PLANNED = "planned"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"
    CLOSED = "closed"


@dataclass
class Hypothesis:
    """Represents a scientific hypothesis."""
    id: str
    description: str
    assumptions: List[str]
    predictions: List[str]
    metadata: Dict[str, Any] = field(default_factory=dict)

    def __repr__(self) -> str:
        return f"<Hypothesis:{self.id}>"


@dataclass
class Experiment:
    """Represents an experimental procedure."""
    id: str
    hypothesis_id: str
    setup: Callable[[], Any]
    procedure: Callable[[Any], Any]
    data_collector: Callable[[Any], Any]
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Observation:
    """Represents a single experimental observation."""
    id: str
    experiment_id: str
    timestamp: datetime
    data: Any
    context: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Analysis:
    """Represents statistical analysis of observations."""
    id: str
    observations: List[Observation]
    statistical_tests: List[str]
    results: Dict[str, Any]
    interpretation: List[tuple[str, str]]


# ============================================================================
# Experiment Execution Engine
# ============================================================================

def execute_experiment(exp: Experiment, replicates: int) -> List[Observation]:
    """
    Execute an experiment for a given number of replicates.

    Args:
        exp: The experiment to execute
        replicates: Number of times to replicate the experiment

    Returns:
        List of observations from all replicates
    """
    observations = []

    for i in range(replicates):
        env = exp.setup()
        result = exp.procedure(env)
        obs = Observation(
            id=f"obs_{i}",
            experiment_id=exp.id,
            timestamp=datetime.now(),
            data=exp.data_collector(result),
            context={
                'replicate': i,
                'environment': env
            }
        )
        observations.append(obs)

    return observations


# ============================================================================
# Statistical Analysis
# ============================================================================

def calculate_mean(data: List[float]) -> float:
    """Calculate arithmetic mean."""
    return mean(data)


def calculate_variance(data: List[float]) -> float:
    """Calculate sample variance."""
    return variance(data)


def calculate_std_dev(data: List[float]) -> float:
    """Calculate standard deviation."""
    return stdev(data)


def t_test(data: List[float], alpha: float = 0.05) -> Dict[str, Any]:
    """
    Simplified one-sample t-test.

    Args:
        data: Sample data
        alpha: Significance level

    Returns:
        Dictionary with t-statistic and significance
    """
    μ = calculate_mean(data)
    s = calculate_std_dev(data)
    n = len(data)
    t_stat = μ / (s / sqrt(n))

    return {
        't_statistic': t_stat,
        'significant': abs(t_stat) > 2.0  # Simplified critical value
    }


def anova_test(data: List[float], alpha: float = 0.05) -> Dict[str, Any]:
    """Placeholder for ANOVA test."""
    return {
        'f_statistic': 0.0,
        'p_value': 1.0
    }


def analyze_results(
    observations: List[Observation],
    tests: List[str] = None,
    alpha: float = 0.05
) -> Analysis:
    """
    Analyze experimental observations with statistical tests.

    Args:
        observations: List of observations to analyze
        tests: List of statistical tests to perform
        alpha: Significance level

    Returns:
        Analysis object with results and interpretation
    """
    if tests is None:
        tests = ['mean', 'variance', 't_test']

    data = [obs.data for obs in observations]

    statistical_results = {}
    for test in tests:
        match test:
            case 'mean':
                statistical_results[test] = calculate_mean(data)
            case 'variance':
                statistical_results[test] = calculate_variance(data)
            case 't_test':
                statistical_results[test] = t_test(data, alpha)
            case 'anova':
                statistical_results[test] = anova_test(data, alpha)
            case _:
                raise ValueError(f"Unknown test: {test}")

    interpretation = interpret_results(statistical_results, alpha)

    return Analysis(
        id=f"analysis_{datetime.now().timestamp()}",
        observations=observations,
        statistical_tests=tests,
        results=statistical_results,
        interpretation=interpretation
    )


def interpret_results(results: Dict[str, Any], alpha: float) -> List[tuple[str, str]]:
    """
    Interpret statistical test results.

    Args:
        results: Dictionary of test results
        alpha: Significance level

    Returns:
        List of (test_name, interpretation) tuples
    """
    interpretation = []

    for test, result in results.items():
        if isinstance(result, dict) and 'significant' in result:
            status = 'significant' if result['significant'] else 'not_significant'
        else:
            status = 'inconclusive'
        interpretation.append((test, status))

    return interpretation


# ============================================================================
# Hypothesis Refinement
# ============================================================================

def refine_hypothesis(h: Hypothesis, analysis_result: Analysis) -> Hypothesis:
    """
    Refine hypothesis based on analysis results.

    Args:
        h: Original hypothesis
        analysis_result: Analysis results

    Returns:
        Refined or alternative hypothesis
    """
    significant_count = sum(
        1 for _, status in analysis_result.interpretation
        if status == 'significant'
    )

    if significant_count > len(analysis_result.interpretation) / 2:
        # Hypothesis supported
        return Hypothesis(
            id=f"{h.id}_refined",
            description=f"Refined: {h.description}",
            assumptions=h.assumptions,
            predictions=h.predictions,
            metadata={**h.metadata, 'status': Status.COMPLETED.value}
        )
    else:
        # Hypothesis rejected
        return Hypothesis(
            id=f"{h.id}_alternative",
            description=f"Alternative: {h.description}",
            assumptions=h.assumptions,
            predictions=[],  # Need new predictions
            metadata={**h.metadata, 'status': Status.BLOCKED.value}
        )


# ============================================================================
# Org-Mode Export
# ============================================================================

def export_to_org(
    hypothesis: Hypothesis,
    experiments: List[Experiment],
    analyses: List[Analysis],
    filename: str = "research.org"
) -> None:
    """
    Export RHEA framework results to Org-mode format.

    Args:
        hypothesis: The hypothesis being tested
        experiments: List of experiments conducted
        analyses: List of analysis results
        filename: Output filename
    """
    with open(filename, 'w') as f:
        f.write(f"#+TITLE: RHEA Research: {hypothesis.description}\n")
        f.write("#+AUTHOR: Generated by RHEA Framework (Python)\n")
        f.write(f"#+DATE: {datetime.now().strftime('%Y-%m-%d')}\n\n")

        f.write("* Hypothesis\n")
        f.write("** Description\n")
        f.write(f"{hypothesis.description}\n\n")

        f.write("** Assumptions\n")
        for assumption in hypothesis.assumptions:
            f.write(f"- {assumption}\n")
        f.write("\n")

        f.write("** Predictions\n")
        for prediction in hypothesis.predictions:
            f.write(f"- {prediction}\n")
        f.write("\n")

        f.write("* Experiments\n")
        for exp in experiments:
            f.write(f"** Experiment: {exp.id}\n")
            f.write("#+BEGIN_SRC python\n")
            f.write(f"# Procedure: {exp.procedure.__name__}\n")
            f.write("#+END_SRC\n\n")

        f.write("* Analysis\n")
        for analysis in analyses:
            f.write("** Analysis Results\n")
            f.write("#+BEGIN_SRC python :results output\n")
            for test, result in analysis.results.items():
                f.write(f"{test}: {result}\n")
            f.write("#+END_SRC\n\n")

            f.write("** Interpretation\n")
            for test, status in analysis.interpretation:
                f.write(f"- {test}: {status}\n")
            f.write("\n")

        f.write("* Workflow Diagram\n")
        f.write("#+BEGIN_SRC mermaid\n")
        export_mermaid_workflow(f, hypothesis, experiments, analyses)
        f.write("#+END_SRC\n")


def export_mermaid_workflow(
    f,
    hypothesis: Hypothesis,
    experiments: List[Experiment],
    analyses: List[Analysis]
) -> None:
    """Generate Mermaid workflow diagram."""
    f.write("graph TD\n")
    f.write(f'  H["{hypothesis.id}"] --> E\n')
    for exp in experiments:
        f.write(f'  E[Experiment: {exp.id}] --> O{exp.id}[Observations]\n')
    for analysis in analyses:
        interp_label = analysis.interpretation[0][1] if analysis.interpretation else "unknown"
        f.write(f'  O --> A{analysis.id}["Analysis: {interp_label}"]\n')
    f.write('  A --> R{"Refine Hypothesis"}\n')
    f.write('  R -->|Supported| H2[New Hypothesis]\n')
    f.write('  R -->|Rejected| H3[Alternative Hypothesis]\n')


# ============================================================================
# Example Usage
# ============================================================================

def run_example():
    """Run example RHEA framework experiment."""

    # Define hypothesis
    lens_composition = Hypothesis(
        id="lens_composition",
        description="Lens composition preserves lens laws",
        assumptions=[
            "Individual lenses satisfy GetPut, PutGet, PutPut",
            "Composition is associative"
        ],
        predictions=[
            "Composed lenses satisfy all three lens laws",
            "Property tests verify laws for 1000+ cases"
        ],
        metadata={
            'domain': 'functional_programming',
            'status': Status.PLANNED.value,
            'related_experiments': [45, 47, 48]
        }
    )

    # Create experiment
    def setup() -> Dict[str, Any]:
        return {'test_data': [1, 2, 3, 4, 5]}

    def procedure(env: Dict[str, Any]) -> int:
        # Simulate lens law verification
        return 1 if random.random() > 0.05 else 0  # 95% success rate

    def collector(result: int) -> int:
        return result

    lens_law_test = Experiment(
        id="lens_law_test",
        hypothesis_id=lens_composition.id,
        setup=setup,
        procedure=procedure,
        data_collector=collector,
        metadata={
            'replicates': 100,
            'timestamp': datetime.now()
        }
    )

    # Execute experiment
    observations = execute_experiment(lens_law_test, 100)

    # Analyze results
    results = analyze_results(observations, tests=['mean', 'variance', 't_test'])

    # Refine hypothesis
    refined = refine_hypothesis(lens_composition, results)

    # Display results
    print("=== RHEA Framework Example Output (Python) ===")
    print(f"Hypothesis: {lens_composition.description}")
    print(f"Mean success rate: {results.results['mean']}")
    print(f"Status: {'SUPPORTED' if any(s == 'significant' for _, s in results.interpretation) else 'REJECTED'}")

    # Export to org-mode
    export_to_org(lens_composition, [lens_law_test], [results], "research-python.org")


if __name__ == "__main__":
    run_example()
