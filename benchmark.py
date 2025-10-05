#!/usr/bin/env python3
"""
Benchmarking script for STLC interpreter.
Tests performance with trampolined vs non-trampolined evaluation.
"""

import subprocess
import time
import os
import sys
import json
from pathlib import Path
from typing import Optional, Dict, List, Tuple
import tempfile

class BenchmarkResult:
    def __init__(self, n: int, mode: str, time_sec: Optional[float],
                 stack_overflow: bool, error: Optional[str] = None):
        self.n = n
        self.mode = mode  # "trampolined" or "direct"
        self.time_sec = time_sec
        self.stack_overflow = stack_overflow
        self.error = error

    def __repr__(self):
        if self.stack_overflow:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, stack_overflow=True)"
        elif self.error:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, error={self.error})"
        else:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, time={self.time_sec:.4f}s)"

class STLCBenchmark:
    def __init__(self, binary_path: str = "./target/release/interpreter-benchmark"):
        self.binary_path = binary_path
        self.temp_dir = tempfile.mkdtemp(prefix="stlc_bench_")

    def cleanup(self):
        """Clean up temporary files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def generate_program(self, function_def: str, test_value: int) -> str:
        """
        Generate an STLC program.

        Args:
            function_def: The function definition (without the application)
            test_value: The value to apply to the function

        Returns:
            Complete STLC program as string
        """
        return f"{function_def} {test_value}"

    def run_interpreter(self, program: str, trampolined: bool, timeout: int = 60) -> BenchmarkResult:
        """
        Run the interpreter on a program and measure execution time.

        Args:
            program: STLC program text
            trampolined: Whether to use trampolined evaluation
            timeout: Maximum execution time in seconds

        Returns:
            BenchmarkResult object
        """
        # Write program to temporary file
        temp_file = os.path.join(self.temp_dir, f"test_{time.time()}.stlc")
        with open(temp_file, 'w') as f:
            f.write(program)

        # Build command
        cmd = [self.binary_path, temp_file]
        if trampolined:
            cmd.append("--trampolined")

        mode = "trampolined" if trampolined else "direct"

        try:
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout
            )
            elapsed = time.time() - start_time

            # Check for errors
            if result.returncode != 0:
                stderr = result.stderr.lower()
                # Check if it's a stack overflow
                if "stack overflow" in stderr or "segmentation fault" in stderr:
                    return BenchmarkResult(None, mode, None, True)
                else:
                    return BenchmarkResult(None, mode, None, False, result.stderr)

            return BenchmarkResult(None, mode, elapsed, False)

        except subprocess.TimeoutExpired:
            return BenchmarkResult(None, mode, None, False, f"Timeout ({timeout}s)")
        except Exception as e:
            return BenchmarkResult(None, mode, None, False, str(e))
        finally:
            # Clean up temp file
            if os.path.exists(temp_file):
                os.remove(temp_file)

    def benchmark_function(self, function_def: str, test_values: List[int],
                          description: str = "") -> List[Dict]:
        """
        Benchmark a function with different input values.

        Args:
            function_def: STLC function definition (without application)
            test_values: List of values to test with
            description: Description of what's being benchmarked

        Returns:
            List of result dictionaries
        """
        print(f"\n{'='*70}")
        print(f"Benchmarking: {description}")
        print(f"{'='*70}")

        results = []

        for n in test_values:
            print(f"\nTesting with n = {n}...")
            program = self.generate_program(function_def, n)

            # Test direct evaluation
            print(f"  Direct evaluation: ", end="", flush=True)
            result_direct = self.run_interpreter(program, trampolined=False)
            result_direct.n = n

            if result_direct.stack_overflow:
                print("STACK OVERFLOW")
            elif result_direct.error:
                print(f"ERROR: {result_direct.error}")
            else:
                print(f"{result_direct.time_sec:.4f}s")

            # Test trampolined evaluation
            print(f"  Trampolined evaluation: ", end="", flush=True)
            result_tramp = self.run_interpreter(program, trampolined=True)
            result_tramp.n = n

            if result_tramp.stack_overflow:
                print("STACK OVERFLOW")
            elif result_tramp.error:
                print(f"ERROR: {result_tramp.error}")
            else:
                print(f"{result_tramp.time_sec:.4f}s")

            # Record results
            results.append({
                'n': n,
                'description': description,
                'direct': {
                    'time': result_direct.time_sec,
                    'stack_overflow': result_direct.stack_overflow,
                    'error': result_direct.error
                },
                'trampolined': {
                    'time': result_tramp.time_sec,
                    'stack_overflow': result_tramp.stack_overflow,
                    'error': result_tramp.error
                }
            })

            # If both modes failed with stack overflow at this size, no point testing larger
            if result_direct.stack_overflow and result_tramp.stack_overflow:
                print(f"\n  Both modes overflow at n={n}, stopping further tests for this function.")
                break

        return results


def save_results(results: List[Dict], output_file: str = "benchmark_results.json"):
    """Save benchmark results to JSON file."""
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"\n\nResults saved to {output_file}")


def print_summary(all_results: List[Dict]):
    """Print a summary table of results."""
    print(f"\n\n{'='*70}")
    print("BENCHMARK SUMMARY")
    print(f"{'='*70}")

    for result in all_results:
        desc = result['description']
        n = result['n']
        direct = result['direct']
        tramp = result['trampolined']

        print(f"\n{desc} (n={n}):")

        # Direct
        if direct['stack_overflow']:
            print(f"  Direct:      STACK OVERFLOW")
        elif direct['error']:
            print(f"  Direct:      ERROR")
        else:
            print(f"  Direct:      {direct['time']:.4f}s")

        # Trampolined
        if tramp['stack_overflow']:
            print(f"  Trampolined: STACK OVERFLOW")
        elif tramp['error']:
            print(f"  Trampolined: ERROR")
        else:
            print(f"  Trampolined: {tramp['time']:.4f}s")

        # Speedup comparison
        if direct['time'] and tramp['time']:
            speedup = direct['time'] / tramp['time']
            if speedup < 1:
                print(f"  -> Direct is {1/speedup:.2f}x faster")
            else:
                print(f"  -> Trampolined is {speedup:.2f}x faster")


def main():
    # Check if binary exists
    binary_path = "./target/release/interpreter-benchmark"
    if not os.path.exists(binary_path):
        print(f"Error: Binary not found at {binary_path}")
        print("Building release binary...")
        subprocess.run(["cargo", "build", "--release"], check=True)

    benchmark = STLCBenchmark(binary_path)

    try:
        all_results = []

        # Test values - start small and go larger
        test_values = [2**i for i in range(4, 20)]

        # Benchmark 1: Simple recursive sum
        # sum n = if n < 1 then 0 else n + sum (n-1)
        sum_function = "(fix sum: Int -> Int. \\n: Int. if n < 1 then 0 else n + sum (n - 1))"
        results = benchmark.benchmark_function(
            sum_function,
            test_values,
            "Simple recursive sum"
        )
        all_results.extend(results)

        # Benchmark 2: Double recursive function
        # bench n = sum n + bench (n-1)
        # This is more complex - we need nested fix points
        bench_function = """(fix bench: Int -> Int. \\n: Int.
  if n < 1 then 0
  else ((fix sum: Int -> Int. \\m: Int. if m < 1 then 0 else m + sum (m - 1)) n) + bench (n - 1))"""
        bench_function = bench_function.replace('\n', ' ').replace('  ', ' ')
        results = benchmark.benchmark_function(
            bench_function,
            test_values,
            "Nested recursive (bench n = sum n + bench (n-1))"
        )
        all_results.extend(results)

        # Benchmark 3: Simple tail-recursive countdown
        countdown_function = "(fix countdown: Int -> Int. \\n: Int. if n < 1 then 0 else countdown (n - 1))"
        results = benchmark.benchmark_function(
            countdown_function,
            test_values,
            "Tail-recursive countdown"
        )
        all_results.extend(results)

        # Benchmark 4: Accumulator pattern (more stack frames)
        acc_function = "(fix acc: Int -> Int. \\n: Int. if n < 1 then 0 else 1 + acc (n - 1))"
        results = benchmark.benchmark_function(
            acc_function,
            test_values,
            "Accumulator pattern (acc n = 1 + acc (n-1))"
        )
        all_results.extend(results)

        # Benchmark 5: Fibonacci (exponential complexity)
        fib_function = "(fix fib: Int -> Int. \\n: Int. if n < 2 then n else fib (n - 1) + fib (n - 2))"
        fib_test_values = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60]  # Smaller values due to exponential growth
        results = benchmark.benchmark_function(
            fib_function,
            fib_test_values,
            "Fibonacci (exponential complexity)"
        )
        all_results.extend(results)

        # Print summary and save results
        print_summary(all_results)
        save_results(all_results)

    finally:
        benchmark.cleanup()


if __name__ == "__main__":
    main()
