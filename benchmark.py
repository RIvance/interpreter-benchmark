#!/usr/bin/env python3
"""
Benchmarking script for STLC interpreter.

Interpreter CLI:
  Usage: interpreter [options] <file>
    -i, --interpreter <type>   substitution (default), recursive, trampoline, worklist
"""

import argparse
import concurrent.futures
import json
import os
import shutil
import statistics
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# ---------- Utilities ----------

def read_text(p: Path) -> str:
    return p.read_text(encoding="utf-8")

def discover_stlc_files(root: Path) -> List[Path]:
    """Find all *.stlc files under root (recursive), sorted for stability."""
    return sorted(root.rglob("*.stlc"))

def first_line_description(src: str) -> Optional[str]:
    """
    Extract description from the first non-empty line if it starts with `//`.
    E.g.:
      // Linear tail recursion
      fix sum: Int -> Int. \n: Int. ...
    """
    for line in src.splitlines():
        s = line.strip()
        if not s:
            continue
        if s.startswith("//"):
            name = s[2:].strip()
            return name if name else None
        # stop scanning once we hit a non-comment, non-empty line
        return None
    return None

# ---------- Results container ----------

class BenchmarkResult:
    def __init__(self, n: int, mode: str, time_sec: Optional[float],
                 stack_overflow: bool, timeout_sec: Optional[float] = None,
                 error: Optional[str] = None):
        self.n = n
        self.mode = mode  # "substitution", "recursive", "trampoline", "worklist", ...
        self.time_sec = time_sec
        self.stack_overflow = stack_overflow
        self.timeout_sec = timeout_sec  # None if no timeout; else seconds
        self.error = error

    def __repr__(self):
        if self.stack_overflow:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, stack_overflow=True)"
        if self.timeout_sec is not None:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, timeout={self.timeout_sec:.1f}s)"
        if self.error:
            return f"BenchmarkResult(n={self.n}, mode={self.mode}, error={self.error})"
        return f"BenchmarkResult(n={self.n}, mode={self.mode}, time={self.time_sec:.4f}s)"

# ---------- Benchmark engine ----------

class STLCBenchmark:
    def __init__(self, binary_path: str, num_workers: Optional[int] = None):
        self.binary_path = binary_path
        self.temp_dir = tempfile.mkdtemp(prefix="stlc_bench_")
        self._temp_file_counter = 0
        self._temp_file_lock = threading.Lock()
        self.global_workers = num_workers or (os.cpu_count() or 4)

    def cleanup(self):
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def _create_temp_file(self, program: str) -> str:
        with self._temp_file_lock:
            self._temp_file_counter += 1
            temp_file = os.path.join(
                self.temp_dir, f"test_{self._temp_file_counter}_{time.time()}.stlc"
            )
        with open(temp_file, "w", encoding="utf-8") as f:
            f.write(program)
        return temp_file

    def run_interpreter_single(self, program: str, mode: str, timeout: int, n: int) -> BenchmarkResult:
        """Run the interpreter once for a given program/mode."""
        temp_file = self._create_temp_file(program)
        cmd = [self.binary_path, "-i", mode, temp_file]

        try:
            start_time = time.time()
            result = subprocess.run(
                cmd, capture_output=True, text=True, timeout=timeout
            )
            elapsed = time.time() - start_time

            if result.returncode != 0:
                stderr_low = (result.stderr or "").lower()
                # Detect stack overflow
                if "stack overflow" in stderr_low or "stackoverflow" in stderr_low:
                    return BenchmarkResult(n, mode, None, stack_overflow=True)
                # Distinguish other errors
                return BenchmarkResult(n, mode, None, stack_overflow=False, error=result.stderr.strip() or "Unknown error")

            return BenchmarkResult(n, mode, elapsed, stack_overflow=False)

        except subprocess.TimeoutExpired:
            # Record timeout seconds explicitly
            return BenchmarkResult(n, mode, None, stack_overflow=False, timeout_sec=float(timeout))
        except Exception as e:
            return BenchmarkResult(n, mode, None, stack_overflow=False, error=str(e))
        finally:
            try:
                os.remove(temp_file)
            except Exception:
                pass

    def run_benchmark_multiple_times(
        self,
        program: str,
        mode: str,
        n: int,
        num_runs: int,
        timeout: int
    ) -> List[BenchmarkResult]:
        """Run the same benchmark multiple times in parallel threads (per-mode)."""
        workers = min(num_runs, self.global_workers)
        print(f"[{mode}] {num_runs} runs with {workers} workers... ", end="", flush=True)

        results: List[BenchmarkResult] = []
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
            futs = [
                ex.submit(self.run_interpreter_single, program, mode, timeout, n)
                for _ in range(num_runs)
            ]
            for fut in concurrent.futures.as_completed(futs):
                results.append(fut.result())

        print("done.")
        return results

    def run_all_modes_parallel_if_possible(
        self,
        program: str,
        n: int,
        modes: List[str],
        num_runs: int,
        timeout: int
    ) -> Dict[str, List[BenchmarkResult]]:
        """
        If we have enough workers (num_workers >= len(modes) * num_runs),
        run *all interpreters* in parallel (each with num_runs tasks).
        Otherwise, fall back to per-mode parallelism.
        """
        total_tasks = len(modes) * num_runs
        if self.global_workers >= total_tasks:
            print(f"[all modes] Parallel across interpreters: {len(modes)} modes × {num_runs} runs "
                  f"(workers={self.global_workers})")
            results_by_mode: Dict[str, List[BenchmarkResult]] = {m: [] for m in modes}
            with concurrent.futures.ThreadPoolExecutor(max_workers=self.global_workers) as ex:
                futs: Dict[concurrent.futures.Future, Tuple[str, int]] = {}
                for m in modes:
                    for i in range(num_runs):
                        fut = ex.submit(self.run_interpreter_single, program, m, timeout, n)
                        futs[fut] = (m, i)
                for fut in concurrent.futures.as_completed(futs):
                    m, _ = futs[fut]
                    results_by_mode[m].append(fut.result())
            # Print mini summaries like the per-mode branch would
            for m in modes:
                rs = results_by_mode[m]
                successful = [r for r in rs if (r.time_sec is not None)]
                timeouts = [r for r in rs if r.timeout_sec is not None]
                overflows = any(r.stack_overflow for r in rs)
                if overflows:
                    print(f"  {m:12} STACK OVERFLOW")
                elif timeouts and not successful:
                    tsec = max(t.timeout_sec or 0 for t in timeouts)
                    print(f"  {m:12} TIMEOUT ({int(tsec)}s)")
                elif successful:
                    times = [r.time_sec for r in successful]
                    avg = statistics.mean(times)
                    std = statistics.stdev(times) if len(times) > 1 else 0.0
                    note = ""
                    if len(successful) != num_runs:
                        note = f"  ({len(successful)}/{num_runs} successful)"
                    print(f"  {m:12} {avg:.6f}s ± {std:.6f}s{note}")
                else:
                    # Only errors
                    errs = [r.error for r in rs if r.error]
                    uniq = sorted(set(errs))
                    print(f"  {m:12} ERROR: {uniq[0] if uniq else 'Unknown error'}")
            return results_by_mode

        # Fallback: per-mode loop with per-mode parallel runs
        results_by_mode = {}
        for mode in modes:
            results_by_mode[mode] = self.run_benchmark_multiple_times(
                program, mode, n, num_runs=num_runs, timeout=timeout
            )
        return results_by_mode

    # ------------- File-based benchmark -------------

    def benchmark_file(
        self,
        file_path: Path,
        test_values: List[int],
        test_index: int,
        num_runs: int,
        timeout: int
    ) -> List[Dict]:
        """
        For a single .stlc file:
          - Print its code
          - For each n, run all modes num_runs times (in parallel)
          - Append ' n' to the file content when invoking (treat file as Int -> *)
        Returns a list of JSON-ready entries (one per n).
        """
        src = read_text(file_path)
        name = first_line_description(src) or f"Test {test_index}"

        # Pretty header + print program
        banner = f"=== {name} ==="
        print("\n" + "=" * len(banner))
        print(banner)
        print("=" * len(banner))
        print(f"[File] {file_path}")
        print("\n--- Program ---")
        print(src.rstrip())
        print("---------------")

        # Modes to test (per updated CLI)
        modes = ["substitution", "recursive", "trampoline", "worklist"]

        def mode_entry_failed(d: Dict) -> bool:
            """
            A mode has failed if:
              - stack_overflow is True, OR
              - there are no times AND (timeout > 0 OR there are errors)
            """
            if d.get("stack_overflow"):
                return True
            no_times = not d.get("times")
            timed_out = bool(d.get("timeout", 0))
            has_errors = bool(d.get("errors"))
            return no_times and (timed_out or has_errors)

        all_n_results: List[Dict] = []

        for n in test_values:
            print(f"\n>> n = {n}")

            # Program is "function" applied to n (wrap for precedence safety)
            program_to_run = "(\n" + src.rstrip() + "\n)" + f"\n {n}\n"

            # ---- Run all modes (possibly in parallel across interpreters) ----
            runs_per_mode: Dict[str, List[BenchmarkResult]] = self.run_all_modes_parallel_if_possible(
                program_to_run, n, modes, num_runs=num_runs, timeout=timeout
            )

            # ---- Aggregate per-mode results ----
            mode_results: Dict[str, Dict] = {}
            for mode, run_results in runs_per_mode.items():
                successful = [r for r in run_results if (r.time_sec is not None)]
                had_overflow = any(r.stack_overflow for r in run_results)
                timeouts = [r for r in run_results if r.timeout_sec is not None]
                other_errors = [r for r in run_results if (r.error is not None)]

                if had_overflow:
                    mode_results[mode] = {
                        "times": None,
                        "average_time": None,
                        "std_dev": None,
                        "stack_overflow": True,
                        "timeout": 0,
                        "errors": None,
                        "successful_runs": 0,
                        "total_runs": num_runs,
                    }
                elif timeouts and not successful:
                    tout_sec = max(t.timeout_sec or 0 for t in timeouts)
                    mode_results[mode] = {
                        "times": None,
                        "average_time": None,
                        "std_dev": None,
                        "stack_overflow": False,
                        "timeout": float(tout_sec),
                        "errors": None if not other_errors else list({e.error for e in other_errors}),
                        "successful_runs": len(successful),
                        "total_runs": num_runs,
                    }
                elif other_errors and not successful:
                    uniq = sorted({e.error for e in other_errors})
                    mode_results[mode] = {
                        "times": None,
                        "average_time": None,
                        "std_dev": None,
                        "stack_overflow": False,
                        "timeout": 0,
                        "errors": uniq,
                        "successful_runs": 0,
                        "total_runs": num_runs,
                    }
                else:
                    times = [r.time_sec for r in successful]
                    avg = statistics.mean(times) if times else None
                    std = statistics.stdev(times) if len(times) > 1 else 0.0
                    tout_sec = max((t.timeout_sec or 0) for t in timeouts) if timeouts else 0
                    err_list = sorted({e.error for e in other_errors}) if other_errors else None

                    mode_results[mode] = {
                        "times": times,
                        "average_time": avg,
                        "std_dev": std,
                        "stack_overflow": False,
                        "timeout": float(tout_sec),
                        "errors": err_list,
                        "successful_runs": len(successful),
                        "total_runs": num_runs,
                    }

            # Record this n’s results
            all_n_results.append(
                {
                    "n": n,
                    "description": name,
                    "file": str(file_path),
                    "modes": mode_results,
                }
            )

            # --------- EARLY STOP: if all modes failed (overflow OR timeout OR error) ---------
            if all(mode_entry_failed(mode_results[m]) for m in modes):
                print(f"\n  Early stop at n={n}: all modes failed (overflow/timeout/error).")
                break
            # ----------------------------------------------------------------------------------

        return all_n_results

# ---------- Analysis ----------

def save_results(results: List[Dict], output_file: str):
    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2)
    print(f"\nResults saved to {output_file}")

def load_results(filename: str) -> List[Dict]:
    with open(filename, "r", encoding="utf-8") as f:
        return json.load(f)

def _format_time_with_ratio(mode_data: Dict, baseline_time: Optional[float], width=26) -> str:
    """Format time ± std and speedup vs baseline (slowest success)."""
    if mode_data.get("stack_overflow"):
        return "OVERFLOW".center(width)
    if mode_data.get("times") is None:
        if mode_data.get("timeout", 0):
            return "TIMEOUT".center(width)
        if mode_data.get("errors"):
            return "ERROR".center(width)
        return "N/A".rjust(width)

    avg = mode_data.get("average_time")
    std = mode_data.get("std_dev", 0.0) or 0.0
    if avg is None:
        return "N/A".rjust(width)

    base = baseline_time
    time_str = f"{avg:.6f}±{std:.4f}"
    if base is not None and base > 0:
        ratio = base / avg
        return f"{time_str} ({ratio:.2f}x)".rjust(width)
    return time_str.rjust(width)

def _read_file_for_report(path_str: str) -> Optional[str]:
    """Best-effort file read for summary printing."""
    try:
        p = Path(path_str)
        if p.exists():
            return p.read_text(encoding="utf-8").rstrip()
    except Exception:
        pass
    return None

def print_detailed_analysis(results: List[Dict]):
    print("=" * 128)
    print("DETAILED BENCHMARK ANALYSIS")
    print("=" * 128)

    # Group by (description, file) so we can show the exact code once per file.
    grouped: Dict[Tuple[str, str], List[Dict]] = {}
    for r in results:
        desc = r.get("description", "Unnamed")
        file_path = r.get("file", "<in-memory>")
        grouped.setdefault((desc, file_path), []).append(r)

    for (desc, file_path), group in grouped.items():
        # Discover union of modes across rows (stable order)
        mode_order: List[str] = []
        seen = set()
        for row in group:
            for m in row["modes"].keys():
                if m not in seen:
                    seen.add(m)
                    mode_order.append(m)

        # Header
        print(f"\n{'='*128}")
        print(f"Benchmark: {desc}")
        print(f"{'='*128}")

        # Print the code (best-effort)
        code_text = _read_file_for_report(file_path)
        if code_text is not None:
            print(f"[File] {file_path}")
            print(code_text)
        else:
            print(f"[File] {file_path} (source unavailable)")

        print(f"{'='*128}")

        # Table header (auto-extends with modes)
        cols = ["n"] + [m.capitalize() for m in mode_order]
        widths = [10] + [26] * len(mode_order)
        header = " | ".join(f"{c:>{w}}" for c, w in zip(cols, widths))
        print(header)
        print("-" * len(header))

        # Rows
        for r in group:
            n = r["n"]
            modes = r["modes"]

            # Baseline = slowest successful avg time among available modes for this row
            successful_times = [
                d.get("average_time")
                for d in modes.values()
                if (not d.get("stack_overflow"))
                and (not d.get("errors"))
                and d.get("times")
                and (d.get("average_time") is not None)
            ]
            baseline_time = max(successful_times) if successful_times else None

            fields = [f"{n:>10}"]
            for m in mode_order:
                d = modes.get(m, {})
                fields.append(_format_time_with_ratio(d, baseline_time, width=26))
            print(" | ".join(fields))


# ---------- Main ----------

def main():
    parser = argparse.ArgumentParser(
        description="Benchmark STLC interpreter across evaluation strategies."
    )
    parser.add_argument("binary_path", nargs="?", help="Path to the interpreter executable")
    parser.add_argument("--runs", type=int, default=5, help="Number of runs per (file, n, mode)")
    parser.add_argument("--timeout", type=int, default=60, help="Per-run timeout seconds")
    parser.add_argument("--num-workers", type=int, default=None, help="Global max parallel workers (default: CPU count)")
    parser.add_argument("--values", type=int, nargs="+", help="Custom list of n values (default: powers of two 1..2^24)")
    parser.add_argument("--root", default=".", help="Root directory to search for *.stlc files (default: '.')")
    parser.add_argument("--output", default="benchmark_results.json", help="Output JSON path (default: benchmark_results.json)")
    parser.add_argument("--load", metavar="RESULTS_JSON", help="Load historical results from JSON and print analysis (skip benchmarking)")

    args = parser.parse_args()

    # If we're in "load only" mode, skip benchmarking
    if args.load:
        hist = load_results(args.load)
        print_detailed_analysis(hist)
        return

    # Otherwise, we must have a binary to run
    if not args.binary_path:
        print("Error: binary_path is required unless --load is used.")
        sys.exit(1)

    if not os.path.exists(args.binary_path):
        print(f"Error: Binary not found at {args.binary_path}")
        sys.exit(1)

    # Try --help to sanity check
    try:
        help_out = subprocess.run([args.binary_path, "--help"], capture_output=True, text=True)
        if help_out.returncode != 0:
            print("Warning: interpreter --help returned non-zero exit code.")
    except Exception as e:
        print(f"Warning: Failed to run --help on interpreter: {e}")

    # Values to test
    if args.values:
        test_values = args.values
    else:
        test_values = [2 ** i for i in range(0, 25)]  # 1 .. 2^24

    # Discover files
    root = Path(args.root).resolve()
    files = discover_stlc_files(root)
    if not files:
        print(f"No *.stlc files found under {root}")
        sys.exit(0)

    print(f"Discovered {len(files)} *.stlc files under {root}.\n")

    bench = STLCBenchmark(args.binary_path, num_workers=args.num_workers)
    all_results: List[Dict] = []

    try:
        for idx, fpath in enumerate(files, start=1):
            file_results = bench.benchmark_file(
                fpath,
                test_values=test_values,
                test_index=idx,
                num_runs=args.runs,
                timeout=args.timeout,
            )
            all_results.extend(file_results)

        # Print robust analysis for fresh results
        print_detailed_analysis(all_results)
        save_results(all_results, args.output)

    finally:
        bench.cleanup()

if __name__ == "__main__":
    main()
