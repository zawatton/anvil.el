#!/usr/bin/env python3
"""Summarize orchestrator benchmark CSVs into a plain-text report.

Usage:
    python3 analyze.py <bench-*.csv> [<codex-ramp-*.csv>]

Emits to stdout.  No external deps (csv + statistics from stdlib).
"""

import csv
import statistics
import sys
from collections import defaultdict
from pathlib import Path


def _as_float(x, default=0.0):
    try:
        return float(x)
    except (TypeError, ValueError):
        return default


def _as_int(x, default=0):
    try:
        return int(x)
    except (TypeError, ValueError):
        return default


def summarize_matrix(path: Path) -> str:
    rows = list(csv.DictReader(path.open()))
    by_cell = defaultdict(list)
    for r in rows:
        by_cell[(r["scenario"], r["condition"])].append(r)

    out = []
    out.append(f"# Matrix summary — {path.name}")
    out.append(f"Total rows: {len(rows)}")
    out.append("")
    hdr = f"{'scenario':<8} {'cond':<4} {'n':>2}  {'wall_ms p50':>12} {'wall_ms p95':>12} {'serial_ms mean':>16} {'cost_usd mean':>14} {'pass/total':>10}"
    out.append(hdr)
    out.append("-" * len(hdr))

    for (scen, cond), cell in sorted(by_cell.items()):
        walls = sorted(_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in cell)
        serials = [_as_int(r.get("serial-sum-ms", r.get("serial_sum_ms", 0))) for r in cell]
        costs = [_as_float(r.get("cost-usd", r.get("cost_usd", 0))) for r in cell]
        passes = [(_as_int(r["pass"]), _as_int(r["total"])) for r in cell]
        n = len(cell)
        p50 = walls[n // 2] if n else 0
        p95 = walls[max(0, int(n * 0.95) - 1)] if n else 0
        serial_mean = int(statistics.mean(serials)) if serials else 0
        cost_mean = statistics.mean(costs) if costs else 0.0
        pass_total = sum(p[0] for p in passes)
        total_total = sum(p[1] for p in passes)
        out.append(
            f"{scen:<8} {cond:<4} {n:>2}  {p50:>12} {p95:>12} {serial_mean:>16} "
            f"{cost_mean:>14.4f} {pass_total:>4}/{total_total:<5}"
        )

    # Speedup table: C3 vs C2 per scenario
    out.append("")
    out.append("## Parallel speedup (C3 wall vs C2 serial, same-provider)")
    for scen in sorted({r["scenario"] for r in rows}):
        c2 = [_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c2"]
        c3 = [_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c3"]
        if c2 and c3:
            m2, m3 = statistics.mean(c2), statistics.mean(c3)
            out.append(f"  {scen}: C2 mean {m2:.0f}ms, C3 mean {m3:.0f}ms → speedup {m2/m3:.2f}x")

    # 3-way wallclock vs C2 serial
    out.append("")
    out.append("## 3-way consensus vs 3x serial claude (wallclock)")
    for scen in sorted({r["scenario"] for r in rows}):
        c2 = [_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c2"]
        c4 = [_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c4"]
        if c2 and c4:
            m2, m4 = statistics.mean(c2), statistics.mean(c4)
            out.append(f"  {scen}: serial-3x-claude {m2:.0f}ms, consensus-3way {m4:.0f}ms → {m2/m4:.2f}x")

    # Cost: C2 vs C4 (3-way fan-out saves $ because codex+ollama are free)
    out.append("")
    out.append("## Marginal cost: 3x claude vs 3-way fan-out")
    for scen in sorted({r["scenario"] for r in rows}):
        c2_cost = [_as_float(r.get("cost-usd", r.get("cost_usd", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c2"]
        c4_cost = [_as_float(r.get("cost-usd", r.get("cost_usd", 0))) for r in rows if r["scenario"] == scen and r["condition"] == "c4"]
        if c2_cost and c4_cost:
            m2, m4 = statistics.mean(c2_cost), statistics.mean(c4_cost)
            saved = (m2 - m4) / m2 * 100 if m2 else 0
            out.append(f"  {scen}: C2 ${m2:.4f}/batch, C4 ${m4:.4f}/batch → {saved:+.0f}% cost change")

    # Validation
    out.append("")
    out.append("## Assertion pass rate by condition")
    by_cond = defaultdict(list)
    for r in rows:
        p, t = _as_int(r["pass"]), _as_int(r["total"])
        if t:
            by_cond[r["condition"]].append((p, t))
    for cond in sorted(by_cond):
        total_p = sum(p for p, _ in by_cond[cond])
        total_t = sum(t for _, t in by_cond[cond])
        rate = total_p / total_t * 100 if total_t else 0
        out.append(f"  {cond}: {total_p}/{total_t} ({rate:.1f}%)")

    return "\n".join(out) + "\n"


def summarize_ramp(path: Path) -> str:
    rows = list(csv.DictReader(path.open()))
    out = []
    out.append(f"# Codex concurrency ramp — {path.name}")
    out.append(f"{'level':>5} {'n':>2} {'wall_ms mean':>12} {'wall_ms stddev':>14} {'failed':>6} {'done':>4}")
    out.append("-" * 55)
    by_level = defaultdict(list)
    for r in rows:
        by_level[_as_int(r["level"])].append(r)
    for lvl, cell in sorted(by_level.items()):
        walls = [_as_int(r.get("wall-ms", r.get("wall_ms", 0))) for r in cell]
        fails = sum(_as_int(r["failed"]) for r in cell)
        dones = sum(_as_int(r["done"]) for r in cell)
        mean = statistics.mean(walls) if walls else 0
        sdev = statistics.stdev(walls) if len(walls) > 1 else 0
        out.append(f"{lvl:>5} {len(cell):>2} {mean:>12.0f} {sdev:>14.0f} {fails:>6} {dones:>4}")
    return "\n".join(out) + "\n"


def main(argv):
    if len(argv) < 2:
        print(__doc__.strip())
        sys.exit(1)
    for arg in argv[1:]:
        p = Path(arg)
        if "codex-ramp" in p.name:
            print(summarize_ramp(p))
        else:
            print(summarize_matrix(p))


if __name__ == "__main__":
    main(sys.argv)
