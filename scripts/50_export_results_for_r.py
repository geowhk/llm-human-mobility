from __future__ import annotations

import json
from pathlib import Path
import pandas as pd


RESULTS_ROOT = Path("results/runs")
OUTPUT_DIR = Path("results/analysis_tables")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


def find_run_dirs():
    runs = []
    for d in RESULTS_ROOT.iterdir():
        if d.is_dir() and "dev" in d.name:
            runs.append(d)
    return runs


def detect_variant(run_dir: Path) -> str:
    cfg = run_dir / "config_snapshot.json"
    if not cfg.exists():
        return "unknown"

    with open(cfg, "r", encoding="utf-8") as f:
        data = json.load(f)

    return data.get("prompt", {}).get("variant", "original")


def load_rq12_metrics(run_dir: Path, variant: str):

    metrics_path = run_dir / "rq12" / "metrics.json"

    if not metrics_path.exists():
        return None

    with open(metrics_path, "r", encoding="utf-8") as f:
        metrics = json.load(f)

    rows = []

    for scale in ["gu", "dong", "all"]:

        block = metrics.get(scale)
        if block is None:
            continue

        rows.append({
            "variant": variant,
            "scale": scale,
            "n_eval": block["n_eval"],
            "mae": block["accuracy"]["mae"],
            "rmse": block["accuracy"]["rmse"],
            "smape": block["accuracy"]["smape"],
            "cpc": block["patterns"]["cpc"],
            "delta_beta": block["patterns"]["delta_beta"],
            "rho_origin": block["patterns"]["rho_origin"],
            "rho_destination": block["patterns"]["rho_destination"],
            "delta_gini": block["patterns"]["delta_gini"],
        })

    return pd.DataFrame(rows)


def load_rq3_results(run_dir: Path, variant: str):

    align_path = run_dir / "rq3" / "rq3_role_alignment.parquet"
    probe_path = run_dir / "rq3" / "rq3_distance_probe.parquet"

    if not align_path.exists():
        return None

    align = pd.read_parquet(align_path)

    if probe_path.exists():

        probe = pd.read_parquet(probe_path)

        # distance probe 결과 컬럼 이름 수정
        if "metric_value" in probe.columns:
            probe = probe.rename(columns={"metric_value": "probe_accuracy"})

        probe = probe[["layer", "probe_accuracy"]]

        align = align.merge(
            probe,
            on="layer",
            how="left"
        )

    align["variant"] = variant

    return align


def main():

    run_dirs = find_run_dirs()

    rq12_tables = []
    rq3_tables = []

    for run_dir in run_dirs:

        variant = detect_variant(run_dir)

        rq12 = load_rq12_metrics(run_dir, variant)
        if rq12 is not None:
            rq12_tables.append(rq12)

        rq3 = load_rq3_results(run_dir, variant)
        if rq3 is not None:
            rq3_tables.append(rq3)

    if rq12_tables:
        rq12_all = pd.concat(rq12_tables, ignore_index=True)
        rq12_all.to_parquet(
            OUTPUT_DIR / "rq12_metrics.parquet",
            index=False
        )

    if rq3_tables:
        rq3_all = pd.concat(rq3_tables, ignore_index=True)
        rq3_all.to_parquet(
            OUTPUT_DIR / "rq3_representation.parquet",
            index=False
        )

    print("Export complete.")
    print("Saved to:", OUTPUT_DIR)


if __name__ == "__main__":
    main()