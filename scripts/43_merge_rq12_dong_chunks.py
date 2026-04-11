from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "src"

import sys

if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

from mobility_llm.eval_metrics import compute_basic_metrics
from mobility_llm.io import ensure_dir, save_parquet, write_json, write_text
from mobility_llm.pattern_metrics import (
    cpc as compute_cpc,
    delta_beta,
    delta_gini,
    destination_marginal_spearman,
    origin_marginal_spearman,
)


REQUIRED_PREDICTION_COLUMNS = [
    "query_id",
    "scale",
    "y_gt",
    "y_hat",
    "split",
    "orig",
    "dest",
    "hour",
    "dist_km",
]
DUPLICATE_KEY_COLUMNS = ["query_id", "orig", "dest", "hour", "split"]


def _resolve_path(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path.resolve()
    return (PROJECT_ROOT / path).resolve()


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Merge dong chunk RQ12 prediction outputs into one merged result set."
    )
    parser.add_argument(
        "--runs-root",
        required=True,
        help="Root directory containing chunk run folders, e.g. results/runs",
    )
    parser.add_argument(
        "--run-id-prefix",
        required=True,
        help="Prefix used to match chunk run directory names.",
    )
    parser.add_argument(
        "--output-dir",
        required=True,
        help="Directory where merged outputs will be written.",
    )
    return parser


def _compute_metrics_json(pred_df: pd.DataFrame) -> dict:
    def _block(df: pd.DataFrame) -> dict:
        n_eval = int(len(df))
        if n_eval == 0:
            return {
                "n_eval": 0,
                "accuracy": {"mae": None, "rmse": None, "smape": None},
                "patterns": {
                    "delta_beta": None,
                    "rho_origin": None,
                    "rho_destination": None,
                    "delta_gini": None,
                    "cpc": None,
                },
            }
        basic = compute_basic_metrics(df["y_gt"], df["y_hat"])
        return {
            "n_eval": n_eval,
            "accuracy": {
                "mae": basic.get("mae"),
                "rmse": basic.get("rmse"),
                "smape": basic.get("smape"),
            },
            "patterns": {
                "delta_beta": delta_beta(df),
                "rho_origin": origin_marginal_spearman(df),
                "rho_destination": destination_marginal_spearman(df),
                "delta_gini": delta_gini(df),
                "cpc": compute_cpc(df),
            },
        }

    return {
        "all": _block(pred_df),
        "gu": _block(pred_df[pred_df["scale"] == "gu"]),
        "dong": _block(pred_df[pred_df["scale"] == "dong"]),
    }


def _find_matching_run_dirs(runs_root: Path, run_id_prefix: str) -> list[Path]:
    if not runs_root.exists():
        raise FileNotFoundError(f"Runs root not found: {runs_root}")
    if not runs_root.is_dir():
        raise NotADirectoryError(f"Runs root is not a directory: {runs_root}")

    matches = [
        path.resolve()
        for path in runs_root.iterdir()
        if path.is_dir() and path.name.startswith(run_id_prefix)
    ]
    return sorted(matches, key=lambda p: p.name)


def _load_predictions(run_dir: Path) -> pd.DataFrame:
    pred_path = run_dir / "rq12" / "predictions.parquet"
    if not pred_path.exists():
        raise FileNotFoundError(f"Missing required file: {pred_path}")

    df = pd.read_parquet(pred_path)
    missing = [col for col in REQUIRED_PREDICTION_COLUMNS if col not in df.columns]
    if missing:
        raise ValueError(
            f"Predictions file missing required columns ({pred_path}): {', '.join(missing)}"
        )

    return df[REQUIRED_PREDICTION_COLUMNS].copy()


def _validate_no_duplicates(pred_df: pd.DataFrame) -> None:
    dup_mask = pred_df.duplicated(subset=DUPLICATE_KEY_COLUMNS, keep=False)
    if not dup_mask.any():
        return

    dup_rows = pred_df.loc[dup_mask, DUPLICATE_KEY_COLUMNS].sort_values(
        DUPLICATE_KEY_COLUMNS,
        kind="stable",
    )
    sample = dup_rows.head(5).to_dict(orient="records")
    raise ValueError(
        "Duplicate prediction rows found after merge for key columns "
        f"{DUPLICATE_KEY_COLUMNS}. Duplicate row count={int(dup_mask.sum())}. "
        f"Sample keys: {sample}"
    )


def main() -> None:
    args = _build_parser().parse_args()

    runs_root = _resolve_path(args.runs_root)
    output_dir = _resolve_path(args.output_dir)
    ensure_dir(str(output_dir))

    log_path = output_dir / "merge_log.txt"
    write_text(str(log_path), f"RUNS_ROOT: {runs_root}\n", append=False)
    write_text(str(log_path), f"RUN_ID_PREFIX: {args.run_id_prefix}\n")
    write_text(str(log_path), f"OUTPUT_DIR: {output_dir}\n")

    run_dirs = _find_matching_run_dirs(runs_root, args.run_id_prefix)
    if not run_dirs:
        raise FileNotFoundError(
            f"No run directories found under {runs_root} with prefix '{args.run_id_prefix}'."
        )

    write_text(str(log_path), f"N_MATCHING_RUN_DIRS: {len(run_dirs)}\n")

    per_run_manifest: list[dict[str, object]] = []
    frames: list[pd.DataFrame] = []

    for run_dir in run_dirs:
        pred_df = _load_predictions(run_dir)
        n_rows = int(len(pred_df))
        frames.append(pred_df)
        per_run_manifest.append(
            {
                "run_dir_name": run_dir.name,
                "run_dir_path": str(run_dir),
                "predictions_path": str(run_dir / "rq12" / "predictions.parquet"),
                "n_rows": n_rows,
            }
        )
        write_text(str(log_path), f"LOADED {run_dir.name}: n_rows={n_rows}\n")

    merged_df = pd.concat(frames, ignore_index=True)
    _validate_no_duplicates(merged_df)

    merged_metrics = _compute_metrics_json(merged_df)

    predictions_out = output_dir / "dong_predictions_merged.parquet"
    metrics_out = output_dir / "dong_metrics_merged.json"
    manifest_out = output_dir / "merge_manifest.json"

    save_parquet(merged_df, str(predictions_out))
    write_json(str(metrics_out), merged_metrics)

    manifest = {
        "runs_root": str(runs_root),
        "run_id_prefix": args.run_id_prefix,
        "number_of_chunk_runs_found": len(per_run_manifest),
        "source_run_directories": [item["run_dir_path"] for item in per_run_manifest],
        "per_run_row_counts": per_run_manifest,
        "total_merged_row_count": int(len(merged_df)),
        "output_predictions_path": str(predictions_out),
        "output_metrics_path": str(metrics_out),
    }
    write_json(str(manifest_out), manifest)

    write_text(str(log_path), f"TOTAL_MERGED_ROWS: {len(merged_df)}\n")
    write_text(str(log_path), f"WROTE {predictions_out.name}\n")
    write_text(str(log_path), f"WROTE {metrics_out.name}\n")
    write_text(str(log_path), f"WROTE {manifest_out.name}\n")

    print(f"Found {len(per_run_manifest)} chunk runs under {runs_root}")
    print(f"Merged rows: {len(merged_df)}")
    print(f"Saved merged predictions: {predictions_out}")
    print(f"Saved merged metrics: {metrics_out}")
    print(f"Saved merge manifest: {manifest_out}")


if __name__ == "__main__":
    main()
