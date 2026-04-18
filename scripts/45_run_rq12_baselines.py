from __future__ import annotations

import argparse
import sys
import traceback
from datetime import datetime
from pathlib import Path
from typing import Any

import pandas as pd
import yaml

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "src"
if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

from mobility_llm.eval_metrics import compute_basic_metrics
from mobility_llm.io import ensure_dir, load_parquet, save_parquet, write_json, write_text
from mobility_llm.pattern_metrics import (
    cpc as compute_cpc,
    delta_beta,
    delta_gini,
    destination_marginal_spearman,
    origin_marginal_spearman,
)
from mobility_llm.prompts import build_prompts_df
from mobility_llm.rq12_baselines import (
    BASELINE_NAMES,
    fit_hourly_mean,
    fit_loglinear_gravity,
    fit_od_marginal_product,
    predict_hourly_mean,
    predict_loglinear_gravity,
    predict_od_marginal_product,
    prepare_canonical_rq12_df,
)
from mobility_llm.split import attach_split, make_group_split, validate_split


def _log_progress(log_paths: Path | list[Path] | tuple[Path, ...] | None, message: str) -> None:
    line = f"[PROGRESS] {message}"
    print(line, flush=True)
    if log_paths is None:
        return
    if isinstance(log_paths, Path):
        paths = [log_paths]
    else:
        paths = list(log_paths)
    seen: set[Path] = set()
    for path in paths:
        if path in seen:
            continue
        write_text(str(path), line + "\n")
        seen.add(path)


def _abs_path(path_str: str) -> Path:
    p = Path(path_str)
    if not p.is_absolute():
        return (PROJECT_ROOT / p).resolve()
    return p.resolve()


def _get_nested(data: dict[str, Any], path: list[str]) -> Any:
    current: Any = data
    for key in path:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def _load_config(path: str, scale: str) -> dict[str, Any]:
    config_path = Path(path)
    if not config_path.exists():
        raise ValueError(f"Config file not found: {path}")

    try:
        with config_path.open("r", encoding="utf-8") as f:
            config = yaml.safe_load(f)
    except yaml.YAMLError as exc:
        raise ValueError(f"Invalid YAML format: {path}") from exc

    if not isinstance(config, dict):
        raise ValueError(f"Config must be a YAML mapping: {path}")

    columns = config.setdefault("columns", {})
    if isinstance(columns, dict):
        if "orig" not in columns and "origin_id" in columns:
            columns["orig"] = columns["origin_id"]
        if "dest" not in columns and "dest_id" in columns:
            columns["dest"] = columns["dest_id"]

    required_paths = [
        ["schema_version"],
        ["data", "datasets", scale, "path"],
        ["columns", "orig"],
        ["columns", "dest"],
        ["columns", "hour"],
        ["columns", "dist_km"],
        ["columns", "flow_gt"],
        ["prompt", "template"],
    ]

    missing = []
    for req_path in required_paths:
        value = _get_nested(config, req_path)
        if value is None or (isinstance(value, str) and value.strip() == ""):
            missing.append(".".join(req_path))

    if missing:
        raise ValueError("Missing required config fields: " + ", ".join(missing))

    return config


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


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run fair RQ12 baselines for gu or dong.")
    parser.add_argument("--config", required=True, help="Path to config YAML")
    parser.add_argument("--scale", required=True, choices=["gu", "dong"], help="Dataset scale")
    return parser


def _predict_for_baseline(name: str, train_df: pd.DataFrame, test_df: pd.DataFrame):
    if name == "hourly_mean":
        model = fit_hourly_mean(train_df)
        return predict_hourly_mean(model, test_df)
    if name == "od_marginal_product":
        model = fit_od_marginal_product(train_df)
        return predict_od_marginal_product(model, test_df)
    if name == "loglinear_gravity":
        model = fit_loglinear_gravity(train_df)
        return predict_loglinear_gravity(model, test_df)
    raise ValueError(f"Unsupported baseline: {name}")


def main() -> None:
    log_path: Path | None = None
    try:
        args = _build_parser().parse_args()
        config = _load_config(args.config, args.scale)

        ts = datetime.now().strftime("%Y%m%d_%H%M%S")
        run_dir = (PROJECT_ROOT / "results" / "baselines" / f"{args.scale}_rq12_baselines_{ts}").resolve()
        ensure_dir(str(run_dir))

        log_path = run_dir / "log.txt"
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n", append=False)
        _log_progress(log_path, f"START config={args.config} scale={args.scale}")
        _log_progress(log_path, f"CONFIG_LOADED scale={args.scale}")

        dataset_path = _abs_path(str(config["data"]["datasets"][args.scale]["path"]))
        df = load_parquet(str(dataset_path)).copy()
        _log_progress(log_path, f"DATA_LOADED path={dataset_path} rows={len(df)}")

        prompts_df = build_prompts_df(df, args.scale, config)
        data_df = prepare_canonical_rq12_df(prompts_df)
        _log_progress(log_path, f"CANONICAL_DF_READY rows={len(data_df)} scale={args.scale}")

        split_seed = int(config.get("run", {}).get("seed", 202511))
        splits_df = make_group_split(
            pairs_df=data_df[["pair_id"]],
            seed=split_seed,
            ratios={"train": 0.7, "val": 0.1, "test": 0.2},
            method="hash",
        )
        data_split_df = attach_split(data_df, splits_df)
        validate_split(data_split_df, expected_hours=24, hour_col="hour")
        save_parquet(splits_df, str(run_dir / "splits.parquet"))

        train_n = int((data_split_df["split"] == "train").sum())
        val_n = int((data_split_df["split"] == "val").sum())
        test_n = int((data_split_df["split"] == "test").sum())
        _log_progress(
            log_path,
            f"SPLIT_READY train_rows={train_n} val_rows={val_n} test_rows={test_n}",
        )

        train_df = data_split_df.loc[data_split_df["split"] == "train"].copy()
        test_df = data_split_df.loc[data_split_df["split"] == "test"].copy()

        for baseline_name in BASELINE_NAMES:
            baseline_dir = run_dir / baseline_name
            ensure_dir(str(baseline_dir))
            baseline_log = baseline_dir / "log.txt"
            write_text(str(baseline_log), f"RUN_DIR: {baseline_dir}\n", append=False)
            _log_progress(
                [log_path, baseline_log],
                f"BASELINE_START name={baseline_name} train_rows={len(train_df)} test_rows={len(test_df)}",
            )

            y_hat = _predict_for_baseline(baseline_name, train_df, test_df)
            pred_test_df = test_df[
                ["query_id", "scale", "y_gt", "split", "orig", "dest", "hour", "dist_km"]
            ].copy()
            pred_test_df["y_hat"] = y_hat
            pred_test_df = pred_test_df[
                ["query_id", "scale", "y_gt", "y_hat", "split", "orig", "dest", "hour", "dist_km"]
            ]

            save_parquet(pred_test_df, str(baseline_dir / "predictions.parquet"))
            metrics = _compute_metrics_json(pred_test_df)
            write_json(str(baseline_dir / "metrics.json"), metrics)
            _log_progress(
                [log_path, baseline_log],
                f"BASELINE_DONE name={baseline_name} n_test_rows={len(pred_test_df)}",
            )

        write_json(str(run_dir / "config_snapshot.json"), config)
        _log_progress(log_path, "RUN_DONE")
    except Exception as e:
        _log_progress(log_path, f"RUN_FAILED error={type(e).__name__}: {e}")
        traceback.print_exc()
        print("EXCEPTION:", e)
        raise


if __name__ == "__main__":
    main()
