from __future__ import annotations

import argparse
import re
import sys
import traceback
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import yaml

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "src"
if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

from mobility_llm.eval_metrics import compute_basic_metrics
from mobility_llm.forward_cache import cache_exists, compute_cache_key, ensure_cache, get_cache_dir
from mobility_llm.io import ensure_dir, load_parquet, save_parquet, write_json, write_text
from mobility_llm.pattern_metrics import (
    cpc as compute_cpc,
    delta_beta,
    delta_gini,
    destination_marginal_spearman,
    origin_marginal_spearman,
)
from mobility_llm.prompts import build_prompts_df
from mobility_llm.readout import predict_readout, train_readout
from mobility_llm.split import attach_split, make_group_split, make_pair_id, validate_split


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


def _load_dong_config(path: str) -> dict[str, Any]:
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
        ["data", "datasets", "dong", "path"],
        ["columns", "orig"],
        ["columns", "dest"],
        ["columns", "hour"],
        ["columns", "dist_km"],
        ["columns", "flow_gt"],
        ["prompt", "template"],
        ["model", "model_id"],
    ]

    missing = []
    for req_path in required_paths:
        value = _get_nested(config, req_path)
        if value is None or (isinstance(value, str) and value.strip() == ""):
            missing.append(".".join(req_path))

    if missing:
        raise ValueError("Missing required config fields: " + ", ".join(missing))

    return config


def _resolve_run_id(config: dict[str, Any]) -> str:
    run_cfg = config.get("run", {})
    run_id = str(run_cfg.get("run_id", "")).strip()
    if run_id:
        return run_id
    return str(run_cfg.get("mode", "run")).strip() or "run"


def _extract_chunk_id(*values: str) -> str | None:
    for value in values:
        match = re.search(r"(chunk[_-]?\d+)", value, flags=re.IGNORECASE)
        if match:
            return match.group(1)
    return None


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


def main() -> None:
    try:
        parser = argparse.ArgumentParser()
        parser.add_argument("--config", required=True, help="Path to config YAML")
        args = parser.parse_args()

        config = _load_dong_config(args.config)
        run_mode = str(config.get("run", {}).get("mode", "run")).strip() or "run"
        resolved_run_id = _resolve_run_id(config)
        output_root_cfg = str(config.get("run", {}).get("output_root", "results/runs"))

        ts = datetime.now().strftime("%Y%m%d_%H%M%S")

        output_root = Path(output_root_cfg)
        if not output_root.is_absolute():
            output_root = (PROJECT_ROOT / output_root).resolve()
        else:
            output_root = output_root.resolve()
        run_dir = output_root / f"{resolved_run_id}_rq12_dong_chunk_{ts}"

        rq12_dir = run_dir / "rq12"

        ensure_dir(str(run_dir))
        ensure_dir(str(rq12_dir))

        log_path = run_dir / "log.txt"
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n", append=False)
        write_text(str(log_path), f"RUN_ID_RESOLVED: {resolved_run_id}\n")
        if resolved_run_id != run_mode:
            write_text(str(log_path), f"RUN_MODE: {run_mode}\n")

        dong_path_raw = str(config["data"]["datasets"]["dong"]["path"])
        dong_abs = _abs_path(dong_path_raw)
        input_path = str(dong_abs)

        write_text(str(log_path), f"INPUT_DONG: {dong_abs}\n")
        chunk_id = _extract_chunk_id(resolved_run_id, dong_abs.stem)
        if chunk_id is not None:
            write_text(str(log_path), f"CHUNK_ID: {chunk_id}\n")

        dong_df = load_parquet(str(dong_abs)).copy()
        prompts_dong = build_prompts_df(dong_df, "dong", config)

        cache_key = compute_cache_key(input_path, config)
        cache_dir = get_cache_dir(PROJECT_ROOT, cache_key)
        cache_hit = cache_exists(cache_dir)
        cached = ensure_cache(
            project_root=PROJECT_ROOT,
            config=config,
            input_path=input_path,
            df=prompts_dong,
        )
        write_text(str(log_path), f"CACHE_DIR: {cache_dir}\n")
        write_text(str(log_path), f"CACHE_KEY: {cache_key}\n")
        write_text(str(log_path), f"CACHE_HIT: {cache_hit}\n")

        rq12_log = rq12_dir / "log.txt"
        write_text(str(rq12_log), f"RUN_DIR: {rq12_dir}\n", append=False)
        write_text(str(rq12_log), f"CACHE_DIR: {cache_dir}\n")
        write_text(str(rq12_log), f"CACHE_KEY: {cache_key}\n")
        write_text(str(rq12_log), f"CACHE_HIT: {cache_hit}\n")

        data_df = prompts_dong.copy()
        data_df = data_df.rename(
            columns={
                "origin_id": "orig",
                "dest_id": "dest",
                "flow_gt": "y_gt",
            }
        )
        data_df["orig"] = data_df["orig"].astype(str)
        data_df["dest"] = data_df["dest"].astype(str)
        data_df["hour"] = pd.to_numeric(data_df["hour"], errors="coerce").astype(int)
        data_df["y_gt"] = pd.to_numeric(data_df["y_gt"], errors="coerce")
        data_df = make_pair_id(data_df)

        split_seed = int(config.get("run", {}).get("seed", 202511))
        splits_df = make_group_split(
            pairs_df=data_df[["pair_id"]],
            seed=split_seed,
            ratios={"train": 0.7, "val": 0.1, "test": 0.2},
            method="hash",
        )
        data_split_df = attach_split(data_df, splits_df)
        validate_split(data_split_df, expected_hours=24, hour_col="hour")
        save_parquet(splits_df, str(rq12_dir / "splits.parquet"))
        write_text(str(rq12_log), "WROTE splits.parquet\n")

        X_all = np.asarray(cached["lasttoken_layer31"], dtype=np.float32)
        if X_all.shape[0] != len(data_split_df):
            raise ValueError(
                "Cached forward row count does not match RQ12 rows: "
                f"{X_all.shape[0]} vs {len(data_split_df)}"
            )

        device = str(config.get("model", {}).get("device", "cpu"))
        train_mask = data_split_df["split"] == "train"
        val_mask = data_split_df["split"] == "val"
        test_mask = data_split_df["split"] == "test"

        X_train = X_all[train_mask.to_numpy()]
        y_train = data_split_df.loc[train_mask, "y_gt"].to_numpy()
        if int(val_mask.sum()) > 0:
            X_val = X_all[val_mask.to_numpy()]
            y_val = data_split_df.loc[val_mask, "y_gt"].to_numpy()
        else:
            X_val = X_train
            y_val = y_train

        head = train_readout(
            X_train=X_train,
            y_train=y_train,
            X_val=X_val,
            y_val=y_val,
            epochs=50,
            lr=1e-3,
            weight_decay=0.0,
            device=device,
        )
        X_test = X_all[test_mask.to_numpy()]
        y_hat_test = predict_readout(head, X_test, device=device)

        pred_test_df = data_split_df.loc[
            test_mask,
            ["query_id", "scale", "y_gt", "split", "orig", "dest", "hour", "dist_km"],
        ].copy()
        pred_test_df["y_hat"] = y_hat_test
        pred_test_df = pred_test_df[
            ["query_id", "scale", "y_gt", "y_hat", "split", "orig", "dest", "hour", "dist_km"]
        ]
        save_parquet(pred_test_df, str(rq12_dir / "predictions.parquet"))

        rq12_metrics = _compute_metrics_json(pred_test_df)
        write_json(str(rq12_dir / "metrics.json"), rq12_metrics)
        write_json(str(run_dir / "config_snapshot.json"), config)

        write_text(str(rq12_log), "WROTE predictions.parquet\n")
        write_text(str(rq12_log), "WROTE metrics.json\n")
        write_text(str(rq12_log), "RQ12 DONE\n")
        write_text(
            str(log_path),
            f"RQ12 DONE: {rq12_dir / 'predictions.parquet'} | {rq12_dir / 'metrics.json'}\n",
        )
    except Exception as e:
        traceback.print_exc()
        print("EXCEPTION:", e)
        raise


if __name__ == "__main__":
    main()
