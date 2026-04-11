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

from mobility_llm.forward_cache import cache_exists, compute_cache_key, ensure_cache, get_cache_dir
from mobility_llm.io import ensure_dir, load_parquet, save_parquet, write_json, write_text
from mobility_llm.prompts import build_prompts_df
from mobility_llm.rq3_step2 import (
    aggregate_nodes_one_layer,
    aggregate_time_one_layer,
    filter_gu_rows,
    layer_keys,
    load_forward_cache,
    make_distance_bins,
    make_hash_split,
    prepare_pair_metadata,
    probe_distance_one_layer,
)
from mobility_llm.rq3_step3 import run_step3


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


def _load_gu_config(path: str) -> dict[str, Any]:
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
        ["data", "datasets", "gu", "path"],
        ["columns", "orig"],
        ["columns", "dest"],
        ["columns", "hour"],
        ["columns", "dist_km"],
        ["columns", "flow_gt"],
        ["prompt", "template"],
        ["model", "model_id"],
        ["rq3"],
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


def _save_layerwise_npz(path: Path, layerwise: dict[str, np.ndarray]) -> None:
    payload = {k: np.asarray(v, dtype=np.float32) for k, v in layerwise.items()}
    np.savez(path, **payload)


def main() -> None:
    try:
        parser = argparse.ArgumentParser()
        parser.add_argument("--config", required=True, help="Path to config YAML")
        args = parser.parse_args()

        config = _load_gu_config(args.config)
        run_mode = str(config.get("run", {}).get("mode", "run")).strip() or "run"
        resolved_run_id = _resolve_run_id(config)
        output_root_cfg = str(config.get("run", {}).get("output_root", "results/runs"))

        ts = datetime.now().strftime("%Y%m%d_%H%M%S")

        output_root = Path(output_root_cfg)
        if not output_root.is_absolute():
            output_root = (PROJECT_ROOT / output_root).resolve()
        else:
            output_root = output_root.resolve()
        run_dir = output_root / f"{resolved_run_id}_rq3_gu_{ts}"

        rq3_dir = run_dir / "rq3"

        ensure_dir(str(run_dir))
        ensure_dir(str(rq3_dir))

        log_path = run_dir / "log.txt"
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n", append=False)
        write_text(str(log_path), f"RUN_ID_RESOLVED: {resolved_run_id}\n")
        if resolved_run_id != run_mode:
            write_text(str(log_path), f"RUN_MODE: {run_mode}\n")

        gu_path_raw = str(config["data"]["datasets"]["gu"]["path"])
        gu_abs = _abs_path(gu_path_raw)
        input_path = str(gu_abs)

        write_text(str(log_path), f"INPUT_GU: {gu_abs}\n")
        chunk_id = _extract_chunk_id(resolved_run_id, gu_abs.stem)
        if chunk_id is not None:
            write_text(str(log_path), f"CHUNK_ID: {chunk_id}\n")

        gu_df = load_parquet(str(gu_abs)).copy()
        prompts_gu = build_prompts_df(gu_df, "gu", config)

        cache_key = compute_cache_key(input_path, config)
        cache_dir = get_cache_dir(PROJECT_ROOT, cache_key)
        cache_hit = cache_exists(cache_dir)
        ensure_cache(
            project_root=PROJECT_ROOT,
            config=config,
            input_path=input_path,
            df=prompts_gu,
        )
        write_text(str(log_path), f"CACHE_DIR: {cache_dir}\n")
        write_text(str(log_path), f"CACHE_KEY: {cache_key}\n")
        write_text(str(log_path), f"CACHE_HIT: {cache_hit}\n")

        rq3_log = rq3_dir / "log.txt"
        write_text(str(rq3_log), f"RUN_DIR: {rq3_dir}\n", append=False)
        write_text(str(rq3_log), f"CACHE_DIR: {cache_dir}\n")
        if not cache_exists(cache_dir):
            raise FileNotFoundError("Forward cache not found before RQ3 execution.")

        rq3_cached = load_forward_cache(cache_dir)
        row_index = rq3_cached.load_row_index()
        row_gu, gu_idx = filter_gu_rows(row_index)
        write_text(str(rq3_log), f"N_ROWS_GU: {len(row_gu)}\n")

        pairs_df, pair_row_indices, nodes_df, pair_orig_node_idx, pair_dest_node_idx = (
            prepare_pair_metadata(row_gu)
        )
        write_text(str(rq3_log), f"N_PAIRS_GU: {len(pairs_df)}\n")
        write_text(str(rq3_log), f"N_NODES_GU: {len(nodes_df)}\n")

        pair_ids = pairs_df["pair_id"]
        dist_series = pairs_df.set_index("pair_id")["dist_km"]
        y_labels, dist_edges = make_distance_bins(dist_series)
        train_mask_rq3, test_mask_rq3 = make_hash_split(pair_ids)
        write_text(str(rq3_log), f"DIST_BIN_EDGES: {dist_edges}\n")

        probe_rows: list[dict[str, Any]] = []
        nodes_out_layerwise: dict[str, np.ndarray] = {}
        nodes_in_layerwise: dict[str, np.ndarray] = {}

        rq3_cached.open()
        try:
            for li, lk in enumerate(layer_keys()):
                last_row = rq3_cached.get_lasttoken_layer(lk)[gu_idx]
                pair_last = aggregate_time_one_layer(pair_row_indices, last_row)
                acc = probe_distance_one_layer(pair_last, y_labels, train_mask_rq3, test_mask_rq3)
                probe_rows.append(
                    {
                        "layer": li,
                        "metric_name": "accuracy",
                        "metric_value": acc,
                        "n_pairs": int(len(pairs_df)),
                    }
                )

                out_row = rq3_cached.get_role_out_layer(lk)[gu_idx]
                pair_out = aggregate_time_one_layer(pair_row_indices, out_row)
                nodes_out_layerwise[lk] = aggregate_nodes_one_layer(
                    pair_out,
                    pair_orig_node_idx,
                    len(nodes_df),
                ).astype(np.float32)

                in_row = rq3_cached.get_role_in_layer(lk)[gu_idx]
                pair_in = aggregate_time_one_layer(pair_row_indices, in_row)
                nodes_in_layerwise[lk] = aggregate_nodes_one_layer(
                    pair_in,
                    pair_dest_node_idx,
                    len(nodes_df),
                ).astype(np.float32)
        finally:
            rq3_cached.close()

        save_parquet(
            pairs_df[["orig", "dest", "pair_id", "dist_km"]],
            str(rq3_dir / "rq3_pairs.parquet"),
        )
        save_parquet(nodes_df[["node_id"]], str(rq3_dir / "rq3_nodes.parquet"))
        save_parquet(pd.DataFrame(probe_rows), str(rq3_dir / "rq3_distance_probe.parquet"))
        write_json(str(rq3_dir / "rq3_distance_probe_meta.json"), {"dist_bin_edges": dist_edges})
        _save_layerwise_npz(rq3_dir / "rq3_nodes_out_layerwise.npz", nodes_out_layerwise)
        _save_layerwise_npz(rq3_dir / "rq3_nodes_in_layerwise.npz", nodes_in_layerwise)
        write_text(str(rq3_log), "WROTE rq3_pairs.parquet\n")
        write_text(str(rq3_log), "WROTE rq3_nodes.parquet\n")
        write_text(str(rq3_log), "WROTE rq3_distance_probe.parquet\n")
        write_text(str(rq3_log), "WROTE rq3_distance_probe_meta.json\n")
        write_text(str(rq3_log), "WROTE rq3_nodes_out_layerwise.npz\n")
        write_text(str(rq3_log), "WROTE rq3_nodes_in_layerwise.npz\n")

        run_step3(
            run_dir=rq3_dir,
            gt_path=str(gu_abs),
            config=config,
            log_path=rq3_log,
        )

        write_json(str(run_dir / "config_snapshot.json"), config)
        write_text(
            str(log_path),
            f"RQ3 DONE: {rq3_dir / 'rq3_role_alignment.parquet'} | "
            f"{rq3_dir / 'rq3_role_alignment_summary.json'}\n",
        )
    except Exception as e:
        traceback.print_exc()
        print("EXCEPTION:", e)
        raise


if __name__ == "__main__":
    main()
