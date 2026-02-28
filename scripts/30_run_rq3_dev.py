from __future__ import annotations

import argparse
import sys
import traceback
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "src"
if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

from mobility_llm.config import load_config
from mobility_llm.forward_cache import cache_exists, compute_cache_key, get_cache_dir
from mobility_llm.io import ensure_dir, save_parquet, write_json, write_text
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


def _abs_data_path(path_str: str) -> Path:
    p = Path(path_str)
    if not p.is_absolute():
        return (PROJECT_ROOT / p).resolve()
    return p.resolve()


def _save_layerwise_npz(path: Path, layerwise: dict[str, np.ndarray]) -> None:
    payload = {k: np.asarray(v, dtype=np.float32) for k, v in layerwise.items()}
    np.savez(path, **payload)


def main() -> None:
    try:
        parser = argparse.ArgumentParser()
        parser.add_argument("--config", required=True, help="Path to config YAML")
        args = parser.parse_args()

        config = load_config(args.config)
        run_mode = str(config.get("run", {}).get("mode", "dev")).strip() or "dev"
        ts = datetime.now().strftime("%Y%m%d_%H%M%S")
        run_dir = PROJECT_ROOT / "results" / "runs" / f"{run_mode}_rq3_dev_{ts}"
        ensure_dir(str(run_dir))

        log_path = run_dir / "log.txt"
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n", append=False)

        gu_abs = _abs_data_path(config["data"]["datasets"]["gu"]["path"])
        dong_abs = _abs_data_path(config["data"]["datasets"]["dong"]["path"])
        input_path = f"{gu_abs};{dong_abs}"
        cache_key = compute_cache_key(input_path, config)
        cache_dir = get_cache_dir(PROJECT_ROOT, cache_key)
        write_text(str(log_path), f"CACHE_DIR: {cache_dir}\n")

        if not cache_exists(cache_dir):
            raise FileNotFoundError(
                "STEP1 forward cache not found. "
                "Run scripts/20_run_readout_dev.py first to build cache."
            )

        cached = load_forward_cache(cache_dir)
        row_index = cached.load_row_index()
        row_gu, gu_idx = filter_gu_rows(row_index)
        write_text(str(log_path), f"N_ROWS_GU: {len(row_gu)}\n")

        pairs_df, pair_row_indices, nodes_df, pair_orig_node_idx, pair_dest_node_idx = (
            prepare_pair_metadata(row_gu)
        )
        write_text(str(log_path), f"N_PAIRS_GU: {len(pairs_df)}\n")
        write_text(str(log_path), f"N_NODES_GU: {len(nodes_df)}\n")

        pair_ids = pairs_df["pair_id"]
        dist_series = pairs_df.set_index("pair_id")["dist_km"]
        y_labels, dist_edges = make_distance_bins(dist_series)
        train_mask, test_mask = make_hash_split(pair_ids)
        write_text(str(log_path), f"DIST_BIN_EDGES: {dist_edges}\n")

        probe_rows: list[dict] = []
        nodes_out_layerwise: dict[str, np.ndarray] = {}
        nodes_in_layerwise: dict[str, np.ndarray] = {}

        cached.open()
        try:
            for li, lk in enumerate(layer_keys()):
                last_row = cached.get_lasttoken_layer(lk)[gu_idx]
                pair_last = aggregate_time_one_layer(pair_row_indices, last_row)
                acc = probe_distance_one_layer(pair_last, y_labels, train_mask, test_mask)
                probe_rows.append(
                    {
                        "layer": li,
                        "metric_name": "accuracy",
                        "metric_value": acc,
                        "n_pairs": int(len(pairs_df)),
                    }
                )

                out_row = cached.get_role_out_layer(lk)[gu_idx]
                pair_out = aggregate_time_one_layer(pair_row_indices, out_row)
                node_out = aggregate_nodes_one_layer(pair_out, pair_orig_node_idx, len(nodes_df))
                nodes_out_layerwise[lk] = np.asarray(node_out, dtype=np.float32)

                in_row = cached.get_role_in_layer(lk)[gu_idx]
                pair_in = aggregate_time_one_layer(pair_row_indices, in_row)
                node_in = aggregate_nodes_one_layer(pair_in, pair_dest_node_idx, len(nodes_df))
                nodes_in_layerwise[lk] = np.asarray(node_in, dtype=np.float32)

                del last_row, pair_last, out_row, pair_out, node_out, in_row, pair_in, node_in
        finally:
            cached.close()

        dist_probe_df = pd.DataFrame(probe_rows)

        save_parquet(
            pairs_df[["orig", "dest", "pair_id", "dist_km"]],
            str(run_dir / "rq3_pairs.parquet"),
        )
        write_text(str(log_path), "WROTE rq3_pairs.parquet\n")
        save_parquet(nodes_df[["node_id"]], str(run_dir / "rq3_nodes.parquet"))
        write_text(str(log_path), "WROTE rq3_nodes.parquet\n")

        save_parquet(dist_probe_df, str(run_dir / "rq3_distance_probe.parquet"))
        write_text(str(log_path), "WROTE rq3_distance_probe.parquet\n")

        write_json(
            str(run_dir / "rq3_distance_probe_meta.json"),
            {"dist_bin_edges": dist_edges},
        )
        write_text(str(log_path), "WROTE rq3_distance_probe_meta.json\n")

        _save_layerwise_npz(run_dir / "rq3_nodes_out_layerwise.npz", nodes_out_layerwise)
        write_text(str(log_path), "WROTE rq3_nodes_out_layerwise.npz\n")

        _save_layerwise_npz(run_dir / "rq3_nodes_in_layerwise.npz", nodes_in_layerwise)
        write_text(str(log_path), "WROTE rq3_nodes_in_layerwise.npz\n")

        write_json(str(run_dir / "config_snapshot.json"), config)
        write_text(str(log_path), "WROTE config_snapshot.json\n")

        gt_path_raw = str(config["data"]["datasets"]["gu"]["path"])
        gt_path_obj = Path(gt_path_raw)
        if not gt_path_obj.is_absolute():
            gt_path_obj = (PROJECT_ROOT / gt_path_obj).resolve()
        else:
            gt_path_obj = gt_path_obj.resolve()

        run_step3(
            run_dir=run_dir,
            gt_path=str(gt_path_obj),
            config=config,
            log_path=log_path,
        )
    except Exception as e:
        traceback.print_exc()
        print("EXCEPTION:", e)
        raise


if __name__ == "__main__":
    main()
