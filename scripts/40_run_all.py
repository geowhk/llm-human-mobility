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
from mobility_llm.split import attach_split, make_group_split, make_pair_id, validate_split


def _abs_path(path_str: str) -> Path:
    p = Path(path_str)
    if not p.is_absolute():
        return (PROJECT_ROOT / p).resolve()
    return p.resolve()


def _save_layerwise_npz(path: Path, layerwise: dict[str, np.ndarray]) -> None:
    payload = {k: np.asarray(v, dtype=np.float32) for k, v in layerwise.items()}
    np.savez(path, **payload)


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

        config = load_config(args.config)
        run_mode = str(config.get("run", {}).get("mode", "run")).strip() or "run"
        output_root_cfg = str(config.get("run", {}).get("output_root", "results/runs"))

        ts = datetime.now().strftime("%Y%m%d_%H%M%S")

        output_root = Path(output_root_cfg)
        if not output_root.is_absolute():
            output_root = (PROJECT_ROOT / output_root).resolve()
        else:
            output_root = output_root.resolve()
        run_dir = output_root / f"{run_mode}_all_{ts}"

        rq12_dir = run_dir / "rq12"
        rq3_dir = run_dir / "rq3"

        ensure_dir(str(run_dir))
        ensure_dir(str(rq12_dir))
        ensure_dir(str(rq3_dir))

        log_path = run_dir / "log.txt"
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n", append=False)

        gu_path_raw = str(config["data"]["datasets"]["gu"]["path"])
        dong_path_raw = str(config["data"]["datasets"]["dong"]["path"])
        gu_abs = _abs_path(gu_path_raw)
        dong_abs = _abs_path(dong_path_raw)
        input_path = f"{gu_abs};{dong_abs}"

        gu_df = load_parquet(str(gu_abs)).copy()
        dong_df = load_parquet(str(dong_abs)).copy()
        prompts_gu = build_prompts_df(gu_df, "gu", config)
        prompts_dong = build_prompts_df(dong_df, "dong", config)
        df_forward = pd.concat([prompts_gu, prompts_dong], ignore_index=True)

        cache_key = compute_cache_key(input_path, config)
        cache_dir = get_cache_dir(PROJECT_ROOT, cache_key)
        cache_hit = cache_exists(cache_dir)
        cached = ensure_cache(
            project_root=PROJECT_ROOT,
            config=config,
            input_path=input_path,
            df=df_forward,
        )
        write_text(str(log_path), f"CACHE_DIR: {cache_dir}\n")
        write_text(str(log_path), f"CACHE_KEY: {cache_key}\n")
        write_text(str(log_path), f"CACHE_HIT: {cache_hit}\n")

        # RQ1/RQ2
        rq12_log = rq12_dir / "log.txt"
        write_text(str(rq12_log), f"RUN_DIR: {rq12_dir}\n", append=False)
        data_df = df_forward.copy()
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
        write_text(str(rq12_log), "WROTE predictions.parquet\n")
        write_text(str(rq12_log), "WROTE metrics.json\n")
        write_text(
            str(log_path),
            f"RQ12 DONE: {rq12_dir / 'predictions.parquet'} | {rq12_dir / 'metrics.json'}\n",
        )

        # Release large cached dicts not needed for RQ3 (rq3 uses lazy loader from files).
        del cached

        # RQ3 (gu only, same logic as script 30 + step3 module)
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

        probe_rows: list[dict] = []
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

        gt_abs = _abs_path(gu_path_raw)
        run_step3(
            run_dir=rq3_dir,
            gt_path=str(gt_abs),
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
