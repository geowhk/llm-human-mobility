from __future__ import annotations

import argparse
import sys
import traceback
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "src"
if str(SRC_DIR) not in sys.path:
    sys.path.insert(0, str(SRC_DIR))

from mobility_llm.config import load_config
from mobility_llm.eval_metrics import compute_basic_metrics
from mobility_llm.forward_cache import (
    cache_exists,
    compute_cache_key,
    ensure_cache,
    get_cache_dir,
)
from mobility_llm.io import (
    load_parquet,
    make_run_dir,
    save_parquet,
    write_json,
    write_text,
)
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


def _compute_metrics_json(pred_df: pd.DataFrame) -> dict:
    return {
        "all": _compute_eval_block(pred_df),
        "gu": _compute_eval_block(pred_df[pred_df["scale"] == "gu"]),
        "dong": _compute_eval_block(pred_df[pred_df["scale"] == "dong"]),
    }


def _compute_eval_block(df: pd.DataFrame) -> dict:
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


def main() -> None:
    try:
        parser = argparse.ArgumentParser()
        parser.add_argument("--config", required=True, help="Path to config YAML")
        args = parser.parse_args()

        config = load_config(args.config)
        config_path = Path(args.config).resolve()

        run_cfg = config.get("run", {})
        output_root = Path(str(run_cfg.get("output_root", "results/runs")))
        if not output_root.is_absolute():
            output_root = PROJECT_ROOT / output_root

        run_mode = str(run_cfg.get("mode", "dev")).strip() or "dev"
        run_id_base = f"{run_mode}_readout"
        run_dir, _ = make_run_dir(str(output_root), run_id_base)
        run_dir_path = Path(run_dir)

        log_path = run_dir_path / "log.txt"
        write_text(str(log_path), f"CONFIG LOADED: {config_path}\n", append=False)
        write_text(str(log_path), f"RUN_DIR: {run_dir}\n")

        gu_path = config["data"]["datasets"]["gu"]["path"]
        dong_path = config["data"]["datasets"]["dong"]["path"]
        gu_df = load_parquet(gu_path).copy()
        dong_df = load_parquet(dong_path).copy()

        prompts_gu = build_prompts_df(gu_df, "gu", config)
        prompts_dong = build_prompts_df(dong_df, "dong", config)
        df_forward = pd.concat([prompts_gu, prompts_dong], ignore_index=True)
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

        splits_df = make_group_split(
            pairs_df=data_df[["pair_id"]],
            seed=int(run_cfg.get("seed", 202511)),
            ratios={"train": 0.7, "val": 0.1, "test": 0.2},
            method="hash",
        )
        data_split_df = attach_split(data_df, splits_df)
        validate_split(data_split_df, expected_hours=24, hour_col="hour")

        save_parquet(splits_df, str(run_dir_path / "splits.parquet"))
        pair_counts = splits_df["split"].value_counts().to_dict()
        row_counts = data_split_df["split"].value_counts().to_dict()
        write_text(
            str(log_path),
            "SPLIT STATS: "
            f"pairs_total={len(splits_df)} "
            f"pairs_train={pair_counts.get('train', 0)} "
            f"pairs_val={pair_counts.get('val', 0)} "
            f"pairs_test={pair_counts.get('test', 0)} "
            f"rows_train={row_counts.get('train', 0)} "
            f"rows_val={row_counts.get('val', 0)} "
            f"rows_test={row_counts.get('test', 0)}\n",
        )

        gu_abs = Path(gu_path)
        if not gu_abs.is_absolute():
            gu_abs = (PROJECT_ROOT / gu_abs).resolve()
        else:
            gu_abs = gu_abs.resolve()
        dong_abs = Path(dong_path)
        if not dong_abs.is_absolute():
            dong_abs = (PROJECT_ROOT / dong_abs).resolve()
        else:
            dong_abs = dong_abs.resolve()

        input_path = f"{gu_abs};{dong_abs}"
        cache_key = compute_cache_key(input_path, config)
        cache_dir = get_cache_dir(PROJECT_ROOT, cache_key)
        cache_hit = cache_exists(cache_dir)
        cached = ensure_cache(
            project_root=PROJECT_ROOT,
            config=config,
            input_path=input_path,
            df=df_forward,
        )
        write_text(str(log_path), f"FORWARD CACHE KEY: {cache_key}\n")
        write_text(str(log_path), f"FORWARD CACHE DIR: {cache_dir}\n")
        write_text(str(log_path), f"FORWARD CACHE HIT: {cache_hit}\n")

        X_all = np.asarray(cached["lasttoken_layer31"], dtype=np.float32)
        if X_all.shape[0] != len(data_split_df):
            raise ValueError(
                "Cached forward row count does not match data rows: "
                f"{X_all.shape[0]} vs {len(data_split_df)}"
            )

        device = str(config["model"].get("device", "cpu"))

        train_mask = data_split_df["split"] == "train"
        val_mask = data_split_df["split"] == "val"
        test_mask = data_split_df["split"] == "test"

        X_train = X_all[train_mask.to_numpy()]
        y_train = data_split_df.loc[train_mask, "y_gt"].to_numpy()
        if val_mask.sum() > 0:
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
        history = getattr(head, "train_history", {})
        write_text(
            str(log_path),
            "TRAIN DONE: "
            f"final_train_loss={history.get('final_train_loss')} "
            f"final_val_loss={history.get('final_val_loss')}\n",
        )

        X_test = X_all[test_mask.to_numpy()]
        y_hat_test = predict_readout(head, X_test, device=device)
        pred_test_df = data_split_df.loc[test_mask, [
            "query_id",
            "scale",
            "y_gt",
            "split",
            "orig",
            "dest",
            "hour",
            "dist_km",
        ]].copy()
        pred_test_df["y_hat"] = y_hat_test
        pred_test_df = pred_test_df[
            ["query_id", "scale", "y_gt", "y_hat", "split", "orig", "dest", "hour", "dist_km"]
        ]

        save_parquet(pred_test_df, str(run_dir_path / "predictions.parquet"))
        metrics = _compute_metrics_json(pred_test_df)
        write_json(str(run_dir_path / "metrics.json"), metrics)
        write_json(str(run_dir_path / "config_snapshot.json"), config)

        write_text(str(log_path), "WROTE predictions.parquet\n")
        write_text(str(log_path), "WROTE metrics.json\n")
        for scale in ["all", "gu", "dong"]:
            m = metrics.get(scale, {})
            acc = m.get("accuracy", {})
            pat = m.get("patterns", {})
            write_text(
                str(log_path),
                f"METRICS_ACCURACY({scale}): "
                f"n_eval={m.get('n_eval')} "
                f"mae={acc.get('mae')} "
                f"rmse={acc.get('rmse')} "
                f"smape={acc.get('smape')}\n",
            )
            write_text(
                str(log_path),
                f"METRICS_PATTERNS({scale}): "
                f"delta_beta={pat.get('delta_beta')} "
                f"rho_origin={pat.get('rho_origin')} "
                f"rho_destination={pat.get('rho_destination')} "
                f"delta_gini={pat.get('delta_gini')} "
                f"cpc={pat.get('cpc')}\n",
            )
    except Exception as e:
        traceback.print_exc()
        print("EXCEPTION:", e)
        raise


if __name__ == "__main__":
    main()
