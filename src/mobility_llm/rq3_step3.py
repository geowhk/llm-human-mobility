from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd


def _layer_keys() -> list[str]:
    return [f"layer_{i}" for i in range(32)]


def _spearman_corr(x: np.ndarray, y: np.ndarray, min_n: int) -> float:
    x_s = pd.to_numeric(pd.Series(x), errors="coerce")
    y_s = pd.to_numeric(pd.Series(y), errors="coerce")
    valid = (~x_s.isna()) & (~y_s.isna())
    if int(valid.sum()) < min_n:
        return float("nan")
    rho = x_s[valid].corr(y_s[valid], method="spearman")
    return float(rho) if pd.notna(rho) else float("nan")


def load_step2_outputs(run_dir: Path):
    """Load STEP2 outputs and validate layer keys."""
    run_dir = Path(run_dir).resolve()
    pairs_path = run_dir / "rq3_pairs.parquet"
    nodes_path = run_dir / "rq3_nodes.parquet"
    out_path = run_dir / "rq3_nodes_out_layerwise.npz"
    in_path = run_dir / "rq3_nodes_in_layerwise.npz"

    pairs_df = pd.read_parquet(pairs_path)
    nodes_df = pd.read_parquet(nodes_path)
    nodes_out_npz = np.load(out_path, allow_pickle=False)
    nodes_in_npz = np.load(in_path, allow_pickle=False)

    expected = set(_layer_keys())
    if not expected.issubset(set(nodes_out_npz.files)):
        nodes_out_npz.close()
        nodes_in_npz.close()
        raise ValueError("Missing layer keys in rq3_nodes_out_layerwise.npz")
    if not expected.issubset(set(nodes_in_npz.files)):
        nodes_out_npz.close()
        nodes_in_npz.close()
        raise ValueError("Missing layer keys in rq3_nodes_in_layerwise.npz")

    return pairs_df, nodes_df, nodes_out_npz, nodes_in_npz


def compute_role_separation(nodes_out: np.ndarray, nodes_in: np.ndarray) -> tuple[float, float]:
    """Compute median and IQR of node-wise cosine(out_i, in_i)."""
    eps = 1e-9
    out = np.asarray(nodes_out, dtype=np.float64)
    inn = np.asarray(nodes_in, dtype=np.float64)
    dot = np.sum(out * inn, axis=1)
    denom = np.linalg.norm(out, axis=1) * np.linalg.norm(inn, axis=1) + eps
    cos = dot / denom
    cos_s = pd.to_numeric(pd.Series(cos), errors="coerce")
    cos_s = cos_s[~cos_s.isna()]
    if len(cos_s) == 0:
        return float("nan"), float("nan")
    median_cos = float(cos_s.median())
    q1 = float(cos_s.quantile(0.25))
    q3 = float(cos_s.quantile(0.75))
    return median_cos, (q3 - q1)


def compute_role_flow_targets_from_gt(
    gt_df: pd.DataFrame,
    orig_col: str,
    dest_col: str,
    flow_col: str,
) -> tuple[pd.Series, pd.Series]:
    """Compute log1p outflow/inflow targets per node from GT gu OD-hour data."""
    df = gt_df.copy()
    df[orig_col] = df[orig_col].astype(str)
    df[dest_col] = df[dest_col].astype(str)
    flow = pd.to_numeric(df[flow_col], errors="coerce").fillna(0.0)
    flow = flow.clip(lower=0.0)
    df["_flow"] = flow

    out = df.groupby(orig_col, as_index=True)["_flow"].sum()
    inn = df.groupby(dest_col, as_index=True)["_flow"].sum()
    o_series = np.log1p(out).astype(float)
    d_series = np.log1p(inn).astype(float)
    o_series.index = o_series.index.astype(str)
    d_series.index = d_series.index.astype(str)
    return o_series, d_series


def compute_role_flow_association(
    nodes_out: np.ndarray,
    nodes_in: np.ndarray,
    node_ids: list[str],
    o_series: pd.Series,
    d_series: pd.Series,
) -> tuple[float, float]:
    """Compute Spearman(norm(H_out), outflow target) and Spearman(norm(H_in), inflow target)."""
    out_norm = np.linalg.norm(np.asarray(nodes_out, dtype=np.float64), axis=1)
    in_norm = np.linalg.norm(np.asarray(nodes_in, dtype=np.float64), axis=1)

    ids = pd.Series([str(x) for x in node_ids], dtype="string")
    out_target = ids.map(o_series)
    in_target = ids.map(d_series)

    rho_out = _spearman_corr(out_norm, out_target.to_numpy(), min_n=3)
    rho_in = _spearman_corr(in_norm, in_target.to_numpy(), min_n=3)
    return rho_out, rho_in


def compute_alignment(
    pairs_df: pd.DataFrame,
    node_ids: list[str],
    nodes_out: np.ndarray,
    nodes_in: np.ndarray,
) -> tuple[float, float]:
    """Compute Spearman between representation distance and geographic distance."""
    eps = 1e-9
    node_to_idx = {str(n): i for i, n in enumerate(node_ids)}

    df = pairs_df.copy()
    df["orig"] = df["orig"].astype(str)
    df["dest"] = df["dest"].astype(str)
    df["dist_km"] = pd.to_numeric(df["dist_km"], errors="coerce")
    df = df.dropna(subset=["dist_km"])
    if len(df) < 10:
        return float("nan"), float("nan")

    o_idx = df["orig"].map(node_to_idx)
    d_idx = df["dest"].map(node_to_idx)
    valid = (~o_idx.isna()) & (~d_idx.isna())
    df = df.loc[valid].copy()
    if len(df) < 10:
        return float("nan"), float("nan")

    oi = o_idx.loc[valid].to_numpy(dtype=int)
    di = d_idx.loc[valid].to_numpy(dtype=int)

    out = np.asarray(nodes_out, dtype=np.float64)
    inn = np.asarray(nodes_in, dtype=np.float64)
    geo = df["dist_km"].to_numpy(dtype=float)

    out_a = out[oi]
    out_b = out[di]
    in_a = inn[oi]
    in_b = inn[di]

    cos_out = np.sum(out_a * out_b, axis=1) / (
        np.linalg.norm(out_a, axis=1) * np.linalg.norm(out_b, axis=1) + eps
    )
    cos_in = np.sum(in_a * in_b, axis=1) / (
        np.linalg.norm(in_a, axis=1) * np.linalg.norm(in_b, axis=1) + eps
    )
    repr_dist_out = 1.0 - cos_out
    repr_dist_in = 1.0 - cos_in

    rho_out = _spearman_corr(repr_dist_out, geo, min_n=10)
    rho_in = _spearman_corr(repr_dist_in, geo, min_n=10)
    return rho_out, rho_in


def _best_layer(df: pd.DataFrame, metric_col: str, mode: str) -> int | None:
    s = pd.to_numeric(df[metric_col], errors="coerce")
    valid = ~s.isna()
    if int(valid.sum()) == 0:
        return None
    idx = s[valid].idxmin() if mode == "min" else s[valid].idxmax()
    return int(df.loc[idx, "layer"])


def run_step3(
    run_dir: Path,
    gt_path: str,
    config: dict,
    log_path: Path | None = None,
) -> None:
    """Run STEP3 role/alignment metrics for layers 0..31 and write outputs."""
    run_dir = Path(run_dir).resolve()
    if log_path is None:
        log_path = run_dir / "log_step3.txt"

    def log(msg: str, append: bool = True) -> None:
        mode = "a" if append else "w"
        with log_path.open(mode, encoding="utf-8") as f:
            f.write(msg + "\n")

    log(f"RUN_DIR: {run_dir}", append=True)

    pairs_df, nodes_df, nodes_out_npz, nodes_in_npz = load_step2_outputs(run_dir)
    log("LOADED rq3_nodes.parquet")
    log("LOADED rq3_pairs.parquet")
    log("LOADED rq3_nodes_out_layerwise.npz / rq3_nodes_in_layerwise.npz")

    gt_df = pd.read_parquet(Path(gt_path))
    log(f"LOADED GT gu dataset path: {Path(gt_path).resolve()}")

    cols = config.get("columns", {})
    orig_col = str(cols.get("origin_id", cols.get("orig", "orig")))
    dest_col = str(cols.get("dest_id", cols.get("dest", "dest")))
    flow_col = str(cols.get("flow_gt", "flow"))
    o_series, d_series = compute_role_flow_targets_from_gt(
        gt_df=gt_df,
        orig_col=orig_col,
        dest_col=dest_col,
        flow_col=flow_col,
    )

    node_ids = nodes_df["node_id"].astype(str).tolist()
    n_nodes = len(node_ids)
    log(f"N_NODES: {n_nodes}")

    rows: list[dict[str, Any]] = []
    for li, lk in enumerate(_layer_keys()):
        nodes_out = np.asarray(nodes_out_npz[lk], dtype=np.float32)
        nodes_in = np.asarray(nodes_in_npz[lk], dtype=np.float32)

        role_sep_median_cos, role_sep_iqr_cos = compute_role_separation(nodes_out, nodes_in)
        rho_assoc_out, rho_assoc_in = compute_role_flow_association(
            nodes_out=nodes_out,
            nodes_in=nodes_in,
            node_ids=node_ids,
            o_series=o_series,
            d_series=d_series,
        )
        rho_align_out, rho_align_in = compute_alignment(
            pairs_df=pairs_df,
            node_ids=node_ids,
            nodes_out=nodes_out,
            nodes_in=nodes_in,
        )

        rows.append(
            {
                "layer": li,
                "role_sep_median_cos": role_sep_median_cos,
                "role_sep_iqr_cos": role_sep_iqr_cos,
                "role_assoc_out_spearman": rho_assoc_out,
                "role_assoc_in_spearman": rho_assoc_in,
                "align_out_spearman": rho_align_out,
                "align_in_spearman": rho_align_in,
                "n_nodes": int(n_nodes),
            }
        )

    nodes_out_npz.close()
    nodes_in_npz.close()

    result_df = pd.DataFrame(rows).sort_values("layer").reset_index(drop=True)
    result_path = run_dir / "rq3_role_alignment.parquet"
    result_df.to_parquet(result_path, index=False)
    log("WROTE rq3_role_alignment.parquet")

    summary = {
        "n_layers": 32,
        "n_nodes": int(n_nodes),
        "best_layer_by_role_sep": _best_layer(result_df, "role_sep_median_cos", "min"),
        "best_layer_by_align_out": _best_layer(result_df, "align_out_spearman", "max"),
        "best_layer_by_align_in": _best_layer(result_df, "align_in_spearman", "max"),
        "best_layer_by_role_assoc_out": _best_layer(result_df, "role_assoc_out_spearman", "max"),
        "best_layer_by_role_assoc_in": _best_layer(result_df, "role_assoc_in_spearman", "max"),
    }
    summary_path = run_dir / "rq3_role_alignment_summary.json"
    with summary_path.open("w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)
    log("WROTE rq3_role_alignment_summary.json")
