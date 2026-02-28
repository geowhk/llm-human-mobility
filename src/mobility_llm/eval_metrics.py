from __future__ import annotations

from typing import Any

import pandas as pd


def aggregate_predictions(
    parsed_df: pd.DataFrame,
    method: str = "median",
) -> pd.DataFrame:
    """Aggregate repeat predictions to query-level predictions."""
    if method not in {"median", "mean"}:
        raise ValueError("aggregate method must be one of: median, mean")

    total_counts = (
        parsed_df.groupby(["query_id", "scale"], as_index=False)
        .size()
        .rename(columns={"size": "n_repeat_total"})
    )

    ok_df = parsed_df[parsed_df["parse_ok"] == True].copy()  # noqa: E712
    ok_counts = (
        ok_df.groupby(["query_id", "scale"], as_index=False)
        .size()
        .rename(columns={"size": "n_repeat_ok"})
    )

    if method == "median":
        agg = (
            ok_df.groupby(["query_id", "scale"], as_index=False)["y_hat_int"]
            .median()
            .rename(columns={"y_hat_int": "y_hat"})
        )
    else:
        agg = (
            ok_df.groupby(["query_id", "scale"], as_index=False)["y_hat_int"]
            .mean()
            .rename(columns={"y_hat_int": "y_hat"})
        )

    out = total_counts.merge(ok_counts, on=["query_id", "scale"], how="left")
    out = out.merge(agg, on=["query_id", "scale"], how="left")
    out["n_repeat_ok"] = out["n_repeat_ok"].fillna(0).astype(int)
    out["ok_rate"] = out["n_repeat_ok"] / out["n_repeat_total"]
    out["y_hat"] = out["y_hat"].astype(float)
    return out[
        ["query_id", "scale", "y_hat", "n_repeat_total", "n_repeat_ok", "ok_rate"]
    ]


def compute_repeat_stability(parsed_df: pd.DataFrame) -> pd.DataFrame:
    """Compute query-level repeat stability statistics from parse_ok rows."""
    ok_df = parsed_df[parsed_df["parse_ok"] == True].copy()  # noqa: E712
    grouped = ok_df.groupby(["query_id", "scale"])["y_hat_int"]
    stability = grouped.agg(
        n_repeat_ok="count",
        pred_mean="mean",
        pred_median="median",
        pred_sd="std",
        pred_min="min",
        pred_max="max",
    ).reset_index()

    stability["pred_iqr"] = grouped.quantile(0.75).reset_index(drop=True) - grouped.quantile(
        0.25
    ).reset_index(drop=True)
    stability["pred_range"] = stability["pred_max"] - stability["pred_min"]

    stability.loc[stability["n_repeat_ok"] < 2, ["pred_sd", "pred_iqr"]] = float("nan")
    return stability[
        [
            "query_id",
            "scale",
            "n_repeat_ok",
            "pred_mean",
            "pred_median",
            "pred_sd",
            "pred_iqr",
            "pred_min",
            "pred_max",
            "pred_range",
        ]
    ]


def compute_basic_metrics(gt: pd.Series, pred: pd.Series) -> dict[str, Any]:
    """Compute MAE, RMSE, sMAPE, CPC and sums."""
    eps = 1e-9
    gt_num = pd.to_numeric(gt, errors="coerce")
    pred_num = pd.to_numeric(pred, errors="coerce")
    valid = (~gt_num.isna()) & (~pred_num.isna())
    gt_num = gt_num[valid]
    pred_num = pred_num[valid]

    n = len(gt_num)
    if n == 0:
        return {
            "mae": None,
            "rmse": None,
            "smape": None,
            "cpc": None,
            "y_sum": 0.0,
            "yhat_sum": 0.0,
        }

    abs_err = (pred_num - gt_num).abs()
    mae = float(abs_err.mean())
    rmse = float(((pred_num - gt_num) ** 2).mean() ** 0.5)
    smape = float((2.0 * abs_err / (pred_num.abs() + gt_num.abs() + eps)).mean())
    cpc = float(
        2.0 * pd.concat([pred_num, gt_num], axis=1).min(axis=1).sum()
        / (pred_num.sum() + gt_num.sum() + eps)
    )

    return {
        "mae": mae,
        "rmse": rmse,
        "smape": smape,
        "cpc": cpc,
        "y_sum": float(gt_num.sum()),
        "yhat_sum": float(pred_num.sum()),
    }


def compute_metrics_by_scale(
    prompts_df: pd.DataFrame,
    pred_agg_df: pd.DataFrame,
) -> dict[str, Any]:
    """Compute metrics by scale and for all rows."""
    gt_df = prompts_df[["query_id", "scale", "flow_gt"]].copy()
    pred_df = pred_agg_df[["query_id", "scale", "y_hat"]].copy()
    eval_df = gt_df.merge(pred_df, on=["query_id", "scale"], how="inner")

    out: dict[str, Any] = {}
    scales = ["gu", "dong", "rq3"]
    for scale in scales:
        gt_s = gt_df[gt_df["scale"] == scale]
        eval_s = eval_df[eval_df["scale"] == scale]
        n_total = int(len(gt_s))
        n_eval = int(len(eval_s))
        coverage = float(n_eval / n_total) if n_total > 0 else 0.0
        metrics = compute_basic_metrics(eval_s["flow_gt"], eval_s["y_hat"])
        out[scale] = {
            "n_total_gt": n_total,
            "n_eval": n_eval,
            "coverage_eval": coverage,
            **metrics,
        }

    n_total_all = int(len(gt_df))
    n_eval_all = int(len(eval_df))
    coverage_all = float(n_eval_all / n_total_all) if n_total_all > 0 else 0.0
    metrics_all = compute_basic_metrics(eval_df["flow_gt"], eval_df["y_hat"])
    out["all"] = {
        "n_total_gt": n_total_all,
        "n_eval": n_eval_all,
        "coverage_eval": coverage_all,
        **metrics_all,
    }
    return out
