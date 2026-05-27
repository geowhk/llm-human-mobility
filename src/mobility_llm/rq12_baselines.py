from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd

from mobility_llm.split import make_pair_id


BASELINE_NAMES = [
    "hourly_mean",
    "od_marginal_product",
    "loglinear_gravity",
]


def prepare_canonical_rq12_df(prompts_df: pd.DataFrame) -> pd.DataFrame:
    df = prompts_df.copy()
    df = df.rename(
        columns={
            "origin_id": "orig",
            "dest_id": "dest",
            "flow_gt": "y_gt",
        }
    )
    df["orig"] = df["orig"].astype(str)
    df["dest"] = df["dest"].astype(str)
    df["hour"] = pd.to_numeric(df["hour"], errors="coerce").astype(int)
    df["y_gt"] = pd.to_numeric(df["y_gt"], errors="coerce")
    df["dist_km"] = pd.to_numeric(df["dist_km"], errors="coerce")
    return make_pair_id(df)


def _fit_hourly_mean_table(train_df: pd.DataFrame) -> tuple[pd.DataFrame, float]:
    hourly = (
        train_df.groupby("hour", as_index=False)["y_gt"]
        .mean()
        .rename(columns={"y_gt": "hourly_mean"})
    )
    global_mean = float(pd.to_numeric(train_df["y_gt"], errors="coerce").fillna(0.0).mean())
    return hourly, global_mean


def fit_hourly_mean(train_df: pd.DataFrame) -> dict[str, Any]:
    hourly, global_mean = _fit_hourly_mean_table(train_df)
    return {
        "hourly_mean_table": hourly,
        "global_mean": global_mean,
    }


def predict_hourly_mean(model: dict[str, Any], test_df: pd.DataFrame) -> np.ndarray:
    merged = test_df[["hour"]].merge(model["hourly_mean_table"], on="hour", how="left")
    preds = merged["hourly_mean"].fillna(float(model["global_mean"]))
    return np.clip(preds.to_numpy(dtype=float), 0.0, None)


def compute_hourly_marginals(train_df: pd.DataFrame) -> dict[str, Any]:
    origin_hour = (
        train_df.groupby(["orig", "hour"], as_index=False)["y_gt"]
        .sum()
        .rename(columns={"y_gt": "orig_hour_total"})
    )
    dest_hour = (
        train_df.groupby(["dest", "hour"], as_index=False)["y_gt"]
        .sum()
        .rename(columns={"y_gt": "dest_hour_total"})
    )
    hour_total = (
        train_df.groupby("hour", as_index=False)["y_gt"]
        .sum()
        .rename(columns={"y_gt": "hour_total"})
    )
    hourly_mean_table, global_mean = _fit_hourly_mean_table(train_df)
    return {
        "origin_hour": origin_hour,
        "dest_hour": dest_hour,
        "hour_total": hour_total,
        "hourly_mean_table": hourly_mean_table,
        "global_mean": global_mean,
    }


def fit_od_marginal_product(train_df: pd.DataFrame) -> dict[str, Any]:
    return compute_hourly_marginals(train_df)


def predict_od_marginal_product(model: dict[str, Any], test_df: pd.DataFrame) -> np.ndarray:
    merged = test_df[["orig", "dest", "hour"]].copy()
    merged = merged.merge(model["origin_hour"], on=["orig", "hour"], how="left")
    merged = merged.merge(model["dest_hour"], on=["dest", "hour"], how="left")
    merged = merged.merge(model["hour_total"], on="hour", how="left")
    merged = merged.merge(model["hourly_mean_table"], on="hour", how="left")

    fallback = merged["hourly_mean"].fillna(float(model["global_mean"])).to_numpy(dtype=float)
    O = merged["orig_hour_total"].to_numpy(dtype=float)
    D = merged["dest_hour_total"].to_numpy(dtype=float)
    T = merged["hour_total"].to_numpy(dtype=float)

    valid = (~np.isnan(O)) & (~np.isnan(D)) & (~np.isnan(T)) & (T > 0.0)
    preds = np.where(valid, (O * D) / T, fallback)
    return np.clip(preds.astype(float, copy=False), 0.0, None)


def fit_loglinear_gravity(train_df: pd.DataFrame) -> dict[str, Any]:
    marginals = compute_hourly_marginals(train_df)
    train_features = train_df[["orig", "dest", "hour", "dist_km", "y_gt"]].copy()
    train_features = train_features.merge(marginals["origin_hour"], on=["orig", "hour"], how="left")
    train_features = train_features.merge(marginals["dest_hour"], on=["dest", "hour"], how="left")

    O = pd.to_numeric(train_features["orig_hour_total"], errors="coerce").fillna(0.0).to_numpy(dtype=float)
    D = pd.to_numeric(train_features["dest_hour_total"], errors="coerce").fillna(0.0).to_numpy(dtype=float)
    dist = (
        pd.to_numeric(train_features["dist_km"], errors="coerce")
        .fillna(0.0)
        .clip(lower=0.0)
        .to_numpy(dtype=float)
    )
    y = pd.to_numeric(train_features["y_gt"], errors="coerce").fillna(0.0).clip(lower=0.0).to_numpy(dtype=float)

    X = np.column_stack([
        np.ones(len(train_features), dtype=float),
        np.log1p(O),
        np.log1p(D),
        np.log1p(dist),
    ])
    y_log = np.log1p(y)
    coef, _, _, _ = np.linalg.lstsq(X, y_log, rcond=None)

    return {
        "origin_hour": marginals["origin_hour"],
        "dest_hour": marginals["dest_hour"],
        "coef": coef.astype(float, copy=False),
    }


def predict_loglinear_gravity(model: dict[str, Any], test_df: pd.DataFrame) -> np.ndarray:
    test_features = test_df[["orig", "dest", "hour", "dist_km"]].copy()
    test_features = test_features.merge(model["origin_hour"], on=["orig", "hour"], how="left")
    test_features = test_features.merge(model["dest_hour"], on=["dest", "hour"], how="left")

    O = pd.to_numeric(test_features["orig_hour_total"], errors="coerce").fillna(0.0).to_numpy(dtype=float)
    D = pd.to_numeric(test_features["dest_hour_total"], errors="coerce").fillna(0.0).to_numpy(dtype=float)
    dist = (
        pd.to_numeric(test_features["dist_km"], errors="coerce")
        .fillna(0.0)
        .clip(lower=0.0)
        .to_numpy(dtype=float)
    )

    X = np.column_stack([
        np.ones(len(test_features), dtype=float),
        np.log1p(O),
        np.log1p(D),
        np.log1p(dist),
    ])
    pred_log = X @ np.asarray(model["coef"], dtype=float)
    preds = np.expm1(pred_log)
    return np.clip(preds.astype(float, copy=False), 0.0, None)
