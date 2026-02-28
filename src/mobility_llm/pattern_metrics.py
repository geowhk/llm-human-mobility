from __future__ import annotations

from typing import Optional

import numpy as np
import pandas as pd


def _to_numeric(series: pd.Series) -> pd.Series:
    return pd.to_numeric(series, errors="coerce")


def fit_beta_from_flow(df: pd.DataFrame, flow_col: str) -> Optional[float]:
    """Fit power-law distance decay beta from one flow column on test rows."""
    if "dist_km" not in df.columns or flow_col not in df.columns:
        return None

    x_raw = _to_numeric(df["dist_km"])
    y_raw = _to_numeric(df[flow_col])
    valid = (~x_raw.isna()) & (~y_raw.isna()) & (x_raw > 0)
    x_raw = x_raw[valid]
    y_raw = y_raw[valid]

    if len(x_raw) < 30:
        return None

    x = np.log(x_raw.to_numpy(dtype=float))
    y = np.log1p(y_raw.to_numpy(dtype=float))
    slope, _intercept = np.polyfit(x, y, deg=1)
    return float(-slope)


def delta_beta(df: pd.DataFrame) -> Optional[float]:
    """Absolute gap between GT beta and prediction beta."""
    beta_gt = fit_beta_from_flow(df, "y_gt")
    beta_pred = fit_beta_from_flow(df, "y_hat")
    if beta_gt is None or beta_pred is None:
        return None
    return float(abs(beta_pred - beta_gt))


def origin_marginal_spearman(df: pd.DataFrame) -> Optional[float]:
    """Spearman correlation of origin marginals between GT and predictions."""
    if "orig" not in df.columns or "y_gt" not in df.columns or "y_hat" not in df.columns:
        return None

    work = df[["orig", "y_gt", "y_hat"]].copy()
    work["y_gt"] = _to_numeric(work["y_gt"])
    work["y_hat"] = _to_numeric(work["y_hat"])
    work = work.dropna(subset=["y_gt", "y_hat"])
    if work.empty:
        return None

    by_orig = work.groupby("orig", as_index=False).agg(
        O_gt=("y_gt", "sum"),
        O_pred=("y_hat", "sum"),
    )
    if len(by_orig) < 3:
        return None

    rho = by_orig["O_gt"].corr(by_orig["O_pred"], method="spearman")
    if pd.isna(rho):
        return None
    return float(rho)


def destination_marginal_spearman(df: pd.DataFrame) -> Optional[float]:
    """Spearman correlation of destination marginals between GT and predictions."""
    if "dest" not in df.columns or "y_gt" not in df.columns or "y_hat" not in df.columns:
        return None

    work = df[["dest", "y_gt", "y_hat"]].copy()
    work["y_gt"] = _to_numeric(work["y_gt"])
    work["y_hat"] = _to_numeric(work["y_hat"])
    work = work.dropna(subset=["y_gt", "y_hat"])
    if work.empty:
        return None

    by_dest = work.groupby("dest", as_index=False).agg(
        D_gt=("y_gt", "sum"),
        D_pred=("y_hat", "sum"),
    )
    if len(by_dest) < 3:
        return None

    rho = by_dest["D_gt"].corr(by_dest["D_pred"], method="spearman")
    if pd.isna(rho):
        return None
    return float(rho)


def gini_coefficient(x: np.ndarray) -> float:
    """Compute Gini coefficient from a non-negative 1D vector."""
    if x.size == 0:
        return 0.0
    x = np.asarray(x, dtype=float)
    x = np.where(np.isnan(x), 0.0, x)
    x = np.clip(x, 0.0, None)
    x = np.sort(x)
    n = x.size
    total = float(x.sum())
    if total <= 0.0:
        return 0.0
    i = np.arange(1, n + 1, dtype=float)
    g = (2.0 * np.sum(i * x) / (n * total)) - (n + 1.0) / n
    return float(g)


def delta_gini(df: pd.DataFrame) -> Optional[float]:
    """Absolute gap between GT and prediction Gini concentration."""
    if "y_gt" not in df.columns or "y_hat" not in df.columns:
        return None

    gt = _to_numeric(df["y_gt"])
    pred = _to_numeric(df["y_hat"])
    valid = (~gt.isna()) & (~pred.isna())
    gt = gt[valid].to_numpy(dtype=float)
    pred = pred[valid].to_numpy(dtype=float)
    if gt.size == 0:
        return None

    return float(abs(gini_coefficient(pred) - gini_coefficient(gt)))


def cpc(df: pd.DataFrame) -> Optional[float]:
    """Common Part of Commuters overlap on OD rows."""
    if "y_gt" not in df.columns or "y_hat" not in df.columns:
        return None

    gt = _to_numeric(df["y_gt"])
    pred = _to_numeric(df["y_hat"])
    valid = (~gt.isna()) & (~pred.isna())
    gt = gt[valid].to_numpy(dtype=float)
    pred = pred[valid].to_numpy(dtype=float)
    if gt.size == 0:
        return None

    gt = np.clip(gt, 0.0, None)
    pred = np.clip(pred, 0.0, None)
    eps = 1e-9
    return float(2.0 * np.minimum(gt, pred).sum() / (gt.sum() + pred.sum() + eps))
