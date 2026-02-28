from __future__ import annotations

import hashlib
from typing import Any

import pandas as pd


def make_pair_id(df: pd.DataFrame) -> pd.DataFrame:
    """Add directed OD pair_id as 'orig|dest'."""
    out = df.copy()
    out["pair_id"] = out["orig"].astype(str) + "|" + out["dest"].astype(str)
    return out


def _hash_to_unit_interval(text: str) -> float:
    digest = hashlib.sha1(text.encode("utf-8")).hexdigest()
    value = int(digest[:12], 16)
    return value / float(16**12 - 1)


def make_group_split(
    pairs_df: pd.DataFrame,
    seed: int,
    ratios: dict[str, float] | None = None,
    method: str = "hash",
) -> pd.DataFrame:
    """Create deterministic group split by pair_id."""
    if method != "hash":
        raise ValueError("Only method='hash' is supported.")

    if ratios is None:
        ratios = {"train": 0.7, "val": 0.1, "test": 0.2}
    required = {"train", "val", "test"}
    if set(ratios.keys()) != required:
        raise ValueError("ratios must have exactly train/val/test keys.")

    r_train = float(ratios["train"])
    r_val = float(ratios["val"])
    r_test = float(ratios["test"])
    if abs((r_train + r_val + r_test) - 1.0) > 1e-9:
        raise ValueError("ratios must sum to 1.0.")

    unique_pairs = (
        pairs_df[["pair_id"]]
        .drop_duplicates()
        .copy()
        .reset_index(drop=True)
    )
    salted = unique_pairs["pair_id"].astype(str) + f"|{int(seed)}"
    u = salted.map(_hash_to_unit_interval)

    train_cut = r_train
    val_cut = r_train + r_val

    split = pd.Series(index=unique_pairs.index, dtype="object")
    split[u < train_cut] = "train"
    split[(u >= train_cut) & (u < val_cut)] = "val"
    split[u >= val_cut] = "test"

    unique_pairs["split"] = split.values
    return unique_pairs[["pair_id", "split"]]


def attach_split(df: pd.DataFrame, splits_df: pd.DataFrame) -> pd.DataFrame:
    """Join split labels by pair_id."""
    out = df.merge(splits_df, on="pair_id", how="left")
    if out["split"].isna().any():
        raise ValueError("Some rows do not have split labels after attach_split.")
    return out


def validate_split(
    df_with_split: pd.DataFrame,
    expected_hours: int = 24,
    hour_col: str = "hour",
) -> None:
    """Validate split consistency and hour coverage per pair."""
    if hour_col not in df_with_split.columns:
        raise ValueError(f"Missing required hour column for split validation: {hour_col}")

    pair_split_n = (
        df_with_split.groupby("pair_id", as_index=False)["split"]
        .nunique()
        .rename(columns={"split": "n_split"})
    )
    if (pair_split_n["n_split"] > 1).any():
        raise ValueError("A pair_id is assigned to multiple splits.")

    hour_n = (
        df_with_split.groupby("pair_id", as_index=False)[hour_col]
        .nunique()
        .rename(columns={hour_col: "n_hour"})
    )
    bad = hour_n[hour_n["n_hour"] != expected_hours]
    if not bad.empty:
        raise ValueError(
            f"Some pair_id do not have exactly {expected_hours} unique hours."
        )
