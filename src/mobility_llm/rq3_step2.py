from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score

def layer_keys() -> list[str]:
    return [f"layer_{i}" for i in range(32)]


class LazyForwardCache:
    """Lazy loader for STEP1 cache files using mmap layer access."""

    def __init__(self, cache_dir: Path):
        self.cache_dir = Path(cache_dir).resolve()
        self.row_index_path = self.cache_dir / "forward_row_index_gu.parquet"
        self.lasttoken_path = self.cache_dir / "forward_lasttoken_layerwise_gu.npz"
        self.role_out_path = self.cache_dir / "forward_role_out_layerwise_gu.npz"
        self.role_in_path = self.cache_dir / "forward_role_in_layerwise_gu.npz"

        self._lasttoken_npz = None
        self._role_out_npz = None
        self._role_in_npz = None

    def load_row_index(self) -> pd.DataFrame:
        return pd.read_parquet(self.row_index_path)

    def open(self) -> None:
        self._lasttoken_npz = np.load(self.lasttoken_path, mmap_mode="r")
        self._role_out_npz = np.load(self.role_out_path, mmap_mode="r")
        self._role_in_npz = np.load(self.role_in_path, mmap_mode="r")
        expected = set(layer_keys())
        if not expected.issubset(set(self._lasttoken_npz.files)):
            raise ValueError("Missing layer keys in forward_lasttoken_layerwise_gu.npz")
        if not expected.issubset(set(self._role_out_npz.files)):
            raise ValueError("Missing layer keys in forward_role_out_layerwise_gu.npz")
        if not expected.issubset(set(self._role_in_npz.files)):
            raise ValueError("Missing layer keys in forward_role_in_layerwise_gu.npz")

    def close(self) -> None:
        if self._lasttoken_npz is not None:
            self._lasttoken_npz.close()
            self._lasttoken_npz = None
        if self._role_out_npz is not None:
            self._role_out_npz.close()
            self._role_out_npz = None
        if self._role_in_npz is not None:
            self._role_in_npz.close()
            self._role_in_npz = None

    def get_lasttoken_layer(self, layer_key: str) -> np.ndarray:
        if self._lasttoken_npz is None:
            raise RuntimeError("LazyForwardCache is not opened.")
        return np.asarray(self._lasttoken_npz[layer_key], dtype=np.float32)

    def get_role_out_layer(self, layer_key: str) -> np.ndarray:
        if self._role_out_npz is None:
            raise RuntimeError("LazyForwardCache is not opened.")
        return np.asarray(self._role_out_npz[layer_key], dtype=np.float32)

    def get_role_in_layer(self, layer_key: str) -> np.ndarray:
        if self._role_in_npz is None:
            raise RuntimeError("LazyForwardCache is not opened.")
        return np.asarray(self._role_in_npz[layer_key], dtype=np.float32)


def load_forward_cache(cache_dir: Path) -> LazyForwardCache:
    """Create lazy cache loader; arrays are loaded layer-by-layer on demand."""
    return LazyForwardCache(cache_dir)


def filter_gu_rows(row_index_df: pd.DataFrame) -> tuple[pd.DataFrame, np.ndarray]:
    """Return all rows from gu-only row index and identity indices."""
    df = row_index_df.copy().reset_index(drop=True)
    idx = np.arange(len(df), dtype=np.int64)
    return df, idx


def prepare_pair_metadata(
    row_index_df_gu: pd.DataFrame,
) -> tuple[pd.DataFrame, list[np.ndarray], pd.DataFrame, np.ndarray, np.ndarray]:
    """Build pair and node metadata for streaming layer-wise aggregation."""
    if row_index_df_gu.empty:
        raise ValueError("No gu rows available for RQ3 STEP2.")

    df = row_index_df_gu.copy()
    df["pair_id"] = df["orig"].astype(str) + "|" + df["dest"].astype(str)

    pairs_df = (
        df.groupby(["orig", "dest", "pair_id"], as_index=False)["dist_km"]
        .mean()
        .sort_values(["orig", "dest"], kind="stable")
        .reset_index(drop=True)
    )
    pair_order = pairs_df["pair_id"].tolist()

    group_idx = df.groupby("pair_id", sort=False).indices
    pair_row_indices = [np.asarray(group_idx[pid], dtype=np.int64) for pid in pair_order]

    nodes = sorted(set(pairs_df["orig"].astype(str)).union(set(pairs_df["dest"].astype(str))))
    nodes_df = pd.DataFrame({"node_id": nodes})
    node_to_idx = {n: i for i, n in enumerate(nodes)}
    pair_orig_node_idx = pairs_df["orig"].astype(str).map(node_to_idx).to_numpy(dtype=np.int64)
    pair_dest_node_idx = pairs_df["dest"].astype(str).map(node_to_idx).to_numpy(dtype=np.int64)

    return pairs_df, pair_row_indices, nodes_df, pair_orig_node_idx, pair_dest_node_idx


def aggregate_time_one_layer(
    pair_row_indices: list[np.ndarray],
    arr_row_level: np.ndarray,
) -> np.ndarray:
    """Aggregate row-level [n_rows, hidden] to pair-level [n_pairs, hidden] by time mean."""
    hidden_dim = int(arr_row_level.shape[1])
    out = np.zeros((len(pair_row_indices), hidden_dim), dtype=np.float32)
    for i, ridx in enumerate(pair_row_indices):
        out[i] = np.asarray(arr_row_level[ridx], dtype=np.float32).mean(axis=0, dtype=np.float32)
    return out


def aggregate_nodes_one_layer(
    pair_arr: np.ndarray,
    pair_node_idx: np.ndarray,
    n_nodes: int,
) -> np.ndarray:
    """Aggregate pair-level role array to node-level mean embedding."""
    hidden_dim = int(pair_arr.shape[1])
    sums = np.zeros((n_nodes, hidden_dim), dtype=np.float32)
    counts = np.zeros(n_nodes, dtype=np.int64)
    for i, nidx in enumerate(pair_node_idx):
        sums[nidx] += pair_arr[i]
        counts[nidx] += 1
    out = np.zeros_like(sums)
    valid = counts > 0
    out[valid] = sums[valid] / counts[valid, None]
    return out


def make_distance_bins(dist_km: pd.Series) -> tuple[np.ndarray, list[float]]:
    """Create fixed 5-bin labels (0..4) from quantile edges using np.digitize."""
    dist = pd.to_numeric(dist_km, errors="coerce")
    if dist.isna().all():
        dist = pd.Series(np.zeros(len(dist), dtype=float), index=dist.index)
    else:
        dist = dist.fillna(float(dist.median()))
    dist_np = dist.to_numpy(dtype=float)

    edges_np = np.quantile(dist_np, [0.0, 0.2, 0.4, 0.6, 0.8, 1.0])
    # np.digitize bins are internal cut points; always yields labels in 0..4.
    y = np.digitize(dist_np, bins=edges_np[1:-1], right=False).astype(np.int64)
    y = np.clip(y, 0, 4)
    return y, [float(v) for v in edges_np.tolist()]


def make_hash_split(pair_ids: pd.Series) -> tuple[np.ndarray, np.ndarray]:
    """Deterministic 80/20 split using sha1(pair_id) % 10."""
    p = pair_ids.astype(str).tolist()
    train = np.zeros(len(p), dtype=bool)
    for i, pid in enumerate(p):
        bucket = int(hashlib.sha1(pid.encode("utf-8")).hexdigest(), 16) % 10
        train[i] = bucket < 8
    test = ~train
    return train, test


def probe_distance_one_layer(
    x_pair: np.ndarray,
    y_labels: np.ndarray,
    train_mask: np.ndarray,
    test_mask: np.ndarray,
) -> float:
    """Train/test multinomial logistic regression and return test accuracy."""
    x_train = x_pair[train_mask]
    x_test = x_pair[test_mask]
    y_train = y_labels[train_mask]
    y_test = y_labels[test_mask]

    if len(x_train) == 0 or len(x_test) == 0 or len(np.unique(y_train)) < 2:
        return float("nan")

    clf = LogisticRegression(
        solver="lbfgs",
        max_iter=1000,
    )
    clf.fit(x_train, y_train)
    y_pred = clf.predict(x_test)
    return float(accuracy_score(y_test, y_pred))
