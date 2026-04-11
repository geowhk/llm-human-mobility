from __future__ import annotations

"""
Create pair-complete chunk parquet files for the full dong dataset.

Usage:
    python scripts/35_make_dong_chunks.py
    python scripts/35_make_dong_chunks.py --pairs-per-chunk 7500 --seed 202511
"""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_INPUT_PATH = PROJECT_ROOT / "data" / "processed" / "gt_flow_dong.parquet"
DEFAULT_OUTPUT_DIR = PROJECT_ROOT / "data" / "processed" / "chunks"
REQUIRED_COLUMNS = [
    "orig",
    "dest",
    "arrival_hour",
    "flow",
    "dist_km",
    "orig_lon",
    "orig_lat",
    "dest_lon",
    "dest_lat",
]
EXPECTED_HOURS = 24


def _resolve_path(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path.resolve()
    return (PROJECT_ROOT / path).resolve()


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Build pair-complete chunk parquet files for the dong flow dataset."
    )
    parser.add_argument(
        "--input-path",
        default=str(DEFAULT_INPUT_PATH.relative_to(PROJECT_ROOT)),
        help="Path to the source dong parquet file.",
    )
    parser.add_argument(
        "--output-dir",
        default=str(DEFAULT_OUTPUT_DIR.relative_to(PROJECT_ROOT)),
        help="Directory where chunk parquet files and the manifest will be written.",
    )
    parser.add_argument(
        "--pairs-per-chunk",
        type=int,
        default=7500,
        help="Number of complete pairs to include in each chunk.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=202511,
        help="Random seed used for deterministic pair shuffling before chunking.",
    )
    return parser


def _validate_required_columns(df: pd.DataFrame) -> None:
    missing = [col for col in REQUIRED_COLUMNS if col not in df.columns]
    if missing:
        raise ValueError(
            "Input parquet is missing required columns: " + ", ".join(sorted(missing))
        )


def _prepare_dataframe(input_path: Path) -> tuple[pd.DataFrame, pd.Series, int]:
    if not input_path.exists():
        raise FileNotFoundError(f"Input parquet not found: {input_path}")

    df = pd.read_parquet(input_path)
    _validate_required_columns(df)

    df = df[REQUIRED_COLUMNS].copy()
    df["orig"] = df["orig"].astype(str)
    df["dest"] = df["dest"].astype(str)
    df["pair_id"] = df["orig"] + "|" + df["dest"]

    pair_hours = df.groupby("pair_id", sort=False)["arrival_hour"].nunique()
    complete_mask = pair_hours == EXPECTED_HOURS
    complete_pairs = pair_hours.index[complete_mask]
    excluded_incomplete_pairs = int((~complete_mask).sum())

    complete_df = df[df["pair_id"].isin(complete_pairs)].copy()
    return complete_df, pd.Index(complete_pairs), excluded_incomplete_pairs


def _chunk_pair_ids(pair_ids: np.ndarray, pairs_per_chunk: int) -> list[np.ndarray]:
    return [
        pair_ids[start : start + pairs_per_chunk]
        for start in range(0, len(pair_ids), pairs_per_chunk)
    ]


def _validate_chunk(chunk_df: pd.DataFrame, expected_pairs: int) -> None:
    actual_pairs = int(chunk_df["pair_id"].nunique())
    if actual_pairs != expected_pairs:
        raise ValueError(
            "Chunk validation failed: expected "
            f"{expected_pairs} pairs but found {actual_pairs}."
        )

    hours_per_pair = chunk_df.groupby("pair_id", sort=False)["arrival_hour"].nunique()
    invalid_pairs = hours_per_pair[hours_per_pair != EXPECTED_HOURS]
    if not invalid_pairs.empty:
        sample_pairs = ", ".join(map(str, invalid_pairs.index[:5]))
        raise ValueError(
            "Chunk validation failed: some pairs do not have exactly "
            f"{EXPECTED_HOURS} unique arrival hours. "
            f"Invalid pair count={len(invalid_pairs)}. Sample pair_ids: {sample_pairs}"
        )


def main() -> None:
    args = _build_parser().parse_args()

    if args.pairs_per_chunk <= 0:
        raise ValueError("--pairs-per-chunk must be a positive integer.")

    input_path = _resolve_path(args.input_path)
    output_dir = _resolve_path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Reading source parquet: {input_path}")
    df, complete_pairs, excluded_incomplete_pairs = _prepare_dataframe(input_path)

    n_complete_pairs = int(len(complete_pairs))
    if n_complete_pairs == 0:
        raise ValueError("No complete pairs with exactly 24 unique arrival_hour values were found.")

    rng = np.random.default_rng(args.seed)
    shuffled_pairs = complete_pairs.to_numpy(copy=True)
    rng.shuffle(shuffled_pairs)
    pair_chunks = _chunk_pair_ids(shuffled_pairs, args.pairs_per_chunk)

    print(f"Complete pairs included: {n_complete_pairs}")
    print(f"Incomplete pairs excluded: {excluded_incomplete_pairs}")
    print(f"Writing {len(pair_chunks)} chunk files to: {output_dir}")

    chunk_entries: list[dict[str, object]] = []

    for idx, chunk_pair_ids in enumerate(pair_chunks, start=1):
        chunk_id = f"dong_chunk_{idx:03d}"
        chunk_path = output_dir / f"{chunk_id}.parquet"

        chunk_df = df[df["pair_id"].isin(chunk_pair_ids)].copy()
        _validate_chunk(chunk_df, expected_pairs=len(chunk_pair_ids))

        # Keep the original dataset columns in the chunk files.
        chunk_df = chunk_df[REQUIRED_COLUMNS]
        chunk_df.to_parquet(chunk_path, index=False)

        n_rows = int(len(chunk_df))
        n_pairs = int(len(chunk_pair_ids))
        print(f"Wrote {chunk_path.name}: n_pairs={n_pairs}, n_rows={n_rows}")

        chunk_entries.append(
            {
                "chunk_id": chunk_id,
                "file_path": str(chunk_path),
                "n_pairs": n_pairs,
                "n_rows": n_rows,
            }
        )

    manifest = {
        "source_input_path": str(input_path),
        "pairs_per_chunk": int(args.pairs_per_chunk),
        "seed": int(args.seed),
        "number_of_complete_pairs": n_complete_pairs,
        "number_of_excluded_incomplete_pairs": excluded_incomplete_pairs,
        "number_of_chunks": len(chunk_entries),
        "chunks": chunk_entries,
    }

    manifest_path = output_dir / "chunk_manifest.json"
    with open(manifest_path, "w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=2)
        f.write("\n")

    print(f"Wrote manifest: {manifest_path}")


if __name__ == "__main__":
    main()
