from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path

import pandas as pd


def ensure_dir(path: str) -> None:
    """Create a directory if it does not already exist."""
    Path(path).mkdir(parents=True, exist_ok=True)


def make_run_dir(output_root: str, run_id: str | None) -> tuple[str, str]:
    """Create a timestamped run directory and return (run_dir, final_run_id)."""
    base_id = (run_id or "").strip() or "dev"
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    final_run_id = f"{base_id}_{timestamp}"

    root = Path(output_root)
    ensure_dir(str(root))

    run_dir = root / final_run_id
    run_dir.mkdir(parents=True, exist_ok=False)
    return str(run_dir), final_run_id


def load_parquet(path: str) -> pd.DataFrame:
    """Load a parquet file into a pandas DataFrame."""
    file_path = Path(path)
    if not file_path.exists():
        raise FileNotFoundError(f"Parquet file not found: {path}")
    return pd.read_parquet(file_path)


def validate_required_columns(
    df: pd.DataFrame, required_cols: list[str], name: str
) -> None:
    """Validate required columns exist in the DataFrame."""
    missing = [col for col in required_cols if col not in df.columns]
    if missing:
        raise ValueError(
            f"Missing required columns in {name}: {', '.join(missing)}"
        )


def save_parquet(df: pd.DataFrame, path: str) -> None:
    """Save a DataFrame as parquet, creating parent directories as needed."""
    file_path = Path(path)
    file_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(file_path, index=False)


def write_text(path: str, text: str, append: bool = True) -> None:
    """Write text to a file in append or overwrite mode."""
    file_path = Path(path)
    file_path.parent.mkdir(parents=True, exist_ok=True)
    mode = "a" if append else "w"
    with file_path.open(mode, encoding="utf-8") as f:
        f.write(text)


def write_json(path: str, obj: dict) -> None:
    """Write a dict to JSON with UTF-8 encoding."""
    file_path = Path(path)
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with file_path.open("w", encoding="utf-8") as f:
        json.dump(obj, f, indent=2, ensure_ascii=False)
