from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any
import warnings

import pandas as pd


def make_query_id(scale: str, origin: str, dest: str, hour: int) -> str:
    """Build a deterministic query id from scale, hour, origin, and destination."""
    query_key = f"{scale}|{hour:02d}|{origin}|{dest}"
    return hashlib.sha1(query_key.encode("utf-8")).hexdigest()


def build_prompt(template: str, origin_text: str, dest_text: str, hour: int) -> str:
    """Render prompt text from template placeholders."""
    return template.format(
        origin_text=origin_text,
        dest_text=dest_text,
        hour=hour,
    ).strip()


def load_code_name_map(path: str) -> dict[str, str]:
    """Load code->name mapping from parquet."""
    map_df = pd.read_parquet(path)
    if "code" not in map_df.columns:
        raise ValueError(f"Mapping parquet must include 'code' column: {path}")

    if "prompt_name" in map_df.columns:
        name_col = "prompt_name"
    elif "name" in map_df.columns:
        name_col = "name"
    else:
        raise ValueError(
            "Mapping parquet must include 'name' or 'prompt_name' column: "
            f"{path}"
        )

    normalized = map_df[["code", name_col]].copy()
    normalized["code"] = normalized["code"].astype(str).str.strip()
    normalized[name_col] = normalized[name_col].astype(str).str.strip()
    normalized = normalized[
        (normalized["code"] != "") & (normalized[name_col] != "")
    ]
    normalized = normalized.drop_duplicates(subset=["code"], keep="first")
    return dict(zip(normalized["code"], normalized[name_col]))


def apply_name_map(code: str, name_map: dict[str, str]) -> str:
    """Map code to readable name, fallback to code if missing."""
    code_str = str(code).strip()
    return name_map.get(code_str, code_str)


def normalize_location_text(text: str) -> str:
    """Normalize location text so 'Seoul' appears exactly once at the end."""
    tokens = [token.strip() for token in str(text).split(",")]
    base_tokens = [
        token for token in tokens
        if token and token.casefold() != "seoul"
    ]
    if base_tokens:
        return f"{', '.join(base_tokens)}, Seoul"
    return "Seoul"


def build_prompts_df(df: pd.DataFrame, scale: str, config: dict[str, Any]) -> pd.DataFrame:
    """Build prompt records from a source dataframe and config column mapping."""
    columns = config["columns"]
    template = config["prompt"]["template"]

    orig_col = columns["origin_id"]
    dest_col = columns["dest_id"]
    hour_col = columns["hour"]
    dist_col = columns["dist_km"]
    flow_col = columns["flow_gt"]
    name_map: dict[str, str] = {}

    map_path = (
        config.get("data", {})
        .get("id_to_prompt_name", {})
        .get(scale)
    )
    if isinstance(map_path, str) and map_path.strip():
        try:
            if Path(map_path).exists():
                name_map = load_code_name_map(map_path)
        except Exception as exc:
            warnings.warn(
                f"Failed to load id_to_prompt_name mapping for scale={scale}: {exc}",
                RuntimeWarning,
            )

    out = pd.DataFrame(
        {
            "scale": scale,
            "origin_id": df[orig_col].astype(str),
            "dest_id": df[dest_col].astype(str),
            "hour": df[hour_col].astype(int),
            "origin_text": df[orig_col].astype(str).map(
                lambda x: normalize_location_text(apply_name_map(x, name_map))
            ),
            "dest_text": df[dest_col].astype(str).map(
                lambda x: normalize_location_text(apply_name_map(x, name_map))
            ),
        }
    )

    if dist_col in df.columns:
        out["dist_km"] = df[dist_col]
    if flow_col in df.columns:
        out["flow_gt"] = df[flow_col]

    out["query_id"] = out.apply(
        lambda row: make_query_id(
            scale=row["scale"],
            origin=row["origin_id"],
            dest=row["dest_id"],
            hour=int(row["hour"]),
        ),
        axis=1,
    )
    out["prompt_text"] = out.apply(
        lambda row: build_prompt(
            template=template,
            origin_text=row["origin_text"],
            dest_text=row["dest_text"],
            hour=int(row["hour"]),
        ),
        axis=1,
    )

    ordered = [
        "query_id",
        "scale",
        "origin_id",
        "dest_id",
        "hour",
    ]
    if "dist_km" in out.columns:
        ordered.append("dist_km")
    if "flow_gt" in out.columns:
        ordered.append("flow_gt")
    ordered.extend(["origin_text", "dest_text", "prompt_text"])
    return out[ordered]
