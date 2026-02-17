from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml


def _get_nested(data: dict[str, Any], path: list[str]) -> Any:
    current: Any = data
    for key in path:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def load_config(path: str) -> dict[str, Any]:
    config_path = Path(path)
    if not config_path.exists():
        raise ValueError(f"Config file not found: {path}")

    try:
        with config_path.open("r", encoding="utf-8") as f:
            config = yaml.safe_load(f)
    except yaml.YAMLError as exc:
        raise ValueError(f"Invalid YAML format: {path}") from exc

    if not isinstance(config, dict):
        raise ValueError(f"Config must be a YAML mapping: {path}")

    columns = config.setdefault("columns", {})
    if isinstance(columns, dict):
        if "orig" not in columns and "origin_id" in columns:
            columns["orig"] = columns["origin_id"]
        if "dest" not in columns and "dest_id" in columns:
            columns["dest"] = columns["dest_id"]

    required_paths = [
        ["schema_version"],
        ["data", "datasets", "gu", "path"],
        ["data", "datasets", "dong", "path"],
        ["columns", "orig"],
        ["columns", "dest"],
        ["columns", "hour"],
        ["columns", "dist_km"],
        ["columns", "flow_gt"],
    ]

    missing = []
    for req_path in required_paths:
        value = _get_nested(config, req_path)
        if value is None or (isinstance(value, str) and value.strip() == ""):
            missing.append(".".join(req_path))

    if missing:
        raise ValueError(
            "Missing required config fields: " + ", ".join(missing)
        )

    return config
