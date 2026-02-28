from __future__ import annotations

import re
from typing import Any

import pandas as pd


def parse_first_integer(text: str, allow_commas: bool = True) -> int | None:
    """Extract the first integer from text."""
    s = str(text)
    if allow_commas:
        pattern = r"[-+]?(?:\d{1,3}(?:,\d{3})+|\d+)"
    else:
        pattern = r"[-+]?\d+"

    match = re.search(pattern, s)
    if match is None:
        return None

    token = match.group(0)
    if allow_commas:
        token = token.replace(",", "")
    return int(token)


def apply_postprocess(
    value: int | None,
    clip_negative_to_zero: bool = True,
    max_value: int | None = None,
) -> int | None:
    """Postprocess parsed integer value by clipping rules."""
    if value is None:
        return None

    out = int(value)
    if clip_negative_to_zero and out < 0:
        out = 0

    if max_value is not None:
        out = min(max(out, 0), int(max_value))

    return out


def parse_generation_rows(rows: list[dict], config: dict[str, Any]) -> pd.DataFrame:
    """Parse raw generation rows into numeric predictions."""
    parsing_cfg = config.get("parsing", {})
    allow_commas = bool(parsing_cfg.get("allow_commas", True))
    clip_negative_to_zero = bool(parsing_cfg.get("clip_negative_to_zero", True))
    max_value = parsing_cfg.get("max_value")
    if max_value is not None:
        max_value = int(max_value)

    parsed_rows: list[dict[str, Any]] = []
    for row in rows:
        raw_output = str(row.get("raw_output", ""))
        parsed = parse_first_integer(raw_output, allow_commas=allow_commas)
        y_hat_int = apply_postprocess(
            parsed,
            clip_negative_to_zero=clip_negative_to_zero,
            max_value=max_value,
        )
        parsed_rows.append(
            {
                "query_id": str(row.get("query_id", "")),
                "scale": str(row.get("scale", "")),
                "repeat_id": int(row.get("repeat_id", 0)),
                "raw_output": raw_output,
                "y_hat_int": y_hat_int,
                "parse_ok": y_hat_int is not None,
            }
        )

    return pd.DataFrame(parsed_rows)


def classify_parse_failure(
    text: str,
    allow_commas: bool = True,
) -> str:
    """Classify parsing result into OK / failure types."""
    raw = str(text)
    if raw.strip() == "":
        return "FAIL_EMPTY"
    try:
        value = parse_first_integer(raw, allow_commas=allow_commas)
        if value is None:
            return "FAIL_NO_INT"
        return "OK"
    except Exception:
        return "FAIL_OTHER"
