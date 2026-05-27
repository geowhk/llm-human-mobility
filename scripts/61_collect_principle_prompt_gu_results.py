from __future__ import annotations

import argparse
import json
import numbers
import sys
import warnings
import zipfile
from pathlib import Path
from typing import Any
from xml.sax.saxutils import escape

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]

CONDITION_ORDER = [
    "original",
    "geometry",
    "original_principles",
    "geometry_principles",
]

CONDITION_METADATA = {
    "original": {"base_variant": "original", "principle_prompt": False},
    "geometry": {"base_variant": "geometry", "principle_prompt": False},
    "original_principles": {"base_variant": "original", "principle_prompt": True},
    "geometry_principles": {"base_variant": "geometry", "principle_prompt": True},
}

METRIC_COLUMNS = [
    "n_eval",
    "mae",
    "rmse",
    "smape",
    "cpc",
    "delta_beta",
    "rho_origin",
    "rho_destination",
    "delta_gini",
]

LOWER_IS_BETTER = {"mae", "rmse", "smape", "delta_beta", "delta_gini"}
HIGHER_IS_BETTER = {"cpc", "rho_origin", "rho_destination"}


def _resolve_path(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path.resolve()
    return (PROJECT_ROOT / path).resolve()


def _parse_metric_arg(value: str) -> tuple[str, Path]:
    if "=" not in value:
        raise argparse.ArgumentTypeError(
            "--metrics values must use key=path format, for example original=aws_results/.../metrics.json"
        )
    key, path_str = value.split("=", 1)
    key = key.strip()
    path_str = path_str.strip()
    if not key:
        raise argparse.ArgumentTypeError("--metrics key must not be empty.")
    if not path_str:
        raise argparse.ArgumentTypeError(f"--metrics path for condition '{key}' must not be empty.")
    if key not in CONDITION_METADATA:
        raise argparse.ArgumentTypeError(
            f"Unknown metrics condition '{key}'. Expected one of: {', '.join(CONDITION_ORDER)}"
        )
    return key, _resolve_path(path_str)


def _load_metrics_json(condition: str, path: Path) -> dict[str, Any]:
    if not path.exists():
        raise FileNotFoundError(f"Metrics file not found for condition '{condition}': {path}")
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except json.JSONDecodeError as exc:
        raise ValueError(f"Invalid JSON metrics file for condition '{condition}': {path}") from exc
    if not isinstance(data, dict):
        raise ValueError(f"Metrics JSON must be an object for condition '{condition}': {path}")
    return data


def _select_metric_block(condition: str, metrics: dict[str, Any]) -> dict[str, Any]:
    gu_block = metrics.get("gu")
    if isinstance(gu_block, dict):
        return gu_block

    all_block = metrics.get("all")
    if isinstance(all_block, dict):
        warnings.warn(
            f"Metrics for condition '{condition}' do not include a 'gu' block; using 'all' block.",
            RuntimeWarning,
        )
        return all_block

    raise ValueError(f"Metrics for condition '{condition}' must include a 'gu' or 'all' block.")


def _nested_number(block: dict[str, Any], *keys: str) -> Any:
    current: Any = block
    for key in keys:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def _row_from_metrics(condition: str, path: Path) -> dict[str, Any]:
    metrics = _load_metrics_json(condition, path)
    block = _select_metric_block(condition, metrics)
    meta = CONDITION_METADATA[condition]
    return {
        "condition": condition,
        "base_variant": meta["base_variant"],
        "principle_prompt": meta["principle_prompt"],
        "n_eval": block.get("n_eval"),
        "mae": _nested_number(block, "accuracy", "mae"),
        "rmse": _nested_number(block, "accuracy", "rmse"),
        "smape": _nested_number(block, "accuracy", "smape"),
        "cpc": _nested_number(block, "patterns", "cpc"),
        "delta_beta": _nested_number(block, "patterns", "delta_beta"),
        "rho_origin": _nested_number(block, "patterns", "rho_origin"),
        "rho_destination": _nested_number(block, "patterns", "rho_destination"),
        "delta_gini": _nested_number(block, "patterns", "delta_gini"),
    }


def _build_delta_row(
    metrics_df: pd.DataFrame,
    base_variant: str,
    base_condition: str,
    principle_condition: str,
) -> dict[str, Any]:
    base_rows = metrics_df[metrics_df["condition"] == base_condition]
    principle_rows = metrics_df[metrics_df["condition"] == principle_condition]
    if len(base_rows) != 1 or len(principle_rows) != 1:
        raise ValueError(f"Cannot compute delta for {principle_condition} - {base_condition}.")

    base = base_rows.iloc[0]
    principle = principle_rows.iloc[0]
    row: dict[str, Any] = {
        "base_variant": base_variant,
        "base_condition": base_condition,
        "principle_condition": principle_condition,
    }

    for metric in METRIC_COLUMNS:
        if metric == "n_eval":
            continue
        base_value = pd.to_numeric(pd.Series([base[metric]]), errors="coerce").iloc[0]
        principle_value = pd.to_numeric(pd.Series([principle[metric]]), errors="coerce").iloc[0]
        row[f"delta_{metric}"] = principle_value - base_value

    for metric in sorted(LOWER_IS_BETTER | HIGHER_IS_BETTER):
        delta = row[f"delta_{metric}"]
        if pd.isna(delta):
            improved = pd.NA
        elif metric in LOWER_IS_BETTER:
            improved = bool(delta < 0)
        else:
            improved = bool(delta > 0)
        row[f"improved_{metric}"] = improved

    return row


def _build_delta_df(metrics_df: pd.DataFrame) -> pd.DataFrame:
    rows = [
        _build_delta_row(
            metrics_df=metrics_df,
            base_variant="original",
            base_condition="original",
            principle_condition="original_principles",
        ),
        _build_delta_row(
            metrics_df=metrics_df,
            base_variant="geometry",
            base_condition="geometry",
            principle_condition="geometry_principles",
        ),
    ]
    columns = [
        "base_variant",
        "base_condition",
        "principle_condition",
        "delta_mae",
        "delta_rmse",
        "delta_smape",
        "delta_cpc",
        "delta_delta_beta",
        "delta_rho_origin",
        "delta_rho_destination",
        "delta_delta_gini",
        "improved_mae",
        "improved_rmse",
        "improved_smape",
        "improved_cpc",
        "improved_delta_beta",
        "improved_rho_origin",
        "improved_rho_destination",
        "improved_delta_gini",
    ]
    return pd.DataFrame(rows)[columns]


def _excel_col_name(index: int) -> str:
    name = ""
    index += 1
    while index:
        index, remainder = divmod(index - 1, 26)
        name = chr(65 + remainder) + name
    return name


def _xlsx_cell_xml(row_idx: int, col_idx: int, value: Any) -> str:
    cell_ref = f"{_excel_col_name(col_idx)}{row_idx}"
    if value is None or pd.isna(value):
        return f'<c r="{cell_ref}"/>'
    if isinstance(value, bool):
        return f'<c r="{cell_ref}" t="b"><v>{1 if value else 0}</v></c>'
    if isinstance(value, numbers.Real):
        return f'<c r="{cell_ref}"><v>{value}</v></c>'
    return (
        f'<c r="{cell_ref}" t="inlineStr"><is><t>'
        f'{escape(str(value))}'
        "</t></is></c>"
    )


def _write_xlsx(df: pd.DataFrame, path: Path, sheet_name: str = "Sheet1") -> None:
    rows_xml: list[str] = []
    header_cells = [
        _xlsx_cell_xml(1, col_idx, column)
        for col_idx, column in enumerate(df.columns)
    ]
    rows_xml.append(f'<row r="1">{"".join(header_cells)}</row>')

    for row_offset, row in enumerate(df.itertuples(index=False, name=None), start=2):
        cells = [
            _xlsx_cell_xml(row_offset, col_idx, value)
            for col_idx, value in enumerate(row)
        ]
        rows_xml.append(f'<row r="{row_offset}">{"".join(cells)}</row>')

    max_col = max(1, len(df.columns))
    max_row = max(1, len(df) + 1)
    dimension = f"A1:{_excel_col_name(max_col - 1)}{max_row}"
    safe_sheet_name = escape(sheet_name[:31] or "Sheet1")

    worksheet_xml = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <dimension ref="{dimension}"/>
  <sheetData>
    {''.join(rows_xml)}
  </sheetData>
</worksheet>
"""
    workbook_xml = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <sheets>
    <sheet name="{safe_sheet_name}" sheetId="1" r:id="rId1"/>
  </sheets>
</workbook>
"""
    workbook_rels_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
  <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>
</Relationships>
"""
    root_rels_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
</Relationships>
"""
    content_types_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
  <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
  <Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>
</Types>
"""
    styles_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <fonts count="1"><font><sz val="11"/><name val="Calibri"/></font></fonts>
  <fills count="1"><fill><patternFill patternType="none"/></fill></fills>
  <borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>
  <cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>
  <cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>
</styleSheet>
"""

    with zipfile.ZipFile(path, "w", compression=zipfile.ZIP_DEFLATED) as xlsx:
        xlsx.writestr("[Content_Types].xml", content_types_xml)
        xlsx.writestr("_rels/.rels", root_rels_xml)
        xlsx.writestr("xl/workbook.xml", workbook_xml)
        xlsx.writestr("xl/_rels/workbook.xml.rels", workbook_rels_xml)
        xlsx.writestr("xl/worksheets/sheet1.xml", worksheet_xml)
        xlsx.writestr("xl/styles.xml", styles_xml)


def _write_outputs(metrics_df: pd.DataFrame, delta_df: pd.DataFrame, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)

    metrics_csv = output_dir / "gu_principle_prompt_metrics.csv"
    metrics_xlsx = output_dir / "gu_principle_prompt_metrics.xlsx"
    delta_csv = output_dir / "gu_principle_prompt_metrics_delta.csv"
    delta_xlsx = output_dir / "gu_principle_prompt_metrics_delta.xlsx"

    metrics_df.to_csv(metrics_csv, index=False)
    _write_xlsx(metrics_df, metrics_xlsx, sheet_name="metrics")
    delta_df.to_csv(delta_csv, index=False)
    _write_xlsx(delta_df, delta_xlsx, sheet_name="delta")

    print(f"Wrote {metrics_csv}")
    print(f"Wrote {metrics_xlsx}")
    print(f"Wrote {delta_csv}")
    print(f"Wrote {delta_xlsx}")


def _write_readme(output_dir: Path) -> None:
    readme = output_dir / "README.md"
    content = """# Principle Prompt GU Experiment

## 1. Generate principle prompt configs
python scripts/60_make_principle_prompt_gu_configs.py \\
  --original-config configs/gu_original_rq12.yaml \\
  --geometry-config configs/gu_geometry_rq12.yaml \\
  --output-dir configs/generated

## 2. Run gu principle prompt conditions
python scripts/40_run_rq12_gu.py --config configs/generated/gu_original_principles_rq12.yaml
python scripts/40_run_rq12_gu.py --config configs/generated/gu_geometry_principles_rq12.yaml

## 3. Collect comparison tables
python scripts/61_collect_principle_prompt_gu_results.py \\
  --metrics original=<기존 지명 기반 metrics.json 경로> \\
  --metrics geometry=<기존 기하 기반 metrics.json 경로> \\
  --metrics original_principles=<원리 명시 지명 기반 metrics.json 경로> \\
  --metrics geometry_principles=<원리 명시 기하 기반 metrics.json 경로> \\
  --output-dir outputs/principle_prompt_gu
"""
    readme.write_text(content, encoding="utf-8")
    print(f"Wrote {readme}")


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Collect gu RQ1/RQ2 metrics for original, geometry, and principle prompt conditions."
    )
    parser.add_argument(
        "--metrics",
        action="append",
        required=True,
        type=_parse_metric_arg,
        help="Condition metrics in key=path format. Repeat for original, geometry, original_principles, geometry_principles.",
    )
    parser.add_argument(
        "--output-dir",
        required=True,
        help="Directory where comparison CSV/XLSX files will be written.",
    )
    return parser


def main() -> None:
    parser = _build_parser()
    args = parser.parse_args()

    metric_paths: dict[str, Path] = {}
    for condition, path in args.metrics:
        if condition in metric_paths:
            parser.error(f"Duplicate --metrics condition: {condition}")
        metric_paths[condition] = path

    missing_conditions = [condition for condition in CONDITION_ORDER if condition not in metric_paths]
    if missing_conditions:
        parser.error("Missing --metrics entries for: " + ", ".join(missing_conditions))

    try:
        rows = [
            _row_from_metrics(condition, metric_paths[condition])
            for condition in CONDITION_ORDER
        ]
        metrics_df = pd.DataFrame(rows)[
            ["condition", "base_variant", "principle_prompt", *METRIC_COLUMNS]
        ]
        delta_df = _build_delta_df(metrics_df)
        output_dir = _resolve_path(args.output_dir)
        _write_outputs(metrics_df, delta_df, output_dir)
        _write_readme(output_dir)
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc


if __name__ == "__main__":
    main()
