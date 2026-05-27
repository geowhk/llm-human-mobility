from __future__ import annotations

import argparse
import copy
import string
from pathlib import Path
from typing import Any

import yaml


PROJECT_ROOT = Path(__file__).resolve().parents[1]

PRINCIPLE_TEMPLATE = """Task: Estimate the hourly number of people traveling from one administrative area to another in Seoul.

General spatial interaction principles:
Some origins generate more trips than others because they differ in population, activity, and urban function.
Some destinations attract more trips than others because they differ in opportunities, services, jobs, and urban centrality.
All else being equal, flows tend to decrease as distance or travel cost between origin and destination increases.

Origin: {origin_text}
Destination: {dest_text}
Hour: {hour}
"""

FORBIDDEN_TEMPORAL_RHYTHM_SENTENCE = (
    "Hourly flows can vary by time of day because urban activities have daily temporal rhythms."
)
ALLOWED_TEMPLATE_FIELDS = {"origin_text", "dest_text", "hour"}


class LiteralString(str):
    """String marker for YAML block scalar output."""


class LiteralDumper(yaml.SafeDumper):
    pass


def _literal_string_representer(dumper: yaml.Dumper, data: LiteralString) -> yaml.ScalarNode:
    return dumper.represent_scalar("tag:yaml.org,2002:str", data, style="|")


LiteralDumper.add_representer(LiteralString, _literal_string_representer)


def _resolve_path(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path.resolve()
    return (PROJECT_ROOT / path).resolve()


def _display_path(path: Path) -> str:
    try:
        return str(path.resolve().relative_to(PROJECT_ROOT))
    except ValueError:
        return str(path.resolve())


def _load_yaml(path: Path) -> dict[str, Any]:
    if not path.exists():
        raise FileNotFoundError(f"Config file not found: {path}")
    try:
        with path.open("r", encoding="utf-8") as f:
            data = yaml.safe_load(f)
    except yaml.YAMLError as exc:
        raise ValueError(f"Invalid YAML format: {path}") from exc
    if not isinstance(data, dict):
        raise ValueError(f"Config must be a YAML mapping: {path}")
    return data


def _validate_template(template: str) -> None:
    if template != PRINCIPLE_TEMPLATE:
        raise ValueError("Generated prompt.template does not match the expected principle template.")
    if FORBIDDEN_TEMPORAL_RHYTHM_SENTENCE in template:
        raise ValueError("Generated prompt.template includes the forbidden temporal rhythm sentence.")
    if template.rstrip("\n").splitlines()[-1] != "Hour: {hour}":
        raise ValueError("Generated prompt.template must end with 'Hour: {hour}'.")

    fields: set[str] = set()
    try:
        parsed = string.Formatter().parse(template)
        for _, field_name, _, _ in parsed:
            if field_name is not None:
                fields.add(field_name)
    except ValueError as exc:
        raise ValueError("Generated prompt.template has invalid format placeholders.") from exc

    invalid_fields = fields - ALLOWED_TEMPLATE_FIELDS
    missing_fields = ALLOWED_TEMPLATE_FIELDS - fields
    if invalid_fields:
        raise ValueError(
            "Generated prompt.template includes unsupported placeholders: "
            + ", ".join(sorted(invalid_fields))
        )
    if missing_fields:
        raise ValueError(
            "Generated prompt.template is missing required placeholders: "
            + ", ".join(sorted(missing_fields))
        )


def _build_principle_config(
    base_config: dict[str, Any],
    expected_variant: str,
    run_id: str,
) -> dict[str, Any]:
    config = copy.deepcopy(base_config)
    prompt = config.get("prompt")
    if not isinstance(prompt, dict):
        raise ValueError("Config must include a 'prompt' mapping.")

    variant = str(prompt.get("variant", "")).strip()
    if variant != expected_variant:
        raise ValueError(
            f"Expected prompt.variant='{expected_variant}', found '{variant or '<missing>'}'."
        )

    prompt["template"] = LiteralString(PRINCIPLE_TEMPLATE)
    prompt["principle_prompt"] = True
    prompt["principle_type"] = "spatial_interaction_principles"

    run = config.get("run")
    if not isinstance(run, dict):
        raise ValueError("Config must include a 'run' mapping so run.run_id can be set.")
    run["run_id"] = run_id

    _validate_template(str(prompt["template"]))
    return config


def _write_yaml(path: Path, config: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        yaml.dump(
            config,
            f,
            Dumper=LiteralDumper,
            sort_keys=False,
            allow_unicode=True,
            default_flow_style=False,
        )


def _validate_written_config(path: Path, expected_variant: str, expected_run_id: str) -> None:
    config = _load_yaml(path)
    prompt = config.get("prompt")
    if not isinstance(prompt, dict):
        raise ValueError(f"Generated config missing prompt mapping: {path}")
    run = config.get("run")
    if not isinstance(run, dict):
        raise ValueError(f"Generated config missing run mapping: {path}")

    variant = str(prompt.get("variant", "")).strip()
    if variant != expected_variant:
        raise ValueError(
            f"Generated config {path} has prompt.variant='{variant}', expected '{expected_variant}'."
        )
    run_id = str(run.get("run_id", "")).strip()
    if run_id != expected_run_id:
        raise ValueError(
            f"Generated config {path} has run.run_id='{run_id}', expected '{expected_run_id}'."
        )
    if prompt.get("principle_prompt") is not True:
        raise ValueError(f"Generated config {path} missing prompt.principle_prompt: true.")
    if prompt.get("principle_type") != "spatial_interaction_principles":
        raise ValueError(f"Generated config {path} has unexpected prompt.principle_type.")
    _validate_template(str(prompt.get("template", "")))


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Generate gu RQ1/RQ2 configs with explicit spatial interaction principles."
    )
    parser.add_argument(
        "--original-config",
        required=True,
        help="Existing gu original config YAML path.",
    )
    parser.add_argument(
        "--geometry-config",
        required=True,
        help="Existing gu geometry config YAML path.",
    )
    parser.add_argument(
        "--output-dir",
        required=True,
        help="Directory where generated principle prompt configs will be written.",
    )
    return parser


def main() -> None:
    args = _build_parser().parse_args()

    original_config_path = _resolve_path(args.original_config)
    geometry_config_path = _resolve_path(args.geometry_config)
    output_dir = _resolve_path(args.output_dir)

    original_base = _load_yaml(original_config_path)
    geometry_base = _load_yaml(geometry_config_path)

    jobs = [
        (
            _build_principle_config(
                base_config=original_base,
                expected_variant="original",
                run_id="gu_original_principles",
            ),
            "original",
            "gu_original_principles",
            output_dir / "gu_original_principles_rq12.yaml",
        ),
        (
            _build_principle_config(
                base_config=geometry_base,
                expected_variant="geometry",
                run_id="gu_geometry_principles",
            ),
            "geometry",
            "gu_geometry_principles",
            output_dir / "gu_geometry_principles_rq12.yaml",
        ),
    ]

    for config, _, _, out_path in jobs:
        _write_yaml(out_path, config)

    for _, expected_variant, expected_run_id, out_path in jobs:
        _validate_written_config(out_path, expected_variant, expected_run_id)

    print("Generated principle prompt gu configs:")
    for _, _, _, out_path in jobs:
        print(f"- {_display_path(out_path)}")

    print("\nRun commands:")
    for _, _, _, out_path in jobs:
        print(f"python scripts/40_run_rq12_gu.py --config {_display_path(out_path)}")


if __name__ == "__main__":
    main()
