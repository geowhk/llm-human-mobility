from __future__ import annotations

import argparse
import json
import subprocess
import sys
from datetime import datetime
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[1]
RUNS_ROOT = PROJECT_ROOT / "results" / "runs"
CHUNK_RUNNER = PROJECT_ROOT / "scripts" / "41_run_rq12_dong_chunk.py"


def _resolve_path(path_str: str) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path.resolve()
    return (PROJECT_ROOT / path).resolve()


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Sequentially run dong chunk RQ12 config files."
    )
    parser.add_argument("--config-dir", required=True, help="Directory containing chunk config YAML files.")
    parser.add_argument("--config-prefix", required=True, help="Filename prefix for chunk config selection.")
    parser.add_argument("--log-dir", required=True, help="Directory for wrapper and per-chunk logs.")
    parser.add_argument("--dry-run", action="store_true", help="Print planned execution order without running.")
    parser.add_argument(
        "--start-from",
        default=None,
        help="Start from the first config whose filename is >= this value.",
    )
    parser.add_argument(
        "--stop-after",
        type=int,
        default=None,
        help="Execute at most this many eligible chunk configs.",
    )
    parser.add_argument(
        "--python-executable",
        default=sys.executable or "python",
        help="Python executable to use for chunk runs.",
    )
    return parser


def _log_message(log_path: Path, message: str) -> None:
    line = f"[WRAPPER] {message}"
    print(line, flush=True)
    with log_path.open("a", encoding="utf-8") as f:
        f.write(line + "\n")


def _write_manifest(path: Path, payload: dict) -> None:
    with path.open("w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2)
        f.write("\n")


def _discover_configs(config_dir: Path, config_prefix: str) -> list[Path]:
    if not config_dir.exists():
        raise FileNotFoundError(f"Config directory not found: {config_dir}")
    if not config_dir.is_dir():
        raise NotADirectoryError(f"Config directory is not a directory: {config_dir}")

    configs = [
        path.resolve()
        for path in config_dir.iterdir()
        if path.is_file()
        and path.name.startswith(config_prefix)
        and path.suffix.lower() in {".yaml", ".yml"}
    ]
    return sorted(configs, key=lambda p: p.name)


def _is_completed_run(run_dir: Path) -> bool:
    pred_path = run_dir / "rq12" / "predictions.parquet"
    metrics_path = run_dir / "rq12" / "metrics.json"
    log_path = run_dir / "log.txt"
    if not pred_path.exists() or not metrics_path.exists() or not log_path.exists():
        return False
    try:
        text = log_path.read_text(encoding="utf-8")
    except OSError:
        return False
    return "RQ12 DONE" in text


def _find_completed_run(chunk_run_id: str) -> Path | None:
    if not RUNS_ROOT.exists() or not RUNS_ROOT.is_dir():
        return None

    matches = [
        path.resolve()
        for path in RUNS_ROOT.iterdir()
        if path.is_dir() and chunk_run_id in path.name and _is_completed_run(path)
    ]
    if not matches:
        return None
    return sorted(matches, key=lambda p: p.name)[-1]


def _apply_start_from(configs: list[Path], start_from: str | None) -> list[Path]:
    if not start_from:
        return configs
    return [cfg for cfg in configs if cfg.stem >= start_from or cfg.name >= start_from]


def _apply_stop_after(configs: list[Path], stop_after: int | None) -> list[Path]:
    if stop_after is None:
        return configs
    return configs[:stop_after]


def main() -> None:
    args = _build_parser().parse_args()
    if args.stop_after is not None and args.stop_after <= 0:
        raise ValueError("--stop-after must be a positive integer when provided.")
    if not CHUNK_RUNNER.exists():
        raise FileNotFoundError(f"Chunk runner script not found: {CHUNK_RUNNER}")

    config_dir = _resolve_path(args.config_dir)
    log_dir = _resolve_path(args.log_dir)
    log_dir.mkdir(parents=True, exist_ok=True)

    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    wrapper_base = f"{args.config_prefix.rstrip('_')}_chunks_{ts}"
    wrapper_log_path = log_dir / f"{wrapper_base}.log"
    wrapper_manifest_path = log_dir / f"{wrapper_base}.json"

    discovered = _discover_configs(config_dir, args.config_prefix)
    selected = _apply_start_from(discovered, args.start_from)
    planned = _apply_stop_after(selected, args.stop_after)
    if not selected:
        raise FileNotFoundError(
            f"No config files found in {config_dir} with prefix '{args.config_prefix}'"
            + (f" after applying start-from '{args.start_from}'." if args.start_from else ".")
        )

    manifest = {
        "config_dir": str(config_dir),
        "config_prefix": args.config_prefix,
        "timestamp": ts,
        "python_executable": args.python_executable,
        "dry_run": bool(args.dry_run),
        "start_from": args.start_from,
        "stop_after": args.stop_after,
        "discovered_config_files": [str(path) for path in discovered],
        "selected_config_files": [str(path) for path in selected],
        "planned_config_files": [str(path) for path in planned],
        "chunk_runs": [],
    }
    _write_manifest(wrapper_manifest_path, manifest)

    _log_message(wrapper_log_path, f"START config_dir={config_dir} config_prefix={args.config_prefix}")
    _log_message(
        wrapper_log_path,
        f"DISCOVERED discovered={len(discovered)} selected={len(selected)} planned={len(planned)}",
    )
    if args.dry_run:
        _log_message(wrapper_log_path, "DRY_RUN enabled")

    executed_count = 0

    config_iterable = planned if args.dry_run else selected

    for config_path in config_iterable:
        chunk_run_id = config_path.stem
        chunk_log_path = log_dir / f"{chunk_run_id}_{ts}.log"
        entry = {
            "status": "planned",
            "config_path": str(config_path),
            "log_path": str(chunk_log_path),
            "exit_code": None,
        }
        manifest["chunk_runs"].append(entry)
        _write_manifest(wrapper_manifest_path, manifest)

        if args.dry_run:
            entry["status"] = "planned"
            _log_message(wrapper_log_path, f"DRY_RUN chunk_run_id={chunk_run_id} config={config_path.name}")
            continue

        completed_run = _find_completed_run(chunk_run_id)
        if completed_run is not None:
            entry["status"] = "skipped"
            entry["completed_run_dir"] = str(completed_run)
            _write_manifest(wrapper_manifest_path, manifest)
            _log_message(
                wrapper_log_path,
                f"SKIP_COMPLETED chunk_run_id={chunk_run_id} completed_run={completed_run.name}",
            )
            continue

        if args.stop_after is not None and executed_count >= args.stop_after:
            _log_message(wrapper_log_path, f"STOP_AFTER_REACHED executed={executed_count}")
            break

        cmd = [
            args.python_executable,
            str(CHUNK_RUNNER),
            "--config",
            str(config_path),
        ]
        _log_message(wrapper_log_path, f"CHUNK_START chunk_run_id={chunk_run_id} cmd={' '.join(cmd)}")

        with chunk_log_path.open("w", encoding="utf-8") as chunk_log:
            chunk_log.write(f"[WRAPPER] CHUNK_START chunk_run_id={chunk_run_id}\n")
            process = subprocess.Popen(
                cmd,
                cwd=str(PROJECT_ROOT),
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,
            )

            assert process.stdout is not None
            for line in process.stdout:
                print(line, end="", flush=True)
                chunk_log.write(line)
                chunk_log.flush()

            exit_code = process.wait()

        entry["exit_code"] = int(exit_code)
        executed_count += 1

        if exit_code != 0:
            entry["status"] = "failed"
            _write_manifest(wrapper_manifest_path, manifest)
            _log_message(
                wrapper_log_path,
                f"CHUNK_FAILED chunk_run_id={chunk_run_id} exit_code={exit_code}",
            )
            raise SystemExit(exit_code)

        entry["status"] = "success"
        _write_manifest(wrapper_manifest_path, manifest)
        _log_message(wrapper_log_path, f"CHUNK_DONE chunk_run_id={chunk_run_id} exit_code=0")

    _write_manifest(wrapper_manifest_path, manifest)
    _log_message(wrapper_log_path, "RUN_DONE")


if __name__ == "__main__":
    main()
