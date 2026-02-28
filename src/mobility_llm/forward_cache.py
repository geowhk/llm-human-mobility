from __future__ import annotations

import hashlib
import json
import shutil
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Optional

import numpy as np
import pandas as pd
import torch

from mobility_llm.repr import load_frozen_llama


_REQUIRED_CACHE_FILES = [
    "forward_row_index.parquet",
    "forward_lasttoken_layer31.npy",
    "forward_row_index_gu.parquet",
    "forward_lasttoken_layerwise_gu.npz",
    "forward_role_out_layerwise_gu.npz",
    "forward_role_in_layerwise_gu.npz",
    "forward_cache_meta.json",
]
_LAYER_KEYS = [f"layer_{i}" for i in range(32)]


def compute_cache_key(input_path: str, config: dict) -> str:
    """Compute deterministic cache key from input file/model/prompt metadata."""
    raw_paths = [p.strip() for p in str(input_path).split(";") if p.strip()]
    if not raw_paths:
        raise ValueError("input_path must contain at least one valid path")

    abs_paths = sorted(str(Path(p).resolve()) for p in raw_paths)

    model_id = str(config.get("model", {}).get("model_id", "")).strip()
    if not model_id:
        raise ValueError("Missing required config key: model.model_id")

    template = str(config.get("prompt", {}).get("template", ""))
    template_hash = hashlib.sha1(template.encode("utf-8")).hexdigest()

    payload_items: list[str] = []
    for i, p in enumerate(abs_paths):
        path_obj = Path(p)
        if not path_obj.exists():
            raise FileNotFoundError(f"Input parquet not found: {path_obj}")
        stat = path_obj.stat()
        payload_items.extend(
            [
                f"input_path_{i}={path_obj}",
                f"file_size_{i}={stat.st_size}",
                f"file_mtime_{i}={stat.st_mtime}",
            ]
        )
    payload_items.extend(
        [
            f"model_id={model_id}",
            f"prompt_template_hash={template_hash}",
        ]
    )
    payload = "\n".join(payload_items)
    return hashlib.sha1(payload.encode("utf-8")).hexdigest()


def get_cache_dir(project_root: Path, cache_key: str) -> Path:
    """Return cache directory path: <project_root>/results/cache/<cache_key>."""
    return (project_root / "results" / "cache" / cache_key).resolve()


def cache_exists(cache_dir: Path) -> bool:
    """Return True only when all required cache files exist."""
    return all((cache_dir / name).exists() for name in _REQUIRED_CACHE_FILES)


def _find_char_span(text: str, needle: str, start: int = 0, end: Optional[int] = None) -> Optional[tuple[int, int]]:
    if not needle:
        return None
    if end is None:
        idx = text.find(needle, start)
    else:
        idx = text.find(needle, start, end)
    if idx < 0:
        return None
    return idx, idx + len(needle)


def _span_token_indices(
    offsets: np.ndarray,
    valid_mask: np.ndarray,
    char_span: Optional[tuple[int, int]],
) -> list[int]:
    if char_span is None:
        return []
    s_char, e_char = char_span
    idxs: list[int] = []
    for i in range(offsets.shape[0]):
        if not bool(valid_mask[i]):
            continue
        s_tok = int(offsets[i, 0])
        e_tok = int(offsets[i, 1])
        if e_tok <= s_tok:
            continue
        if not (e_tok <= s_char or s_tok >= e_char):
            idxs.append(i)
    return idxs


def _required_df_columns() -> list[str]:
    return [
        "query_id",
        "origin_id",
        "dest_id",
        "hour",
        "dist_km",
        "prompt_text",
        "origin_text",
        "dest_text",
    ]


def _save_npz(path: Path, arrays_by_layer: list[np.ndarray]) -> None:
    payload = {f"layer_{i}": arrays_by_layer[i] for i in range(len(arrays_by_layer))}
    np.savez(path, **payload)


def _normalize_input_path(input_path: str) -> str:
    paths = [p.strip() for p in str(input_path).split(";") if p.strip()]
    abs_paths = [str(Path(p).resolve()) for p in paths]
    return ";".join(abs_paths)


def run_forward_and_cache(
    config: dict,
    df: pd.DataFrame,
    cache_dir: Path,
    input_path: str,
) -> dict:
    """Run frozen-LLM forward on row-level prompts and write cache artifacts."""
    missing = [c for c in _required_df_columns() if c not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns for forward cache: {missing}")

    batch_size = int(config.get("batching", {}).get("batch_size", 8))
    if batch_size <= 0:
        raise ValueError("batching.batch_size must be a positive integer")
    max_context_tokens = config.get("model", {}).get("max_context_tokens")
    model_id = str(config.get("model", {}).get("model_id", "")).strip()
    prompt_template = str(config.get("prompt", {}).get("template", ""))
    prompt_template_hash = hashlib.sha1(prompt_template.encode("utf-8")).hexdigest()
    input_path_abs = _normalize_input_path(input_path)

    df_work = df.reset_index(drop=True).copy()
    prompts = df_work["prompt_text"].astype(str).tolist()
    origin_texts = df_work["origin_text"].astype(str).tolist()
    dest_texts = df_work["dest_text"].astype(str).tolist()

    tokenizer, model = load_frozen_llama(config)
    device = str(config.get("model", {}).get("device", "cpu"))

    layer31_all_chunks: list[np.ndarray] = []
    layer_lasttoken_gu_chunks: list[list[np.ndarray]] = [[] for _ in range(32)]
    layer_role_out_gu_chunks: list[list[np.ndarray]] = [[] for _ in range(32)]
    layer_role_in_gu_chunks: list[list[np.ndarray]] = [[] for _ in range(32)]

    fallback_span_count = 0
    hidden_dim: Optional[int] = None

    for start in range(0, len(prompts), batch_size):
        end = min(start + batch_size, len(prompts))
        prompt_batch = prompts[start:end]
        orig_batch = origin_texts[start:end]
        dest_batch = dest_texts[start:end]
        orig_id_batch = df_work.iloc[start:end]["origin_id"].astype(str).to_numpy()
        dest_id_batch = df_work.iloc[start:end]["dest_id"].astype(str).to_numpy()
        gu_mask_batch = np.array(
            [(len(o) == 5 and len(d) == 5) for o, d in zip(orig_id_batch, dest_id_batch)],
            dtype=bool,
        )

        enc = tokenizer(
            prompt_batch,
            return_tensors="pt",
            return_offsets_mapping=True,
            add_special_tokens=True,
            padding=True,
            truncation=True,
            max_length=max_context_tokens,
        )
        if "offset_mapping" not in enc:
            raise RuntimeError(
                "Tokenizer did not return offset_mapping; a fast tokenizer is required."
            )

        offsets = enc.pop("offset_mapping")
        tokenized = {k: v.to(device) for k, v in enc.items()}

        with torch.no_grad():
            out = model(
                **tokenized,
                output_hidden_states=True,
                return_dict=True,
            )

        layer_states = list(out.hidden_states[-32:])
        if len(layer_states) != 32:
            raise RuntimeError(
                f"Expected 32 layer states, got {len(layer_states)} from model output."
            )

        attn_mask = tokenized["attention_mask"]  # [B, T]
        bsz, seq_len = attn_mask.shape
        mask_np = attn_mask.detach().cpu().numpy().astype(bool)
        offsets_np = offsets.detach().cpu().numpy()

        rev_idx = torch.argmax(torch.flip(attn_mask, dims=[1]), dim=1)
        last_idx = (seq_len - 1 - rev_idx).clamp(min=0).detach().cpu().numpy()
        batch_indices = np.arange(bsz)

        origin_tok_idxs: list[list[int]] = []
        dest_tok_idxs: list[list[int]] = []
        for i in range(bsz):
            text = prompt_batch[i]
            o_text = orig_batch[i]
            d_text = dest_batch[i]

            origin_label = "Origin:"
            destination_label = "Destination:"
            origin_label_pos = text.find(origin_label)
            destination_label_pos = text.find(destination_label)

            if origin_label_pos >= 0:
                o_search_start = origin_label_pos + len(origin_label)
            else:
                o_search_start = 0
            if destination_label_pos >= 0:
                o_search_end: Optional[int] = destination_label_pos
                d_search_start = destination_label_pos + len(destination_label)
            else:
                o_search_end = None
                d_search_start = 0

            o_span = _find_char_span(text, o_text, o_search_start, o_search_end)
            if o_span is None:
                o_span = _find_char_span(text, o_text, 0)

            d_span = _find_char_span(text, d_text, d_search_start, None)
            if d_span is None:
                d_span = _find_char_span(text, d_text, 0)

            origin_tok_idxs.append(_span_token_indices(offsets_np[i], mask_np[i], o_span))
            dest_tok_idxs.append(_span_token_indices(offsets_np[i], mask_np[i], d_span))

        for layer_idx, layer_tensor in enumerate(layer_states):
            layer_np = layer_tensor.detach().to(torch.float32).cpu().numpy()  # [B, T, H]
            if hidden_dim is None:
                hidden_dim = int(layer_np.shape[-1])

            last_vec = layer_np[batch_indices, last_idx, :]  # [B, H]
            if layer_idx == 31:
                layer31_all_chunks.append(last_vec.astype(np.float32, copy=False))

            out_role = np.empty_like(last_vec, dtype=np.float32)
            in_role = np.empty_like(last_vec, dtype=np.float32)
            for i in range(bsz):
                o_idxs = origin_tok_idxs[i]
                d_idxs = dest_tok_idxs[i]

                if len(o_idxs) == 0:
                    out_role[i] = last_vec[i]
                    fallback_span_count += 1
                else:
                    out_role[i] = layer_np[i, o_idxs, :].mean(axis=0, dtype=np.float32)

                if len(d_idxs) == 0:
                    in_role[i] = last_vec[i]
                    fallback_span_count += 1
                else:
                    in_role[i] = layer_np[i, d_idxs, :].mean(axis=0, dtype=np.float32)

            layer_lasttoken_gu_chunks[layer_idx].append(last_vec[gu_mask_batch].astype(np.float32, copy=False))
            layer_role_out_gu_chunks[layer_idx].append(out_role[gu_mask_batch].astype(np.float32, copy=False))
            layer_role_in_gu_chunks[layer_idx].append(in_role[gu_mask_batch].astype(np.float32, copy=False))

    if hidden_dim is None:
        hidden_dim = 0

    if len(layer31_all_chunks) == 0:
        lasttoken_layer31 = np.zeros((0, int(hidden_dim)), dtype=np.float32)
    else:
        lasttoken_layer31 = np.concatenate(layer31_all_chunks, axis=0).astype(np.float32, copy=False)

    def _concat_gu(chunks: list[np.ndarray]) -> np.ndarray:
        non_empty = [c for c in chunks if c.shape[0] > 0]
        if len(non_empty) == 0:
            return np.zeros((0, int(hidden_dim)), dtype=np.float32)
        return np.concatenate(non_empty, axis=0).astype(np.float32, copy=False)

    lasttoken_layerwise_gu = [_concat_gu(layer_lasttoken_gu_chunks[i]) for i in range(32)]
    role_out_layerwise_gu = [_concat_gu(layer_role_out_gu_chunks[i]) for i in range(32)]
    role_in_layerwise_gu = [_concat_gu(layer_role_in_gu_chunks[i]) for i in range(32)]

    row_index_df = pd.DataFrame(
        {
            "query_id": df_work["query_id"].astype(str),
            "orig": df_work["origin_id"].astype(str),
            "dest": df_work["dest_id"].astype(str),
            "hour": df_work["hour"],
            "dist_km": pd.to_numeric(df_work["dist_km"], errors="coerce"),
        }
    )
    gu_mask_all = (row_index_df["orig"].str.len() == 5) & (row_index_df["dest"].str.len() == 5)
    row_index_df_gu = row_index_df.loc[gu_mask_all].reset_index(drop=True)

    meta = {
        "cache_key": cache_dir.name,
        "input_path": input_path_abs,
        "model_id": model_id,
        "prompt_template_hash": prompt_template_hash,
        "n_rows_all": int(len(df_work)),
        "n_rows_gu": int(len(row_index_df_gu)),
        "hidden_dim": int(hidden_dim or 0),
        "batch_size": batch_size,
        "fallback_span_count": int(fallback_span_count),
        "created_utc": datetime.now(timezone.utc).isoformat(),
    }

    cache_dir = cache_dir.resolve()
    tmp_dir = cache_dir.parent / f".tmp_{cache_dir.name}_{uuid.uuid4().hex}"
    tmp_dir.mkdir(parents=True, exist_ok=False)

    try:
        row_index_df.to_parquet(tmp_dir / "forward_row_index.parquet", index=False)
        row_index_df_gu.to_parquet(tmp_dir / "forward_row_index_gu.parquet", index=False)
        np.save(tmp_dir / "forward_lasttoken_layer31.npy", lasttoken_layer31)
        _save_npz(tmp_dir / "forward_lasttoken_layerwise_gu.npz", lasttoken_layerwise_gu)
        _save_npz(tmp_dir / "forward_role_out_layerwise_gu.npz", role_out_layerwise_gu)
        _save_npz(tmp_dir / "forward_role_in_layerwise_gu.npz", role_in_layerwise_gu)
        with (tmp_dir / "forward_cache_meta.json").open("w", encoding="utf-8") as f:
            json.dump(meta, f, ensure_ascii=False, indent=2)

        if cache_dir.exists():
            shutil.rmtree(cache_dir)
        tmp_dir.rename(cache_dir)
    except Exception:
        if tmp_dir.exists():
            shutil.rmtree(tmp_dir, ignore_errors=True)
        raise

    return meta


def ensure_cache(
    project_root: Path,
    config: dict,
    input_path: str,
    df: pd.DataFrame,
) -> dict[str, Any]:
    """Get cache if available; otherwise build it and then load."""
    cache_key = compute_cache_key(input_path, config)
    cache_dir = get_cache_dir(project_root, cache_key)

    if cache_exists(cache_dir):
        return load_cached_arrays(cache_dir)

    run_forward_and_cache(
        config=config,
        df=df,
        cache_dir=cache_dir,
        input_path=input_path,
    )
    return load_cached_arrays(cache_dir)


def load_cached_arrays(cache_dir: Path) -> dict[str, Any]:
    """Load cached forward artifacts for downstream modules."""
    cache_dir = cache_dir.resolve()
    if not cache_exists(cache_dir):
        raise FileNotFoundError(f"Incomplete cache directory: {cache_dir}")

    row_index = pd.read_parquet(cache_dir / "forward_row_index.parquet")
    row_index_gu = pd.read_parquet(cache_dir / "forward_row_index_gu.parquet")
    layer31 = np.load(cache_dir / "forward_lasttoken_layer31.npy", mmap_mode="r")
    with (cache_dir / "forward_cache_meta.json").open("r", encoding="utf-8") as f:
        meta = json.load(f)

    return {
        "row_index": row_index,
        "lasttoken_layer31": layer31,
        "row_index_gu": row_index_gu,
        "row_index_gu_path": str(cache_dir / "forward_row_index_gu.parquet"),
        "lasttoken_layerwise_gu_path": str(cache_dir / "forward_lasttoken_layerwise_gu.npz"),
        "role_out_layerwise_gu_path": str(cache_dir / "forward_role_out_layerwise_gu.npz"),
        "role_in_layerwise_gu_path": str(cache_dir / "forward_role_in_layerwise_gu.npz"),
        "meta": meta,
    }
