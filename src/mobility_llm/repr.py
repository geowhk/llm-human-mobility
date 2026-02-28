from __future__ import annotations

from typing import Any

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer


def load_frozen_llama(config: dict[str, Any]):
    """Load Llama tokenizer/model and freeze all model parameters."""
    model_cfg = config.get("model", {})
    model_id = model_cfg.get("model_id")
    device = str(model_cfg.get("device", "cpu"))
    dtype_str = str(model_cfg.get("dtype", "float32")).lower()

    if not model_id:
        raise ValueError("Missing required config key: model.model_id")
    if device.startswith("cuda") and not torch.cuda.is_available():
        raise RuntimeError(
            "Config requested CUDA but torch.cuda.is_available() is False."
        )

    dtype_map = {
        "bfloat16": torch.bfloat16,
        "float16": torch.float16,
        "float32": torch.float32,
    }
    if dtype_str not in dtype_map:
        raise ValueError("model.dtype must be one of bfloat16/float16/float32")

    tokenizer = AutoTokenizer.from_pretrained(model_id)
    tokenizer.padding_side = "left"
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    model = AutoModelForCausalLM.from_pretrained(
        model_id,
        torch_dtype=dtype_map[dtype_str],
    )
    model.to(device)
    model.eval()
    for p in model.parameters():
        p.requires_grad = False

    return tokenizer, model


def encode_prompts(
    tokenizer,
    prompts: list[str],
    device: str,
    max_length: int | None = None,
) -> dict[str, torch.Tensor]:
    """Tokenize prompts and move tensors to target device."""
    enc = tokenizer(
        prompts,
        return_tensors="pt",
        padding=True,
        truncation=True,
        max_length=max_length,
    )
    return {k: v.to(device) for k, v in enc.items()}


def extract_last_layer_repr(
    model,
    tokenized: dict[str, torch.Tensor],
) -> torch.Tensor:
    """Extract last-layer representation at last valid token position."""
    with torch.no_grad():
        out = model(
            **tokenized,
            output_hidden_states=True,
            return_dict=True,
        )

    hidden = out.hidden_states[-1]  # [B, T, H]
    mask = tokenized["attention_mask"]  # [B, T]
    bsz, seq_len = mask.shape

    # Last valid token index robust to left/right padding.
    rev_idx = torch.argmax(torch.flip(mask, dims=[1]), dim=1)
    last_idx = (seq_len - 1 - rev_idx).clamp(min=0)  # [B]

    batch_idx = torch.arange(bsz, device=hidden.device)
    reprs = hidden[batch_idx, last_idx, :]  # [B, H]
    return reprs
