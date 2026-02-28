from __future__ import annotations

import traceback
from typing import Any

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer


def load_llama(config: dict[str, Any]):
    """Load tokenizer and Llama model from config."""
    try:
        model_cfg = config.get("model", {})
        model_id = model_cfg.get("model_id")
        device = str(model_cfg.get("device", "cpu"))
        dtype_str = str(model_cfg.get("dtype", "float32")).lower()

        if not model_id:
            raise ValueError("Missing required config key: model.model_id")
        if not device:
            raise ValueError("Missing required config key: model.device")
        if not dtype_str:
            raise ValueError("Missing required config key: model.dtype")

        dtype_map = {
            "bfloat16": torch.bfloat16,
            "float16": torch.float16,
            "float32": torch.float32,
        }
        if dtype_str not in dtype_map:
            raise ValueError(
                "Unsupported model.dtype. Use one of: bfloat16, float16, float32."
            )
        torch_dtype = dtype_map[dtype_str]

        if device.startswith("cuda") and not torch.cuda.is_available():
            raise RuntimeError(
                "Config requested model.device='cuda' but CUDA is not available. "
                "Set model.device to 'cpu' or run on a CUDA-enabled environment."
            )

        tokenizer = AutoTokenizer.from_pretrained(model_id)
        if tokenizer.pad_token is None:
            tokenizer.pad_token = tokenizer.eos_token

        model = AutoModelForCausalLM.from_pretrained(
            model_id,
            torch_dtype=torch_dtype,
        )
        model.to(device)
        model.eval()
        return tokenizer, model
    except Exception as e:
        traceback.print_exc()
        print("EXCEPTION in load_llama:", e)
        raise


def generate_one(tokenizer, model, prompt: str, gen_cfg: dict[str, Any]) -> str:
    """Generate a single text output from one prompt."""
    device = next(model.parameters()).device
    max_ctx = gen_cfg.get("max_context_tokens")
    model_inputs = tokenizer(
        prompt,
        return_tensors="pt",
        truncation=True,
        max_length=max_ctx if isinstance(max_ctx, int) and max_ctx > 0 else None,
    )
    model_inputs = {k: v.to(device) for k, v in model_inputs.items()}
    input_len = int(model_inputs["input_ids"].shape[1])

    with torch.no_grad():
        outputs = model.generate(
            **model_inputs,
            do_sample=bool(gen_cfg.get("do_sample", True)),
            temperature=float(gen_cfg.get("temperature", 0.7)),
            top_p=float(gen_cfg.get("top_p", 0.9)),
            max_new_tokens=int(gen_cfg.get("max_new_tokens", 16)),
            pad_token_id=tokenizer.pad_token_id,
        )

    gen_tokens = outputs[0, input_len:]
    return tokenizer.decode(gen_tokens, skip_special_tokens=True).strip()


def generate_repeats(
    tokenizer,
    model,
    prompts: list[str],
    config: dict[str, Any],
    gen_override: dict[str, Any] | None = None,
) -> list[dict[str, Any]]:
    """Run repeated generations for prompts and return raw outputs."""
    generation_cfg = config.get("generation", {})
    batching_cfg = config.get("batching", {})
    effective_cfg = dict(generation_cfg)
    if gen_override:
        effective_cfg.update(gen_override)

    n_repeat = int(generation_cfg.get("n_repeat", 1))
    batch_size = int(batching_cfg.get("batch_size", 1))
    max_ctx = config.get("model", {}).get("max_context_tokens")
    meta = config.get("_prompt_meta")

    if meta is None or not isinstance(meta, list) or len(meta) != len(prompts):
        raise ValueError(
            "generate_repeats requires config['_prompt_meta'] with "
            "query_id/scale metadata aligned to prompts."
        )
    if batch_size <= 0:
        raise ValueError("batching.batch_size must be >= 1")
    if n_repeat <= 0:
        raise ValueError("generation.n_repeat must be >= 1")

    device = next(model.parameters()).device
    results: list[dict[str, Any]] = []

    for repeat_id in range(1, n_repeat + 1):
        for start in range(0, len(prompts), batch_size):
            end = min(start + batch_size, len(prompts))
            batch_prompts = prompts[start:end]
            batch_meta = meta[start:end]

            inputs = tokenizer(
                batch_prompts,
                return_tensors="pt",
                padding=True,
                truncation=True,
                max_length=max_ctx if isinstance(max_ctx, int) and max_ctx > 0 else None,
            )
            attention_mask = inputs["attention_mask"]
            input_lens = attention_mask.sum(dim=1).tolist()
            inputs = {k: v.to(device) for k, v in inputs.items()}

            try:
                with torch.no_grad():
                    outputs = model.generate(
                        **inputs,
                        do_sample=bool(effective_cfg.get("do_sample", True)),
                        temperature=float(effective_cfg.get("temperature", 0.7)),
                        top_p=float(effective_cfg.get("top_p", 0.9)),
                        max_new_tokens=int(effective_cfg.get("max_new_tokens", 16)),
                        pad_token_id=tokenizer.pad_token_id,
                    )
            except RuntimeError as exc:
                traceback.print_exc()
                print("EXCEPTION in generate_repeats/model.generate:", exc)
                if "out of memory" in str(exc).lower():
                    raise RuntimeError(
                        "Model generation ran out of memory. "
                        "Try reducing batching.batch_size in config."
                    ) from exc
                raise
            except Exception as exc:
                traceback.print_exc()
                print("EXCEPTION in generate_repeats/model.generate:", exc)
                raise

            for idx, out_tokens in enumerate(outputs):
                gen_tokens = out_tokens[int(input_lens[idx]) :]
                raw_output = tokenizer.decode(
                    gen_tokens,
                    skip_special_tokens=True,
                ).strip()
                row_meta = batch_meta[idx]
                # For retry calls (n_repeat == 1), preserve per-row repeat_id from meta.
                if n_repeat == 1 and "repeat_id" in row_meta:
                    out_repeat_id = int(row_meta["repeat_id"])
                else:
                    out_repeat_id = repeat_id
                results.append(
                    {
                        "query_id": row_meta["query_id"],
                        "scale": row_meta["scale"],
                        "repeat_id": out_repeat_id,
                        "raw_output": raw_output,
                        "prompt_text": batch_prompts[idx],
                    }
                )

    return results
