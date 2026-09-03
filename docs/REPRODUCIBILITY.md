# Reproducibility guide

## What can be inspected without restricted inputs

- Prompt construction for the name and coordinate conditions
- OD-pair-level train, validation, and test splitting
- Frozen-model representation extraction and caching logic
- Linear-readout fitting
- Output accuracy and spatial-pattern metrics
- Distance probing and layer-wise representation diagnostics
- Baseline specifications
- Aggregate reported results and selected thesis figures

## Environment

The original AWS run used:

- Python 3.10.12
- NVIDIA A10G GPU with 23,028 MiB memory
- PyTorch 2.5.1 with CUDA 12.1
- pandas 2.3.3
- pyarrow 23.0.1
- scikit-learn 1.7.2
- scipy 1.15.3
- transformers 5.5.0

`requirements.txt` retains broad lower bounds from the original project. Exact GPU package installation depends on the target CUDA environment.

## Setup

```bash
bash tools/setup_env.sh
```

Obtain the source data and model access separately, then prepare the files described in `data/README.md`.

## Core district-scale conditions

```bash
python scripts/40_run_rq12_gu.py --config configs/name_condition.yaml
python scripts/40_run_rq12_gu.py --config configs/coordinate_condition.yaml
python scripts/42_run_rq3_gu.py --config configs/name_condition.yaml
python scripts/42_run_rq3_gu.py --config configs/coordinate_condition.yaml
```

These commands perform model inference and can incur substantial GPU time. They should not be used as lightweight smoke tests.

## Baselines

```bash
python scripts/45_run_rq12_baselines.py --config configs/name_condition.yaml --scale gu
python scripts/45_run_rq12_baselines.py --config configs/name_condition.yaml --scale dong
```

## Neighborhood-scale execution

The neighborhood-scale analysis was divided into OD-pair chunks because of its size. The relevant entry points are:

1. `scripts/35_make_dong_chunks.py`
2. `scripts/36_make_chunk_configs.py`
3. `scripts/44_run_dong_chunks_sequential.py`
4. `scripts/43_merge_rq12_dong_chunks.py`

The repository does not include the generated chunk configurations or the full representation caches.

## Principle-prompt comparison

The two principle configurations reproduce the prompt definitions used for the additional district-scale comparison:

```bash
python scripts/40_run_rq12_gu.py --config configs/name_condition_principles.yaml
python scripts/40_run_rq12_gu.py --config configs/coordinate_condition_principles.yaml
```

Aggregate the four condition-specific metric files with `scripts/61_collect_principle_prompt_gu_results.py`.

## Post-processing

The R scripts under `analysis/` expect a local result directory supplied through `MOBILITY_RESULTS_ROOT`. Row-level results are not distributed publicly.

```bash
MOBILITY_RESULTS_ROOT=/path/to/private/results Rscript analysis/01_summarize_output_results.R
MOBILITY_RESULTS_ROOT=/path/to/private/results Rscript analysis/02_summarize_representation_results.R
```

## Important limitations

- The public repository is not a one-command replication package.
- Full reruns require licensed model access, separately obtained mobility data, and substantial GPU resources.
- Sampling and generation settings should be read from the condition YAML files and run snapshots before interpreting exact numerical replication.
- Spatial-scale comparisons are conditional: scale, sparsity, OD count, and chunk-wise fitting differ together in the original design.
