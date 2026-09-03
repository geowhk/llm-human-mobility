# LLM Representations and Spatial Interaction

Research code and selected derived results for the master's thesis:

> **Evaluating the Capacity of Large Language Model Representations to Capture Spatial Interaction Structure: A Case Study Using Seoul Living Mobility Data**<br>
> Woohyung Kim, M.A. in Geography Education, Seoul National University, August 2026

## Overview

This study asks whether representations extracted from a frozen large language model can capture and reproduce the spatial interaction structure of urban origin-destination (OD) flows. It uses hourly weekday mobility flows within Seoul and compares two ways of representing locations in prompts:

- **Name condition:** administrative-area names provide place and contextual cues.
- **Coordinate condition:** centroid coordinates provide geometric location cues.

Meta-Llama-3-8B-Instruct was used as a frozen representation generator. The study evaluates the representations at two levels:

1. **Output-level reproduction:** Can a linear readout trained on LLM representations reproduce held-out mobility volumes and spatial interaction patterns?
2. **Internal representation:** Across model layers, can distance, origin-destination role conditioning, flow magnitude, and geographic distance structure be detected?

The LLM-based conditions are compared with an hourly-mean baseline, an OD-marginal-product baseline, and a log-linear gravity baseline.

## Main findings

- The LLM-representation approach did not consistently reproduce held-out flow magnitudes or spatial interaction patterns as well as the structural baselines.
- Neither location-name nor coordinate prompts were uniformly superior across output metrics and spatial scales.
- Coordinate-based prompts produced stronger internal signals for distance-bin detection and alignment between representation-space and geographic distances.
- Detectable geographic information inside the model did not automatically translate into accurate output-level mobility structure.
- Adding general spatial-interaction principles to prompts improved some district-level metrics, but the changes were not uniform and do not identify a causal prompt effect.

These results support using spatial-interaction theory not only as a prediction baseline but also as a diagnostic framework for geographic representations.

## Selected result

The clearest internal-representation result was the contrast in geographic alignment between the two prompt conditions:

![Layer-wise alignment between representation-space and geographic distances](docs/figures/figure_4_4_geographic_alignment.png)

Aggregate result tables and brief summaries of the other analyses are provided in [docs/RESULTS.md](docs/RESULTS.md).

## Repository structure

```text
.
├── analysis/        # R scripts for aggregate tables and thesis figures
├── configs/         # Reconstructed configurations for reported conditions
├── data/README.md   # Expected inputs and data-access notes
├── docs/
│   ├── figures/     # One English-language summary figure
│   ├── results/     # Aggregate, non-row-level result tables
│   ├── PROVENANCE.md
│   ├── REPRODUCIBILITY.md
│   └── RESULTS.md
├── scripts/         # Experiment, baseline, chunking, and export entry points
├── src/mobility_llm # Core prompt, representation, readout, split, and metric code
└── requirements.txt
```

## Data and model access

The raw and processed mobility records are not distributed in this repository. The source data are available from the [Seoul Open Data Plaza](https://data.seoul.go.kr/dataVisual/seoul/seoulLivingMigration.do); users remain responsible for complying with its current terms and documentation. See [data/README.md](data/README.md) for the expected processed schemas.

The model weights are also not distributed. Access to `meta-llama/Meta-Llama-3-8B-Instruct` is governed by Meta's license and the model host's access requirements.

## Reproduction scope

The repository preserves the analysis logic, configurations, aggregate metrics, and selected figures. A full end-to-end rerun requires separately obtained mobility data, compatible administrative boundaries, access to the model weights, and a CUDA-capable environment. The original full experiments ran with Python 3.10.12, PyTorch 2.5.1 with CUDA 12.1, and an NVIDIA A10G GPU.

Start with [docs/REPRODUCIBILITY.md](docs/REPRODUCIBILITY.md). This is a curated research archive rather than a guaranteed one-command replication package.

The relationship between the final thesis tables and figures and the preserved artifacts is documented in [docs/THESIS_ARTIFACT_MAP.md](docs/THESIS_ARTIFACT_MAP.md).

## Citation

If you use this code or its derived materials, cite the thesis using the metadata in [CITATION.cff](CITATION.cff).

The thesis PDF is not distributed in this repository. A persistent institutional-repository link will be added here when it becomes available.

## License

The code and original documentation in this repository are available under the [MIT License](LICENSE). External data, model weights, and third-party materials remain subject to their respective terms.

## Repository status

This candidate release was reconstructed from the thesis workstation, the Git history used during analysis, and checksum-verified final AWS outputs. Raw model caches, row-level predictions, server logs, and restricted data are intentionally excluded.
