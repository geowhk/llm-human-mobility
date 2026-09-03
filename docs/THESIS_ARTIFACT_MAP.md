# Thesis-to-artifact map

This map connects the final thesis tables and figures to the public artifacts retained in this curated repository. It is an audit trail, not a claim of complete end-to-end reproducibility.

| Thesis item | Subject | Public artifact | Main generating or summarizing code | Reproduction boundary |
|---|---|---|---|---|
| Table 3-1 | Analytical data structure by spatial scale | Summary in [`data/README.md`](../data/README.md) | Private preprocessing workflow | Raw and processed OD records are excluded. |
| Figure 3-1 | Distribution of hourly weekday-average flows | Not redistributed | Private preprocessing workflow | Described in `data/README.md`; row-level source data are excluded. |
| Table 4-1 | Output-flow error metrics | [`output_metrics.csv`](results/output_metrics.csv) | [`analysis/01_summarize_output_results.R`](../analysis/01_summarize_output_results.R) | Aggregate metrics are public; predictions and split assignments are excluded. |
| Table 4-2 | Spatial-pattern reproduction metrics | [`output_metrics.csv`](results/output_metrics.csv) | [`analysis/01_summarize_output_results.R`](../analysis/01_summarize_output_results.R) | Aggregate metrics are public; predictions and split assignments are excluded. |
| Table 4-3 | Changes after adding spatial-interaction principles | [`principle_prompt_metrics.csv`](results/principle_prompt_metrics.csv), [`principle_prompt_metric_changes.csv`](results/principle_prompt_metric_changes.csv) | [`scripts/61_collect_principle_prompt_gu_results.py`](../scripts/61_collect_principle_prompt_gu_results.py) | Final district-level aggregate files are public; representation caches are excluded. |
| Figure 4-1 | Layer-wise distance-bin probe accuracy | Not redistributed | [`analysis/02_summarize_representation_results.R`](../analysis/02_summarize_representation_results.R) | Summarized in `docs/RESULTS.md`; layer-level source exports are excluded. |
| Figure 4-2 | Origin/destination role-conditioned similarity | Not redistributed | [`analysis/02_summarize_representation_results.R`](../analysis/02_summarize_representation_results.R) | Summarized in `docs/RESULTS.md`; layer-level source exports are excluded. |
| Figure 4-3 | Representation magnitude and node flows | Not redistributed | [`analysis/02_summarize_representation_results.R`](../analysis/02_summarize_representation_results.R) | Summarized in `docs/RESULTS.md`; layer-level source exports are excluded. |
| Figure 4-4 | Representation-space/geographic-space alignment | [`figure_4_4_geographic_alignment.png`](figures/figure_4_4_geographic_alignment.png) | [`analysis/02_summarize_representation_results.R`](../analysis/02_summarize_representation_results.R) | Final figure is preserved; layer-level source exports are excluded. |

## Curatorial decisions

- The original exploratory preprocessing script was not promoted into the public workflow because it mixed data preparation, notebook-style diagnostics, and machine-specific development steps. Publishing it as a clean pipeline would imply a level of reproducibility that the current archive does not provide.
- The two public R scripts were aligned with the final thesis figure numbering, changed to accept a private result root through `MOBILITY_RESULTS_ROOT`, and use English labels for the representation-analysis outputs.
- The principle-prompt change table was regenerated from the checksum-verified final run metrics. This corrected a stale local aggregate file and reproduces Table 4-3 after rounding.
