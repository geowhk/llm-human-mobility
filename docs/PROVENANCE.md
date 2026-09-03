# Provenance and curation record

## Source states

- Original public repository: `geowhk/llm-human-mobility`
- Git history used during the thesis: 15 commits from 2026-02-14 through 2026-05-27
- Curated source commit: `2286645`
- Local and AWS Git worktrees were both aligned with that commit during the 2026-09-03 audit.

## Result verification

Checksum comparisons confirmed that the locally retained final district, neighborhood, and baseline outputs matched the corresponding AWS outputs; only file timestamps differed. Two final principle-prompt runs that remained only on AWS were separately preserved before the instance was stopped. Re-aggregating their metrics with the base conditions reproduced the changes reported in thesis Table 4-3.

## Exclusions

The curated repository excludes:

- approximately 269 GB of reusable LLM representation caches;
- raw and processed mobility records;
- row-level predictions and split assignments;
- development, failed-run, and server logs;
- debug files and machine-specific environment files.

The selected CSV files in `docs/results/` contain aggregate evaluation metrics only.

## Interpretation boundary

The public materials document the analysis logic and reported aggregate evidence. They should not be described as independently executable without separately obtained data, model access, and a compatible GPU environment.
