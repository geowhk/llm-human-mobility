# Candidate-release validation

Validation date: 2026-09-03 (Asia/Seoul)

## Passed checks

- Parsed all 28 Python files in `src/` and `scripts/` with Python's abstract-syntax-tree parser.
- Imported the core configuration, prompt, split, readout, baseline, and evaluation modules in the retained project environment.
- Parsed both public R analysis scripts with R 4.x.
- Parsed all four YAML experiment configurations and `CITATION.cff`.
- Checked every relative Markdown link in the README and `docs/` files.
- Confirmed that the principle-prompt delta CSV reproduces thesis Table 4-3 after rounding.
- Scanned the current tree and the retained Git patch history for common AWS, Hugging Face, GitHub, and private-key credential patterns; no credentials were detected.
- Confirmed that no current working-tree file outside `.git/` exceeds 10 MB.
- Ran Git's whitespace-error check with no reported errors.

## Checks intentionally not performed

- Full model inference was not rerun because it requires licensed model access, the excluded OD data, and a CUDA-capable machine.
- The single public summary figure was regenerated with English labels from the checksum-verified local RQ3 exports and visually inspected. Its layer-level source exports remain excluded.
- `CITATION.cff` was syntax-checked as YAML; registry-level validation should be repeated after an institutional thesis URL becomes available.

## Release gate

The release uses the existing `geowhk/llm-human-mobility` repository name and the MIT License. The institutional thesis URL remains pending.
